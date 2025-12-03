// Intermediate code generator: reads parser AST dumps and produces LLVM IR
// using the lightweight IR library under compiler_ir-master.

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>
#include <functional>

#include "../../compiler_ir-master/include/Constant.h"
#include "../../compiler_ir-master/include/Function.h"
#include "../../compiler_ir-master/include/GlobalVariable.h"
#include "../../compiler_ir-master/include/IRbuilder.h"
#include "../../compiler_ir-master/include/Module.h"
#include "../../compiler_ir-master/include/Type.h"
#include "../../compiler_ir-master/include/Value.h"

namespace fs = std::filesystem;

struct AstNode {
	std::string symbol;
	std::string lexeme;
	std::vector<std::unique_ptr<AstNode>> children;
};

struct SemanticError : std::runtime_error {
	using std::runtime_error::runtime_error;
};

const AstNode *childAt(const AstNode &node, size_t index) {
	if (index >= node.children.size()) {
		throw SemanticError("Unexpected AST shape for symbol " + node.symbol);
	}
	return node.children[index].get();
}

const AstNode *findChild(const AstNode &node, const std::string &symbol) {
	for (const auto &child : node.children) {
		if (child->symbol == symbol) {
			return child.get();
		}
	}
	return nullptr;
}

const AstNode *expectChild(const AstNode &node, const std::string &symbol) {
	if (const AstNode *child = findChild(node, symbol)) {
		return child;
	}
	throw SemanticError("Missing child " + symbol + " under " + node.symbol);
}

std::string nodeLexeme(const AstNode &node) {
	return node.lexeme;
}

bool isSymbol(const AstNode &node, const std::string &symbol) {
	return node.symbol == symbol;
}

struct ConstValue {
	enum class Kind { Int, Float, Bool };
	Kind kind = Kind::Int;
	int intValue = 0;
	double floatValue = 0.0;
	bool boolValue = false;

	static ConstValue fromInt(int v) {
		ConstValue c;
		c.kind = Kind::Int;
		c.intValue = v;
		return c;
	}

	static ConstValue fromFloat(double v) {
		ConstValue c;
		c.kind = Kind::Float;
		c.floatValue = v;
		return c;
	}

	static ConstValue fromBool(bool v) {
		ConstValue c;
		c.kind = Kind::Bool;
		c.boolValue = v;
		return c;
	}
};

void collectOperands(const AstNode &node, const std::string &symbol,
			 std::vector<const AstNode *> &out) {
	if (node.children.empty()) {
		return;
	}
	if (node.children.size() == 1) {
		out.push_back(childAt(node, 0));
		return;
	}
	if (node.children.size() == 3 && node.children[0]->symbol == symbol) {
		collectOperands(*node.children[0], symbol, out);
		out.push_back(node.children[2].get());
		return;
	}
	out.push_back(node.children.back().get());
}

std::string trim(std::string_view text) {
	const size_t start = text.find_first_not_of(' ');
	if (start == std::string_view::npos) {
		return "";
	}
	const size_t end = text.find_last_not_of(' ');
	return std::string{text.substr(start, end - start + 1)};
}

std::unique_ptr<AstNode> parseAstFile(const fs::path &path) {
	std::ifstream input(path);
	if (!input) {
		throw std::runtime_error("Failed to open AST file: " + path.string());
	}
	std::string line;
	std::vector<AstNode *> stack;
	std::unique_ptr<AstNode> root;
	while (std::getline(input, line)) {
		if (line.empty()) {
			continue;
		}
		size_t spaces = 0;
		while (spaces < line.size() && line[spaces] == ' ') {
			++spaces;
		}
		const size_t depth = spaces / 2;
		std::string content = trim(std::string_view(line).substr(spaces));
		if (content.empty()) {
			continue;
		}
		std::string symbol = content;
		std::string lexeme;
		size_t separator = content.rfind(" (");
		if (separator != std::string::npos && content.back() == ')') {
			symbol = content.substr(0, separator);
			lexeme = content.substr(separator + 2, content.size() - separator - 3);
		}
		auto node = std::make_unique<AstNode>();
		node->symbol = symbol;
		node->lexeme = lexeme;
		if (!root) {
			root = std::move(node);
			stack.clear();
			stack.push_back(root.get());
			continue;
		}
		while (stack.size() > depth) {
			stack.pop_back();
		}
		if (stack.empty()) {
			throw std::runtime_error("Invalid AST indentation in " + path.string());
		}
		AstNode *parent = stack.back();
		parent->children.push_back(std::move(node));
		stack.push_back(parent->children.back().get());
	}
	return root;
}

void declareRuntime(Module &module) {
	Type *voidTy = module.get_void_type();
	Type *i32 = module.get_int32_type();
	PointerType *i32Ptr = module.get_int32_ptr_type();
	auto declare = [&](Type *ret, const std::string &name,
				std::vector<Type *> params) {
		FunctionType *fnType = FunctionType::get(ret, std::move(params));
		Function::create(fnType, name, &module);
	};
	declare(i32, "getint", {});
	declare(i32, "getch", {});
	declare(i32, "getarray", {i32Ptr});
	declare(voidTy, "putint", {i32});
	declare(voidTy, "putch", {i32});
	declare(voidTy, "putarray", {i32, i32Ptr});
	declare(voidTy, "starttime", {});
	declare(voidTy, "stoptime", {});
}

enum class ScalarType { Int, Float };

struct SymbolInfo {
	ScalarType type = ScalarType::Int;
	bool isConst = false;
	bool isGlobal = false;
	bool isCompileTimeConst = false;
	Value *storage = nullptr; // alloca or global variable pointer
	std::optional<int> intValue;
	std::optional<double> floatValue;
};

class CodeGenerator {
public:
	explicit CodeGenerator(Module &module)
		: module_(module), builder_(nullptr, &module) {}

	void generate(const AstNode &root);
	void setSourceName(const std::string &name) { sourceName_ = name; }

private:
	Module &module_;
	IRBuilder builder_;
	Function *currentFunction_ = nullptr;
	BasicBlock *entryBlock_ = nullptr;
	enum class ReturnType { Int, Void };
	ReturnType currentReturnType_ = ReturnType::Void;
	std::string sourceName_;
	int labelCounter_ = 0;
	std::vector<std::unordered_map<std::string, SymbolInfo>> scopes_;

	// Scope helpers
	void pushScope();
	void popScope();
	SymbolInfo *lookup(const std::string &name);
	void define(const std::string &name, SymbolInfo symbol);

	// Module level
	void emitCompUnit(const AstNode &node);
	void emitUnit(const AstNode &node);
	void emitDecl(const AstNode &node, bool isGlobal);
	void emitConstDecl(const AstNode &node, bool isGlobal);
	void emitVarDecl(const AstNode &node, bool isGlobal);
	void visitConstDefList(const AstNode &node, ScalarType type, bool isGlobal);
	void visitVarDefList(const AstNode &node, ScalarType type, bool isGlobal);
	void handleConstDef(const AstNode &node, ScalarType type, bool isGlobal);
	void handleVarDef(const AstNode &node, ScalarType type, bool isGlobal);
	void emitFuncDef(const AstNode &node);

	// Block / statements
	void emitBlock(const AstNode &node, bool createScope = true);
	void emitBlockItemList(const AstNode &node);
	void emitBlockItem(const AstNode &node);
	void emitStmt(const AstNode &node);
	void emitMatchedStmt(const AstNode &node);
	void emitUnmatchedStmt(const AstNode &node);
	void emitIfLike(const AstNode &condNode, const AstNode &thenNode,
			   const AstNode *elseNode);
	void emitReturn(const AstNode *expOpt);
	void emitAssign(const AstNode &lvalNode, const AstNode &expNode);

	// Expressions
	Value *emitExp(const AstNode &node);
	Value *emitCond(const AstNode &node);
	Value *emitLOrExp(const AstNode &node);
	Value *emitLAndExp(const AstNode &node);
	Value *emitEqExp(const AstNode &node);
	Value *emitRelExp(const AstNode &node);
	Value *emitAddExp(const AstNode &node);
	Value *emitMulExp(const AstNode &node);
	Value *emitUnaryExp(const AstNode &node);
	Value *emitPrimaryExp(const AstNode &node);
	Value *emitLVal(const AstNode &node);
	Value *ensureBool(Value *value);
	Value *ensureInt(Value *value);
	Value *emitShortCircuit(const std::vector<const AstNode *> &operands,
				 bool isOr,
				 const std::function<Value *(const AstNode &)> &emitOperand);

	// Constant evaluation
	std::optional<ConstValue> tryEval(const AstNode &node);
	std::optional<ConstValue> evalLOr(const AstNode &node);
	std::optional<ConstValue> evalLAnd(const AstNode &node);
	std::optional<ConstValue> evalEq(const AstNode &node);
	std::optional<ConstValue> evalRel(const AstNode &node);
	std::optional<ConstValue> evalAdd(const AstNode &node);
	std::optional<ConstValue> evalMul(const AstNode &node);
	std::optional<ConstValue> evalUnary(const AstNode &node);
	std::optional<ConstValue> evalPrimary(const AstNode &node);

	Value *createAlloca(Type *type);
	Value *emitStore(Value *value, Value *addr);
	ConstValue getConstOrThrow(const AstNode &node);
	int asInt(const ConstValue &cv) const;
	double asFloat(const ConstValue &cv) const;
	bool asBool(const ConstValue &cv) const;
	Type *typeFor(ScalarType t);
	ScalarType parseBType(const AstNode &node);
	Value *constantValue(const ConstValue &cv);
	std::string nextLabel(const std::string &hint);
	bool currentBlockTerminated();
};

void CodeGenerator::pushScope() { scopes_.emplace_back(); }

void CodeGenerator::popScope() {
	if (scopes_.empty()) {
		throw SemanticError("Scope underflow");
	}
	scopes_.pop_back();
}

SymbolInfo *CodeGenerator::lookup(const std::string &name) {
	for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
		auto pos = it->find(name);
		if (pos != it->end()) {
			return &pos->second;
		}
	}
	return nullptr;
}

void CodeGenerator::define(const std::string &name, SymbolInfo symbol) {
	if (scopes_.empty()) {
		pushScope();
	}
	auto &scope = scopes_.back();
	if (scope.count(name)) {
		throw SemanticError("Duplicate symbol: " + name);
	}
	scope.emplace(name, std::move(symbol));
}

Type *CodeGenerator::typeFor(ScalarType t) {
	return (t == ScalarType::Int) ? static_cast<Type *>(module_.get_int32_type())
									 : static_cast<Type *>(module_.get_float_type());
}

Value *CodeGenerator::createAlloca(Type *type) {
	BasicBlock *saved = builder_.get_insert_block();
	if (!entryBlock_) {
		throw SemanticError("Missing entry block for allocation");
	}
	builder_.set_insert_point(entryBlock_);
	Value *allocaInst = builder_.create_alloca(type);
	if (saved) {
		builder_.set_insert_point(saved);
	}
	return allocaInst;
}

Value *CodeGenerator::emitStore(Value *value, Value *addr) {
	return builder_.create_store(value, addr);
}

int CodeGenerator::asInt(const ConstValue &cv) const {
	switch (cv.kind) {
	case ConstValue::Kind::Int:
		return cv.intValue;
	case ConstValue::Kind::Bool:
		return cv.boolValue ? 1 : 0;
	case ConstValue::Kind::Float:
		return static_cast<int>(cv.floatValue);
	}
	return 0;
}

double CodeGenerator::asFloat(const ConstValue &cv) const {
	switch (cv.kind) {
	case ConstValue::Kind::Float:
		return cv.floatValue;
	case ConstValue::Kind::Int:
		return static_cast<double>(cv.intValue);
	case ConstValue::Kind::Bool:
		return cv.boolValue ? 1.0 : 0.0;
	}
	return 0.0;
}

bool CodeGenerator::asBool(const ConstValue &cv) const {
	switch (cv.kind) {
	case ConstValue::Kind::Bool:
		return cv.boolValue;
	case ConstValue::Kind::Int:
		return cv.intValue != 0;
	case ConstValue::Kind::Float:
		return cv.floatValue != 0.0;
	}
	return false;
}

Value *CodeGenerator::constantValue(const ConstValue &cv) {
	switch (cv.kind) {
	case ConstValue::Kind::Int:
		return ConstantInt::get(cv.intValue, &module_);
	case ConstValue::Kind::Bool:
		return ConstantInt::get(cv.boolValue, &module_);
	case ConstValue::Kind::Float:
		// float constants not supported in current IR; approximate via int
		return ConstantInt::get(static_cast<int>(cv.floatValue), &module_);
	}
	return ConstantInt::get(0, &module_);
}

std::string CodeGenerator::nextLabel(const std::string &hint) {
	return hint + "." + std::to_string(labelCounter_++);
}

bool CodeGenerator::currentBlockTerminated() {
	BasicBlock *bb = builder_.get_insert_block();
	return bb == nullptr || bb->get_terminator() != nullptr;
}

Value *CodeGenerator::ensureInt(Value *value) {
	if (value->get_type()->is_int32_type()) {
		return value;
	}
	if (value->get_type()->is_int1_type()) {
		return builder_.create_zext(value, module_.get_int32_type());
	}
	throw SemanticError("Expected integer type");
}

Value *CodeGenerator::ensureBool(Value *value) {
	if (value->get_type()->is_int1_type()) {
		return value;
	}
	if (value->get_type()->is_int32_type()) {
		return builder_.create_icmp_ne(value, ConstantInt::get(0, &module_));
	}
	throw SemanticError("Expected integer for boolean conversion");
}

Value *CodeGenerator::emitExp(const AstNode &node) {
	const AstNode *lor = expectChild(node, "lOrExp");
	Value *value = emitLOrExp(*lor);
	return ensureInt(value);
}

Value *CodeGenerator::emitCond(const AstNode &node) {
	const AstNode *lor = expectChild(node, "lOrExp");
	Value *value = emitLOrExp(*lor);
	return ensureBool(value);
}

Value *CodeGenerator::emitLOrExp(const AstNode &node) {
	std::vector<const AstNode *> operands;
	collectOperands(node, "lOrExp", operands);
	if (operands.size() == 1) {
		return emitLAndExp(*operands[0]);
	}
	auto emitOperand = [&](const AstNode &term) { return emitLAndExp(term); };
	Value *boolVal = emitShortCircuit(operands, /*isOr=*/true, emitOperand);
	return builder_.create_zext(boolVal, module_.get_int32_type());
}

Value *CodeGenerator::emitLAndExp(const AstNode &node) {
	std::vector<const AstNode *> operands;
	collectOperands(node, "lAndExp", operands);
	if (operands.size() == 1) {
		return emitEqExp(*operands[0]);
	}
	auto emitOperand = [&](const AstNode &term) { return emitEqExp(term); };
	Value *boolVal = emitShortCircuit(operands, /*isOr=*/false, emitOperand);
	return builder_.create_zext(boolVal, module_.get_int32_type());
}

Value *CodeGenerator::emitShortCircuit(const std::vector<const AstNode *> &operands,
				 bool isOr,
				 const std::function<Value *(const AstNode &)> &emitOperand) {
	if (!currentFunction_) {
		throw SemanticError("Logical expression outside function");
	}
	BasicBlock *mergeBB = BasicBlock::create(&module_,
				 isOr ? nextLabel("lor.merge") : nextLabel("land.merge"),
				 currentFunction_);
	Value *trueConst = ConstantInt::get(true, &module_);
	Value *falseConst = ConstantInt::get(false, &module_);
	std::vector<std::pair<Value *, BasicBlock *>> incoming;
	for (size_t i = 0; i < operands.size(); ++i) {
		bool isLast = (i + 1) == operands.size();
		Value *cond = ensureBool(emitOperand(*operands[i]));
		BasicBlock *currentBB = builder_.get_insert_block();
		if (isOr) {
			BasicBlock *falseBB = BasicBlock::create(&module_,
							 isLast ? nextLabel("lor.false")
								 : nextLabel("lor.next"),
						 currentFunction_);
			builder_.create_cond_br(cond, mergeBB, falseBB);
			incoming.emplace_back(trueConst, currentBB);
			builder_.set_insert_point(falseBB);
			if (isLast) {
				builder_.create_br(mergeBB);
				incoming.emplace_back(falseConst, falseBB);
				break;
			}
		} else {
			BasicBlock *trueBB = BasicBlock::create(&module_,
							 isLast ? nextLabel("land.true")
								 : nextLabel("land.next"),
						 currentFunction_);
			builder_.create_cond_br(cond, trueBB, mergeBB);
			incoming.emplace_back(falseConst, currentBB);
			builder_.set_insert_point(trueBB);
			if (isLast) {
				builder_.create_br(mergeBB);
				incoming.emplace_back(trueConst, trueBB);
				break;
			}
		}
	}
	builder_.set_insert_point(mergeBB);
	PhiInst *phi = PhiInst::create_phi(module_.get_int1_type(), mergeBB);
	for (const auto &[value, bb] : incoming) {
		phi->add_phi_pair_operand(value, bb);
	}
	return phi;
}

Value *CodeGenerator::emitEqExp(const AstNode &node) {
	if (node.children.size() == 1) {
		return emitRelExp(*childAt(node, 0));
	}
	Value *lhs = ensureInt(emitEqExp(*childAt(node, 0)));
	Value *rhs = ensureInt(emitRelExp(*childAt(node, 2)));
	const AstNode *opNode = childAt(node, 1);
	std::string op = nodeLexeme(*childAt(*opNode, 0));
	Value *cmp = (op == "==") ? builder_.create_icmp_eq(lhs, rhs)
							 	: builder_.create_icmp_ne(lhs, rhs);
	return builder_.create_zext(cmp, module_.get_int32_type());
}

Value *CodeGenerator::emitRelExp(const AstNode &node) {
	if (node.children.size() == 1) {
		return emitAddExp(*childAt(node, 0));
	}
	Value *lhs = ensureInt(emitRelExp(*childAt(node, 0)));
	Value *rhs = ensureInt(emitAddExp(*childAt(node, 2)));
	const AstNode *opNode = childAt(node, 1);
	std::string op = nodeLexeme(*childAt(*opNode, 0));
	Value *cmp = nullptr;
	if (op == "<") {
		cmp = builder_.create_icmp_lt(lhs, rhs);
	} else if (op == "<=") {
		cmp = builder_.create_icmp_le(lhs, rhs);
	} else if (op == ">") {
		cmp = builder_.create_icmp_gt(lhs, rhs);
	} else {
		cmp = builder_.create_icmp_ge(lhs, rhs);
	}
	return builder_.create_zext(cmp, module_.get_int32_type());
}

Value *CodeGenerator::emitAddExp(const AstNode &node) {
	if (node.children.size() == 1) {
		return emitMulExp(*childAt(node, 0));
	}
	Value *lhs = ensureInt(emitAddExp(*childAt(node, 0)));
	Value *rhs = ensureInt(emitMulExp(*childAt(node, 2)));
	const AstNode *opNode = childAt(node, 1);
	std::string op = nodeLexeme(*childAt(*opNode, 0));
	return (op == "+") ? builder_.create_iadd(lhs, rhs)
						 : builder_.create_isub(lhs, rhs);
}

Value *CodeGenerator::emitMulExp(const AstNode &node) {
	if (node.children.size() == 1) {
		return emitUnaryExp(*childAt(node, 0));
	}
	Value *lhs = ensureInt(emitMulExp(*childAt(node, 0)));
	Value *rhs = ensureInt(emitUnaryExp(*childAt(node, 2)));
	const AstNode *opNode = childAt(node, 1);
	std::string op = nodeLexeme(*childAt(*opNode, 0));
	if (op == "*") {
		return builder_.create_imul(lhs, rhs);
	}
	if (op == "/") {
		return builder_.create_isdiv(lhs, rhs);
	}
	return builder_.create_irem(lhs, rhs);
}

Value *CodeGenerator::emitUnaryExp(const AstNode &node) {
	const AstNode *first = childAt(node, 0);
	if (first->symbol == "primaryExp") {
		return emitPrimaryExp(*first);
	}
	if (first->symbol == "unaryOp") {
		std::string op = nodeLexeme(*childAt(*first, 0));
		Value *inner = emitUnaryExp(*childAt(node, 1));
		if (op == "+") {
			return inner;
		}
		if (op == "-") {
			return builder_.create_isub(ConstantInt::get(0, &module_), ensureInt(inner));
		}
		Value *boolVal = ensureBool(inner);
		Value *neg = builder_.create_icmp_eq(boolVal, ConstantInt::get(false, &module_));
		return builder_.create_zext(neg, module_.get_int32_type());
	}
	throw SemanticError("Unsupported unary expression");
}

Value *CodeGenerator::emitPrimaryExp(const AstNode &node) {
	const AstNode *first = childAt(node, 0);
	if (first->symbol == "(") {
		return emitExp(*childAt(node, 1));
	}
	if (first->symbol == "lVal") {
		return emitLVal(*first);
	}
	if (first->symbol == "number") {
		const AstNode *leaf = childAt(*first, 0);
		if (leaf->symbol == "IntConst") {
			return ConstantInt::get(std::stoi(leaf->lexeme), &module_);
		}
		return ConstantInt::get(static_cast<int>(std::stod(leaf->lexeme)), &module_);
	}
	throw SemanticError("Unsupported primary expression");
}

Value *CodeGenerator::emitLVal(const AstNode &node) {
	const AstNode *identNode = expectChild(node, "Ident");
	std::string name = nodeLexeme(*identNode);
	SymbolInfo *symbol = lookup(name);
	if (!symbol) {
		throw SemanticError("Unknown identifier: " + name);
	}
	if (symbol->isCompileTimeConst && symbol->intValue && symbol->storage == nullptr) {
		return ConstantInt::get(*symbol->intValue, &module_);
	}
	if (!symbol->storage) {
		throw SemanticError("Unsupported lVal for identifier: " + name);
	}
	return builder_.create_load(typeFor(symbol->type), symbol->storage);
}

ConstValue CodeGenerator::getConstOrThrow(const AstNode &node) {
	if (auto value = tryEval(node)) {
		return *value;
	}
	throw SemanticError("Initializer must be constant");
}

std::optional<ConstValue> CodeGenerator::tryEval(const AstNode &node) {
	if (node.symbol == "exp") {
		return evalLOr(*expectChild(node, "lOrExp"));
	}
	if (node.symbol == "constExp") {
		return evalAdd(*expectChild(node, "addExp"));
	}
	if (node.symbol == "lOrExp") {
		return evalLOr(node);
	}
	if (node.symbol == "lAndExp") {
		return evalLAnd(node);
	}
	if (node.symbol == "eqExp") {
		return evalEq(node);
	}
	if (node.symbol == "relExp") {
		return evalRel(node);
	}
	if (node.symbol == "addExp") {
		return evalAdd(node);
	}
	if (node.symbol == "mulExp") {
		return evalMul(node);
	}
	if (node.symbol == "unaryExp") {
		return evalUnary(node);
	}
	if (node.symbol == "primaryExp") {
		return evalPrimary(node);
	}
	return std::nullopt;
}

std::optional<ConstValue> CodeGenerator::evalLOr(const AstNode &node) {
	std::vector<const AstNode *> operands;
	collectOperands(node, "lOrExp", operands);
	bool result = false;
	for (const AstNode *term : operands) {
		auto value = evalLAnd(*term);
		if (!value) {
			return std::nullopt;
		}
		result = result || asBool(*value);
		if (result) {
			break;
		}
	}
	return ConstValue::fromBool(result);
}

std::optional<ConstValue> CodeGenerator::evalLAnd(const AstNode &node) {
	std::vector<const AstNode *> operands;
	collectOperands(node, "lAndExp", operands);
	bool result = true;
	for (const AstNode *term : operands) {
		auto value = evalEq(*term);
		if (!value) {
			return std::nullopt;
		}
		result = result && asBool(*value);
		if (!result) {
			break;
		}
	}
	return ConstValue::fromBool(result);
}

std::optional<ConstValue> CodeGenerator::evalEq(const AstNode &node) {
	if (node.children.size() == 1) {
		return evalRel(*childAt(node, 0));
	}
	auto lhs = evalEq(*childAt(node, 0));
	auto rhs = evalRel(*childAt(node, 2));
	if (!lhs || !rhs) {
		return std::nullopt;
	}
	std::string op = nodeLexeme(*childAt(*childAt(node, 1), 0));
	bool res = (op == "==") ? (asInt(*lhs) == asInt(*rhs))
					 : (asInt(*lhs) != asInt(*rhs));
	return ConstValue::fromBool(res);
}

std::optional<ConstValue> CodeGenerator::evalRel(const AstNode &node) {
	if (node.children.size() == 1) {
		return evalAdd(*childAt(node, 0));
	}
	auto lhs = evalRel(*childAt(node, 0));
	auto rhs = evalAdd(*childAt(node, 2));
	if (!lhs || !rhs) {
		return std::nullopt;
	}
	int l = asInt(*lhs);
	int r = asInt(*rhs);
	std::string op = nodeLexeme(*childAt(*childAt(node, 1), 0));
	bool res = false;
	if (op == "<") {
		res = l < r;
	} else if (op == "<=") {
		res = l <= r;
	} else if (op == ">") {
		res = l > r;
	} else {
		res = l >= r;
	}
	return ConstValue::fromBool(res);
}

std::optional<ConstValue> CodeGenerator::evalAdd(const AstNode &node) {
	if (node.children.size() == 1) {
		return evalMul(*childAt(node, 0));
	}
	auto lhs = evalAdd(*childAt(node, 0));
	auto rhs = evalMul(*childAt(node, 2));
	if (!lhs || !rhs) {
		return std::nullopt;
	}
	int l = asInt(*lhs);
	int r = asInt(*rhs);
	std::string op = nodeLexeme(*childAt(*childAt(node, 1), 0));
	return ConstValue::fromInt((op == "+") ? (l + r) : (l - r));
}

std::optional<ConstValue> CodeGenerator::evalMul(const AstNode &node) {
	if (node.children.size() == 1) {
		return evalUnary(*childAt(node, 0));
	}
	auto lhs = evalMul(*childAt(node, 0));
	auto rhs = evalUnary(*childAt(node, 2));
	if (!lhs || !rhs) {
		return std::nullopt;
	}
	int l = asInt(*lhs);
	int r = asInt(*rhs);
	std::string op = nodeLexeme(*childAt(*childAt(node, 1), 0));
	if (op == "*") {
		return ConstValue::fromInt(l * r);
	}
	if (op == "/") {
		if (r == 0) {
			return std::nullopt;
		}
		return ConstValue::fromInt(l / r);
	}
	if (r == 0) {
		return std::nullopt;
	}
	return ConstValue::fromInt(l % r);
}

std::optional<ConstValue> CodeGenerator::evalUnary(const AstNode &node) {
	const AstNode *first = childAt(node, 0);
	if (first->symbol == "primaryExp") {
		return evalPrimary(*first);
	}
	if (first->symbol == "unaryOp") {
		std::string op = nodeLexeme(*childAt(*first, 0));
		auto inner = evalUnary(*childAt(node, 1));
		if (!inner) {
			return std::nullopt;
		}
		if (op == "+") {
			return inner;
		}
		if (op == "-") {
			return ConstValue::fromInt(-asInt(*inner));
		}
		return ConstValue::fromBool(!asBool(*inner));
	}
	return std::nullopt;
}

std::optional<ConstValue> CodeGenerator::evalPrimary(const AstNode &node) {
	const AstNode *first = childAt(node, 0);
	if (first->symbol == "(") {
		return tryEval(*childAt(node, 1));
	}
	if (first->symbol == "number") {
		const AstNode *leaf = childAt(*first, 0);
		if (leaf->symbol == "IntConst") {
			return ConstValue::fromInt(std::stoi(leaf->lexeme));
		}
		return ConstValue::fromInt(static_cast<int>(std::stod(leaf->lexeme)));
	}
	if (first->symbol == "lVal") {
		const AstNode *identNode = expectChild(*first, "Ident");
		std::string name = nodeLexeme(*identNode);
		SymbolInfo *symbol = lookup(name);
		if (symbol && symbol->isCompileTimeConst && symbol->intValue) {
			return ConstValue::fromInt(*symbol->intValue);
		}
		return std::nullopt;
	}
	return std::nullopt;
}

void CodeGenerator::generate(const AstNode &root) {
	scopes_.clear();
	pushScope();
	currentFunction_ = nullptr;
	entryBlock_ = nullptr;
	currentReturnType_ = ReturnType::Void;
	builder_.set_insert_point(nullptr);
	const AstNode *compUnit = expectChild(root, "compUnit");
	emitCompUnit(*compUnit);
}

void CodeGenerator::emitCompUnit(const AstNode &node) {
	if (node.children.empty()) {
		return;
	}
	if (node.children.size() == 1) {
		emitUnit(*childAt(node, 0));
		return;
	}
	if (node.children.size() == 2) {
		emitCompUnit(*childAt(node, 0));
		emitUnit(*childAt(node, 1));
		return;
	}
	throw SemanticError("Unexpected compUnit form");
}

void CodeGenerator::emitUnit(const AstNode &node) {
	const AstNode *child = childAt(node, 0);
	if (child->symbol == "decl") {
		emitDecl(*child, /*isGlobal=*/currentFunction_ == nullptr);
		return;
	}
	if (child->symbol == "funcDef") {
		emitFuncDef(*child);
		return;
	}
	throw SemanticError("Unsupported unit: " + child->symbol);
}

void CodeGenerator::emitDecl(const AstNode &node, bool isGlobal) {
	const AstNode *child = childAt(node, 0);
	if (child->symbol == "constDecl") {
		emitConstDecl(*child, isGlobal);
	} else if (child->symbol == "varDecl") {
		emitVarDecl(*child, isGlobal);
	} else {
		throw SemanticError("Unknown decl type: " + child->symbol);
	}
}

ScalarType CodeGenerator::parseBType(const AstNode &node) {
	const AstNode *kw = childAt(node, 0);
	const std::string lex = nodeLexeme(*kw);
	if (lex == "int" || lex == "INT" || kw->symbol == "int") {
		return ScalarType::Int;
	}
	if (lex == "float" || lex == "FLOAT" || kw->symbol == "float") {
		return ScalarType::Float;
	}
	throw SemanticError("Unsupported base type: " + lex);
}

void CodeGenerator::emitConstDecl(const AstNode &node, bool isGlobal) {
	ScalarType type = parseBType(*expectChild(node, "bType"));
	const AstNode *defList = expectChild(node, "constDefList");
	visitConstDefList(*defList, type, isGlobal);
}

void CodeGenerator::emitVarDecl(const AstNode &node, bool isGlobal) {
	ScalarType type = parseBType(*expectChild(node, "bType"));
	const AstNode *defList = expectChild(node, "varDefList");
	visitVarDefList(*defList, type, isGlobal);
}

void CodeGenerator::visitConstDefList(const AstNode &node, ScalarType type,
								 bool isGlobal) {
	if (node.children.size() == 1) {
		handleConstDef(*childAt(node, 0), type, isGlobal);
		return;
	}
	if (node.children.size() == 3) {
		visitConstDefList(*childAt(node, 0), type, isGlobal);
		handleConstDef(*childAt(node, 2), type, isGlobal);
		return;
	}
	throw SemanticError("Unexpected constDefList form");
}

void CodeGenerator::visitVarDefList(const AstNode &node, ScalarType type,
						      bool isGlobal) {
	if (node.children.size() == 1) {
		handleVarDef(*childAt(node, 0), type, isGlobal);
		return;
	}
	if (node.children.size() == 3) {
		visitVarDefList(*childAt(node, 0), type, isGlobal);
		handleVarDef(*childAt(node, 2), type, isGlobal);
		return;
	}
	throw SemanticError("Unexpected varDefList form");
}

void CodeGenerator::handleConstDef(const AstNode &node, ScalarType type,
						    bool isGlobal) {
	const AstNode *identNode = expectChild(node, "Ident");
	std::string name = nodeLexeme(*identNode);
	const AstNode *initNode = expectChild(node, "constInitVal");
	const AstNode *constExp = expectChild(*initNode, "constExp");
	ConstValue value = getConstOrThrow(*constExp);
	SymbolInfo symbol;
	symbol.type = type;
	symbol.isGlobal = isGlobal;
	symbol.isConst = false;
	symbol.isCompileTimeConst = true;
	if (type == ScalarType::Int) {
		int iv = asInt(value);
		symbol.intValue = iv;
		Constant *init = ConstantInt::get(iv, &module_);
		if (isGlobal) {
			symbol.storage = GlobalVariable::create(name, &module_,
						module_.get_int32_type(), false, init);
			define(name, symbol);
		} else {
			Value *addr = createAlloca(typeFor(type));
			symbol.storage = addr;
			define(name, symbol);
			emitStore(init, addr);
		}
		return;
	}
	if (type == ScalarType::Float) {
		symbol.floatValue = asFloat(value);
		symbol.intValue = static_cast<int>(*symbol.floatValue);
		symbol.storage = nullptr;
		define(name, symbol);
		return;
	}
}

void CodeGenerator::handleVarDef(const AstNode &node, ScalarType type,
					       bool isGlobal) {
	const AstNode *identNode = expectChild(node, "Ident");
	std::string name = nodeLexeme(*identNode);
	const AstNode *initNode = findChild(node, "initVal");
	SymbolInfo symbol;
	symbol.type = type;
	symbol.isConst = false;
	symbol.isGlobal = isGlobal;

	if (type == ScalarType::Float) {
		ConstValue value = initNode ? getConstOrThrow(*expectChild(*initNode, "exp"))
					      : ConstValue::fromFloat(0.0);
		symbol.floatValue = asFloat(value);
		symbol.isConst = true;
		symbol.isCompileTimeConst = true;
		symbol.intValue = static_cast<int>(*symbol.floatValue);
		define(name, symbol);
		return;
	}

	if (isGlobal) {
		ConstValue value = initNode ? getConstOrThrow(*expectChild(*initNode, "exp"))
					      : ConstValue::fromInt(0);
		int iv = asInt(value);
		Constant *initConst = ConstantInt::get(iv, &module_);
		GlobalVariable *gv = GlobalVariable::create(name, &module_,
						module_.get_int32_type(), false, initConst);
		symbol.storage = gv;
		symbol.intValue = iv;
		define(name, symbol);
		return;
	}

	Value *addr = createAlloca(typeFor(type));
	symbol.storage = addr;
	define(name, symbol);
	if (initNode) {
		Value *rhs = emitExp(*expectChild(*initNode, "exp"));
		rhs = ensureInt(rhs);
		emitStore(rhs, addr);
	}
}

void CodeGenerator::emitFuncDef(const AstNode &node) {
	const AstNode *retNode = childAt(node, 0);
	const AstNode *identNode = expectChild(node, "Ident");
	std::string funcName = nodeLexeme(*identNode);
	ReturnType retType = ReturnType::Void;
	Type *returnTy = module_.get_void_type();
	if (retNode->symbol == "bType") {
		ScalarType bt = parseBType(*retNode);
		if (bt != ScalarType::Int) {
			throw SemanticError("Only int return type supported");
		}
		retType = ReturnType::Int;
		returnTy = module_.get_int32_type();
	}
	currentReturnType_ = retType;
	std::vector<Type *> paramTypes;
	FunctionType *funcType = FunctionType::get(returnTy, paramTypes);
	Function *func = Function::create(funcType, funcName, &module_);
	currentFunction_ = func;
	entryBlock_ = BasicBlock::create(&module_, funcName + ".entry", func);
	builder_.set_insert_point(entryBlock_);
	pushScope();
	const AstNode *blockNode = expectChild(node, "block");
	emitBlock(*blockNode, /*createScope=*/false);
	if (!currentBlockTerminated()) {
		if (retType == ReturnType::Void) {
			builder_.create_void_ret();
		} else {
			builder_.create_ret(ConstantInt::get(0, &module_));
		}
	}
	popScope();
	currentFunction_ = nullptr;
	entryBlock_ = nullptr;
}

void CodeGenerator::emitBlock(const AstNode &node, bool createScope) {
	if (createScope) {
		pushScope();
	}
	const AstNode *items = findChild(node, "blockItemList");
	if (items) {
		emitBlockItemList(*items);
	}
	if (createScope) {
		popScope();
	}
}

void CodeGenerator::emitBlockItemList(const AstNode &node) {
	if (node.children.empty()) {
		return;
	}
	if (node.children.size() == 1) {
		emitBlockItem(*childAt(node, 0));
		return;
	}
	if (node.children.size() == 2) {
		emitBlockItemList(*childAt(node, 0));
		emitBlockItem(*childAt(node, 1));
		return;
	}
	throw SemanticError("Unexpected blockItemList shape");
}

void CodeGenerator::emitBlockItem(const AstNode &node) {
	const AstNode *child = childAt(node, 0);
	if (child->symbol == "decl") {
		emitDecl(*child, /*isGlobal=*/false);
		return;
	}
	if (child->symbol == "stmt") {
		emitStmt(*child);
		return;
	}
	throw SemanticError("Unknown block item type: " + child->symbol);
}

void CodeGenerator::emitStmt(const AstNode &node) {
	const AstNode *child = childAt(node, 0);
	if (child->symbol == "matchedStmt") {
		emitMatchedStmt(*child);
		return;
	}
	if (child->symbol == "unmatchedStmt") {
		emitUnmatchedStmt(*child);
		return;
	}
	throw SemanticError("Unknown stmt form: " + child->symbol);
}

void CodeGenerator::emitMatchedStmt(const AstNode &node) {
	if (node.children.empty()) {
		return;
	}
	const AstNode *first = childAt(node, 0);
	if (first->symbol == "lVal") {
		emitAssign(*first, *childAt(node, 2));
		return;
	}
	if (first->symbol == "expOpt") {
		const AstNode *expNode = findChild(*first, "exp");
		if (expNode) {
			emitExp(*expNode);
		}
		return;
	}
	if (first->symbol == "block") {
		emitBlock(*first);
		return;
	}
	if (first->symbol == "if") {
		const AstNode *condNode = childAt(node, 2);
		const AstNode *thenNode = childAt(node, 4);
		const AstNode *elseNode = childAt(node, 6);
		emitIfLike(*condNode, *thenNode, elseNode);
		return;
	}
	if (first->symbol == "return") {
		const AstNode *expOpt = childAt(node, 1);
		emitReturn(expOpt);
		return;
	}
	throw SemanticError("Unsupported matched statement");
}

void CodeGenerator::emitUnmatchedStmt(const AstNode &node) {
	if (node.children.empty()) {
		return;
	}
	const AstNode *first = childAt(node, 0);
	if (first->symbol != "if") {
		throw SemanticError("Unsupported unmatched statement");
	}
	const AstNode *condNode = childAt(node, 2);
	const AstNode *thenNode = childAt(node, 4);
	if (node.children.size() == 5) {
		emitIfLike(*condNode, *thenNode, nullptr);
		return;
	}
	const AstNode *elseStmt = childAt(node, 6);
	const AstNode *elseChild = childAt(*elseStmt, 0);
	if (elseChild->symbol == "matchedStmt") {
		emitIfLike(*condNode, *thenNode, elseChild);
	} else {
		emitIfLike(*condNode, *thenNode, elseStmt);
	}
}

void CodeGenerator::emitAssign(const AstNode &lvalNode, const AstNode &expNode) {
	const AstNode *identNode = expectChild(lvalNode, "Ident");
	std::string name = nodeLexeme(*identNode);
	SymbolInfo *symbol = lookup(name);
	if (!symbol) {
		throw SemanticError("Unknown identifier: " + name);
	}
	if (!symbol->storage) {
		throw SemanticError("Cannot assign to constant or unsupported type: " + name);
	}
	Value *rhs = emitExp(expNode);
	rhs = ensureInt(rhs);
	emitStore(rhs, symbol->storage);
}

void CodeGenerator::emitReturn(const AstNode *expOpt) {
	if (!currentFunction_) {
		throw SemanticError("return outside function");
	}
	if (currentReturnType_ == ReturnType::Void) {
		builder_.create_void_ret();
		return;
	}
	Value *retVal = ConstantInt::get(0, &module_);
	if (expOpt) {
		if (const AstNode *expNode = findChild(*expOpt, "exp")) {
			retVal = ensureInt(emitExp(*expNode));
		}
	}
	builder_.create_ret(retVal);
}

void CodeGenerator::emitIfLike(const AstNode &condNode, const AstNode &thenNode,
				   const AstNode *elseNode) {
	Value *cond = emitCond(condNode);
	Function *func = currentFunction_;
	if (!func) {
		throw SemanticError("if outside function");
	}
	BasicBlock *thenBB = BasicBlock::create(&module_, nextLabel("if.then"), func);
	BasicBlock *mergeBB = BasicBlock::create(&module_, nextLabel("if.end"), func);
	BasicBlock *elseBB = elseNode ? BasicBlock::create(&module_, nextLabel("if.else"), func)
						: mergeBB;
	builder_.create_cond_br(cond, thenBB, elseBB);
	auto emitBody = [&](const AstNode &body) {
		if (body.symbol == "matchedStmt") {
			emitMatchedStmt(body);
		} else if (body.symbol == "stmt") {
			emitStmt(body);
		} else if (body.symbol == "block") {
			emitBlock(body);
		} else if (body.symbol == "unmatchedStmt") {
			emitUnmatchedStmt(body);
		} else {
			throw SemanticError("Unsupported branch body");
		}
	};
	builder_.set_insert_point(thenBB);
	emitBody(thenNode);
	if (!currentBlockTerminated()) {
		builder_.create_br(mergeBB);
	}
	if (elseNode) {
		builder_.set_insert_point(elseBB);
		emitBody(*elseNode);
		if (!currentBlockTerminated()) {
			builder_.create_br(mergeBB);
		}
	}
	builder_.set_insert_point(mergeBB);
}

int main(int argc, char *argv[]) {
	try {
		fs::path workspace = fs::current_path();
		fs::path inputDir = (argc > 1) ? fs::path(argv[1])
								 : workspace / "test_sources" / "intermediate_sources" / "inter_test";
		fs::path outputDir = (argc > 2) ? fs::path(argv[2])
								  : workspace / "test_sources" / "intermediate_sources" / "inter_output";
		if (!fs::exists(inputDir)) {
			throw std::runtime_error("Input directory does not exist: " + inputDir.string());
		}
		fs::create_directories(outputDir);
		std::vector<fs::path> sources;
		for (const auto &entry : fs::directory_iterator(inputDir)) {
			if (entry.is_regular_file() && entry.path().extension() == ".sy") {
				sources.push_back(entry.path());
			}
		}
		std::sort(sources.begin(), sources.end());
		if (sources.empty()) {
			throw std::runtime_error("No .sy AST files found under " + inputDir.string());
		}
		for (const auto &source : sources) {
			auto ast = parseAstFile(source);
			Module module("sysy2022_compiler");
			declareRuntime(module);
			CodeGenerator generator(module);
			generator.setSourceName(source.filename().string());
			generator.generate(*ast);
			module.set_print_name();
			fs::path outPath = outputDir / source.stem();
			outPath += ".ll";
			std::ofstream output(outPath);
			if (!output) {
				throw std::runtime_error("Failed to write " + outPath.string());
			}
			output << "; ModuleID = 'sysy2022_compiler'\n";
			output << "source_filename = \"" << source.string() << "\"\n";
			output << module.print();
			std::cout << "Generated " << outPath.filename() << " from " << source.filename()
				  << '\n';
		}
	} catch (const std::exception &ex) {
		std::cerr << "Intermediate code generator failed: " << ex.what() << '\n';
		return 1;
	}
	return 0;
}
