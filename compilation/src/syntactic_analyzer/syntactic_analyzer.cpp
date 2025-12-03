// SLR(1) syntax analyzer for the C-- subset.
// Reads lexical analyzer outputs (token pairs) and emits shift/reduce traces per spec.

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace fs = std::filesystem;

namespace {

constexpr const char *kEpsilon = "$";

struct Token {
	std::string symbol;
	std::string lexeme;
};

struct AstNode {
	std::string symbol;
	std::string lexeme;
	std::vector<std::unique_ptr<AstNode>> children;
};

struct ParseResult {
	std::vector<std::string> trace;
	std::unique_ptr<AstNode> ast;
};

struct Production {
	int id;
	std::string lhs;
	std::vector<std::string> rhs;
};

struct Item {
	int production;
	size_t dot;

	bool operator==(const Item &other) const {
		return production == other.production && dot == other.dot;
	}

	bool operator<(const Item &other) const {
		if (production != other.production) {
			return production < other.production;
		}
		return dot < other.dot;
	}
};

enum class ActionType { Shift, Reduce, Accept };

struct Action {
	ActionType type = ActionType::Shift;
	int target = -1;
};

struct ItemVectorHash {
	size_t operator()(const std::vector<Item> &items) const noexcept {
		size_t seed = 0;
		for (const auto &item : items) {
			seed ^= std::hash<int>{}(item.production * 131 + static_cast<int>(item.dot)) +
					0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
		}
		return seed;
	}
};

class SlrParser {
public:
	SlrParser() {
		initializeTerminals();
		productions_.push_back({0, "S'", {startSymbol_}});
		buildGrammar();
		indexProductions();
		computeFirstSets();
		computeFollowSets();
		buildCanonicalCollection();
		buildParsingTables();
	}

	ParseResult parse(const std::vector<Token> &tokens) const {
		ParseResult result;
		std::vector<std::string> &trace = result.trace;
		std::vector<int> stateStack = {0};
		std::vector<std::string> symbolStack = {"#"};
		std::vector<std::unique_ptr<AstNode>> astStack;
		size_t inputIndex = 0;
		size_t step = 1;

		while (true) {
			if (inputIndex >= tokens.size()) {
				throw std::runtime_error("Unexpected end of token stream");
			}
			const Token &lookahead = tokens[inputIndex];
			const auto &actions = actionTable_.at(stateStack.back());
			auto actionIt = actions.find(lookahead.symbol);
			if (actionIt == actions.end()) {
				const std::string stackTopSymbol = symbolStack.empty() ? "#" : symbolStack.back();
				logStep(trace, step++, stackTopSymbol, lookahead.symbol, "error");
				if (lookahead.symbol == "EOF") {
					break;
				}
				++inputIndex;
				continue;
			}

			const Action &act = actionIt->second;
			if (act.type == ActionType::Shift) {
				logStep(trace, step++, lookahead.symbol, lookahead.symbol, "move");
				symbolStack.push_back(lookahead.symbol);
				stateStack.push_back(act.target);
				auto leaf = std::make_unique<AstNode>();
				leaf->symbol = lookahead.symbol;
				leaf->lexeme = lookahead.lexeme;
				astStack.push_back(std::move(leaf));
				++inputIndex;
				continue;
			}

			if (act.type == ActionType::Accept) {
				logStep(trace, step++, startSymbol_, "EOF", "accept");
				break;
			}

			const Production &prod = productions_.at(act.target);
			logStep(trace, step++, prod.lhs, lookahead.symbol, "reduction");
			std::vector<std::unique_ptr<AstNode>> children;
			for (size_t i = 0; i < prod.rhs.size(); ++i) {
				if (symbolStack.size() <= 1 || stateStack.size() <= 1) {
					throw std::runtime_error("Parser stack underflow during reduction");
				}
				symbolStack.pop_back();
				stateStack.pop_back();
				if (!astStack.empty()) {
					children.push_back(std::move(astStack.back()));
					astStack.pop_back();
				}
			}
			std::reverse(children.begin(), children.end());
			auto parent = std::make_unique<AstNode>();
			parent->symbol = prod.lhs;
			parent->children = std::move(children);
			symbolStack.push_back(prod.lhs);
			const auto &gotoRow = gotoTable_.at(stateStack.back());
			auto gotoIt = gotoRow.find(prod.lhs);
			if (gotoIt == gotoRow.end()) {
				throw std::runtime_error("No goto entry for state " + std::to_string(stateStack.back()) +
										 " on symbol " + prod.lhs);
			}
			stateStack.push_back(gotoIt->second);
			astStack.push_back(std::move(parent));
		}

		if (!astStack.empty()) {
			result.ast = std::move(astStack.back());
		}
		return result;
	}

	void dumpDiagnostics() const {
		const std::vector<std::string> symbols = {"addExp", "relExp", "eqExp", "lAndExp", "lOrExp"};
		for (const auto &sym : symbols) {
			auto it = follow_.find(sym);
			if (it == follow_.end()) {
				continue;
			}
			std::cerr << "FOLLOW(" << sym << ") = {";
			bool first = true;
			for (const auto &token : it->second) {
				if (!first) {
					std::cerr << ", ";
				}
				std::cerr << token;
				first = false;
			}
			std::cerr << "}\n";
		}
	}

private:
	void logStep(std::vector<std::string> &trace, size_t step, const std::string &stackTop,
				 const std::string &lookahead, const std::string &action) const {
		std::ostringstream oss;
		oss << step << '\t' << stackTop << '#' << lookahead << '\t' << action;
		trace.push_back(oss.str());
	}

	void initializeTerminals() {
		const std::vector<std::string> terms = {
			"int",   "void",      "return",   "const",    "float",  "if",   "else",
			"Ident", "IntConst",  "floatConst", "+",      "-",     "!",    "*",
			"/",     "%",        "=",        "==",       "!=",     "<",    ">",
			"<=",    ">=",       "&&",       "||",       "(",      ")",   "{",
			"}",     ";",        ",",        "EOF",      "ERROR"};
		terminals_.insert(terms.begin(), terms.end());
	}

	void addProduction(const std::string &lhs, const std::vector<std::string> &rhs) {
		productions_.push_back({static_cast<int>(productions_.size()), lhs, rhs});
		nonTerminals_.insert(lhs);
		for (const auto &symbol : rhs) {
			if (!symbol.empty() && terminals_.count(symbol) == 0) {
				nonTerminals_.insert(symbol);
			}
		}
	}

	void buildGrammar() {
		addProduction("Program", {"compUnit"});
		addProduction("compUnit", {"compUnit", "unit"});
		addProduction("compUnit", {"unit"});
		addProduction("unit", {"decl"});
		addProduction("unit", {"funcDef"});
		addProduction("decl", {"constDecl"});
		addProduction("decl", {"varDecl"});
		addProduction("constDecl", {"const", "bType", "constDefList", ";"});
		addProduction("constDefList", {"constDefList", ",", "constDef"});
		addProduction("constDefList", {"constDef"});
		addProduction("constDef", {"Ident", "=", "constInitVal"});
		addProduction("constInitVal", {"constExp"});
		addProduction("varDecl", {"bType", "varDefList", ";"});
		addProduction("varDefList", {"varDefList", ",", "varDef"});
		addProduction("varDefList", {"varDef"});
		addProduction("varDef", {"Ident"});
		addProduction("varDef", {"Ident", "=", "initVal"});
		addProduction("initVal", {"exp"});
		addProduction("funcDef", {"void", "Ident", "(", "funcFParamsOpt", ")", "block"});
		addProduction("funcDef", {"bType", "Ident", "(", "funcFParamsOpt", ")", "block"});
		addProduction("funcFParamsOpt", {"funcFParams"});
		addProduction("funcFParamsOpt", {});
		addProduction("funcFParams", {"funcFParams", ",", "funcFParam"});
		addProduction("funcFParams", {"funcFParam"});
		addProduction("funcFParam", {"bType", "Ident"});
		addProduction("block", {"{", "blockItemList", "}"});
		addProduction("blockItemList", {"blockItemList", "blockItem"});
		addProduction("blockItemList", {});
		addProduction("blockItem", {"decl"});
		addProduction("blockItem", {"stmt"});
		addProduction("stmt", {"matchedStmt"});
		addProduction("stmt", {"unmatchedStmt"});
		addProduction("matchedStmt", {"lVal", "=", "exp", ";"});
		addProduction("matchedStmt", {"expOpt", ";"});
		addProduction("matchedStmt", {"block"});
		addProduction("matchedStmt", {"if", "(", "cond", ")", "matchedStmt", "else", "matchedStmt"});
		addProduction("matchedStmt", {"return", "expOpt", ";"});
		addProduction("unmatchedStmt", {"if", "(", "cond", ")", "stmt"});
		addProduction("unmatchedStmt", {"if", "(", "cond", ")", "matchedStmt", "else", "unmatchedStmt"});
		addProduction("expOpt", {"exp"});
		addProduction("expOpt", {});
		addProduction("lVal", {"Ident"});
		addProduction("primaryExp", {"(", "exp", ")"});
		addProduction("primaryExp", {"lVal"});
		addProduction("primaryExp", {"number"});
		addProduction("number", {"IntConst"});
		addProduction("number", {"floatConst"});
		addProduction("unaryExp", {"primaryExp"});
		addProduction("unaryExp", {"Ident", "(", "funcRParamsOpt", ")"});
		addProduction("unaryExp", {"unaryOp", "unaryExp"});
		addProduction("unaryOp", {"+"});
		addProduction("unaryOp", {"-"});
		addProduction("unaryOp", {"!"});
		addProduction("funcRParamsOpt", {"funcRParams"});
		addProduction("funcRParamsOpt", {});
		addProduction("funcRParams", {"funcRParams", ",", "funcRParam"});
		addProduction("funcRParams", {"funcRParam"});
		addProduction("funcRParam", {"exp"});
		addProduction("mulExp", {"mulExp", "mulOp", "unaryExp"});
		addProduction("mulExp", {"unaryExp"});
		addProduction("mulOp", {"*"});
		addProduction("mulOp", {"/"});
		addProduction("mulOp", {"%"});
		addProduction("addExp", {"addExp", "addOp", "mulExp"});
		addProduction("addExp", {"mulExp"});
		addProduction("addOp", {"+"});
		addProduction("addOp", {"-"});
		addProduction("relExp", {"relExp", "relOp", "addExp"});
		addProduction("relExp", {"addExp"});
		addProduction("relOp", {"<"});
		addProduction("relOp", {">"});
		addProduction("relOp", {"<="});
		addProduction("relOp", {">="});
		addProduction("eqExp", {"eqExp", "eqOp", "relExp"});
		addProduction("eqExp", {"relExp"});
		addProduction("eqOp", {"=="});
		addProduction("eqOp", {"!="});
		addProduction("lAndExp", {"lAndExp", "&&", "eqExp"});
		addProduction("lAndExp", {"eqExp"});
		addProduction("lOrExp", {"lOrExp", "||", "lAndExp"});
		addProduction("lOrExp", {"lAndExp"});
		addProduction("constExp", {"addExp"});
		addProduction("exp", {"lOrExp"});
		addProduction("cond", {"lOrExp"});
		addProduction("bType", {"int"});
		addProduction("bType", {"float"});
	}

	void indexProductions() {
		for (const auto &prod : productions_) {
			productionsByLhs_[prod.lhs].push_back(prod.id);
		}
		for (const auto &nt : nonTerminals_) {
			first_[nt];
			follow_[nt];
		}
	}

	void computeFirstSets() {
		bool changed;
		do {
			changed = false;
			for (const auto &prod : productions_) {
				if (prod.id == 0) {
					continue;
				}
				auto &firstLhs = first_[prod.lhs];
				if (prod.rhs.empty()) {
					if (firstLhs.insert(kEpsilon).second) {
						changed = true;
					}
					continue;
				}
				bool derivesEmpty = true;
				for (const auto &symbol : prod.rhs) {
					if (terminals_.count(symbol)) {
						if (firstLhs.insert(symbol).second) {
							changed = true;
						}
						derivesEmpty = false;
						break;
					}
					auto &firstSym = first_[symbol];
					for (const auto &entry : firstSym) {
						if (entry != kEpsilon && firstLhs.insert(entry).second) {
							changed = true;
						}
					}
					if (firstSym.count(kEpsilon) == 0) {
						derivesEmpty = false;
						break;
					}
				}
				if (derivesEmpty) {
					if (firstLhs.insert(kEpsilon).second) {
						changed = true;
					}
				}
			}
		} while (changed);
	}

	std::unordered_set<std::string> firstOfSequence(const std::vector<std::string> &symbols,
													size_t startIndex) const {
		std::unordered_set<std::string> result;
		bool derivesEmpty = true;
		for (size_t i = startIndex; i < symbols.size(); ++i) {
			const std::string &symbol = symbols[i];
			if (terminals_.count(symbol)) {
				result.insert(symbol);
				derivesEmpty = false;
				break;
			}
			const auto &firstSym = first_.at(symbol);
			for (const auto &entry : firstSym) {
				if (entry != kEpsilon) {
					result.insert(entry);
				}
			}
			if (firstSym.count(kEpsilon) == 0) {
				derivesEmpty = false;
				break;
			}
		}
		if (derivesEmpty) {
			result.insert(kEpsilon);
		}
		return result;
	}

	void computeFollowSets() {
		follow_[startSymbol_].insert("EOF");
		bool changed;
		do {
			changed = false;
			for (const auto &prod : productions_) {
				if (prod.id == 0) {
					continue;
				}
				for (size_t i = 0; i < prod.rhs.size(); ++i) {
					const std::string &symbol = prod.rhs[i];
					if (terminals_.count(symbol)) {
						continue;
					}
					auto firstBeta = firstOfSequence(prod.rhs, i + 1);
					auto &followSet = follow_[symbol];
					for (const auto &entry : firstBeta) {
						if (entry == kEpsilon) {
							continue;
						}
						if (followSet.insert(entry).second) {
							changed = true;
						}
					}
					if (firstBeta.count(kEpsilon) || i + 1 == prod.rhs.size()) {
						const auto &followLhs = follow_[prod.lhs];
						for (const auto &entry : followLhs) {
							if (followSet.insert(entry).second) {
								changed = true;
							}
						}
					}
				}
			}
		} while (changed);
	}

	std::vector<Item> closure(std::vector<Item> items) const {
		std::set<Item> itemSet(items.begin(), items.end());
		std::queue<Item> pending;
		for (const auto &item : itemSet) {
			pending.push(item);
		}

		while (!pending.empty()) {
			Item current = pending.front();
			pending.pop();
			const auto &prod = productions_.at(current.production);
			if (current.dot >= prod.rhs.size()) {
				continue;
			}
			const std::string &symbol = prod.rhs[current.dot];
			if (terminals_.count(symbol) || productionsByLhs_.count(symbol) == 0) {
				continue;
			}
			for (int prodId : productionsByLhs_.at(symbol)) {
				Item next{prodId, 0};
				if (itemSet.insert(next).second) {
					pending.push(next);
				}
			}
		}

		return {itemSet.begin(), itemSet.end()};
	}

	void buildCanonicalCollection() {
		std::vector<Item> startClosure = closure({{0, 0}});
		itemSets_.push_back(startClosure);
		transitions_.emplace_back();
		std::unordered_map<std::vector<Item>, int, ItemVectorHash> index;
		index[startClosure] = 0;
		std::queue<int> pending;
		pending.push(0);

		while (!pending.empty()) {
			int stateId = pending.front();
			pending.pop();
			std::unordered_map<std::string, std::vector<Item>> grouped;
			for (const auto &item : itemSets_[stateId]) {
				const auto &prod = productions_.at(item.production);
				if (item.dot >= prod.rhs.size()) {
					continue;
				}
				grouped[prod.rhs[item.dot]].push_back({item.production, item.dot + 1});
			}
			for (auto &entry : grouped) {
				auto closureSet = closure(entry.second);
				auto it = index.find(closureSet);
				int targetId;
				if (it == index.end()) {
					targetId = static_cast<int>(itemSets_.size());
					itemSets_.push_back(closureSet);
					transitions_.emplace_back();
					index[closureSet] = targetId;
					pending.push(targetId);
				} else {
					targetId = it->second;
				}
				transitions_[stateId][entry.first] = targetId;
			}
		}
	}

	void buildParsingTables() {
		actionTable_.resize(itemSets_.size());
		gotoTable_.resize(itemSets_.size());

		for (size_t state = 0; state < itemSets_.size(); ++state) {
			for (const auto &transition : transitions_[state]) {
				const std::string &symbol = transition.first;
				int target = transition.second;
				if (terminals_.count(symbol)) {
					setAction(state, symbol, {ActionType::Shift, target});
				} else {
					gotoTable_[state][symbol] = target;
				}
			}

			for (const auto &item : itemSets_[state]) {
				const auto &prod = productions_.at(item.production);
				if (item.dot < prod.rhs.size()) {
					continue;
				}
				if (prod.id == 0) {
					setAction(state, "EOF", {ActionType::Accept, -1});
					continue;
				}
				const auto &followSet = follow_.at(prod.lhs);
				for (const auto &terminal : followSet) {
					setAction(state, terminal, {ActionType::Reduce, prod.id});
				}
			}
		}
	}

	void setAction(size_t state, const std::string &terminal, const Action &action) {
		auto &row = actionTable_[state];
		auto it = row.find(terminal);
		if (it != row.end() && (it->second.type != action.type || it->second.target != action.target)) {
			std::ostringstream oss;
			oss << "SLR conflict at state " << state << " on terminal '" << terminal << "'";
			throw std::runtime_error(oss.str());
		}
		row[terminal] = action;
	}

	const std::string startSymbol_ = "Program";
	std::vector<Production> productions_;
	std::unordered_set<std::string> terminals_;
	std::unordered_set<std::string> nonTerminals_;
	std::unordered_map<std::string, std::vector<int>> productionsByLhs_;
	std::unordered_map<std::string, std::unordered_set<std::string>> first_;
	std::unordered_map<std::string, std::unordered_set<std::string>> follow_;
	std::vector<std::vector<Item>> itemSets_;
	std::vector<std::unordered_map<std::string, int>> transitions_;
	std::vector<std::unordered_map<std::string, Action>> actionTable_;
	std::vector<std::unordered_map<std::string, int>> gotoTable_;
};

std::string trim(const std::string &value) {
	size_t start = value.find_first_not_of(" \t\r\n");
	if (start == std::string::npos) {
		return "";
	}
	size_t end = value.find_last_not_of(" \t\r\n");
	return value.substr(start, end - start + 1);
}

std::string parseTagSymbol(const std::string &tagRaw) {
	if (tagRaw.size() < 2 || tagRaw.front() != '<' || tagRaw.back() != '>') {
		throw std::runtime_error("Invalid tag format: " + tagRaw);
	}
	std::string content = tagRaw.substr(1, tagRaw.size() - 2);
	size_t commaPos = content.find(',');
	if (commaPos == std::string::npos) {
		throw std::runtime_error("Invalid tag content: " + content);
	}
	std::string type = content.substr(0, commaPos);
	std::string value = content.substr(commaPos + 1);
	return type + ":" + value;
}

std::string keywordFromCode(int code) {
	static const std::unordered_map<int, std::string> table = {
		{1, "int"},   {2, "void"},  {3, "return"}, {4, "const"},
		{5, "main"},  {6, "float"}, {7, "if"},     {8, "else"}
	};
	auto it = table.find(code);
	return (it == table.end()) ? std::string{} : it->second;
}

std::string operatorFromCode(int code) {
	static const std::unordered_map<int, std::string> table = {
		{9, "+"},  {10, "-"}, {11, "*"}, {12, "/"}, {13, "%"},
		{14, "="}, {15, ">"}, {16, "<"}, {17, "=="}, {18, "<="},
		{19, ">="}, {20, "!="}, {21, "&&"}, {22, "||"}
	};
	auto it = table.find(code);
	return (it == table.end()) ? std::string{} : it->second;
}

std::string separatorFromCode(int code) {
	static const std::unordered_map<int, std::string> table = {
		{23, "("}, {24, ")"}, {25, "{"}, {26, "}"}, {27, ";"}, {28, ","}
	};
	auto it = table.find(code);
	return (it == table.end()) ? std::string{} : it->second;
}

Token decodeToken(const std::string &lexeme, const std::string &tagRaw) {
	std::string descriptor = parseTagSymbol(trim(tagRaw));
	size_t colon = descriptor.find(':');
	const std::string type = descriptor.substr(0, colon);
	const std::string value = descriptor.substr(colon + 1);

	if (type == "KW") {
		int code = std::stoi(value);
		std::string symbol = keywordFromCode(code);
		if (symbol.empty()) {
			throw std::runtime_error("Unknown keyword code: " + std::to_string(code));
		}
		if (symbol == "main") {
			return {"Ident", lexeme};
		}
		return {symbol, lexeme};
	}

	if (type == "IDN") {
		return {"Ident", lexeme};
	}

	if (type == "INT") {
		return {"IntConst", lexeme};
	}

	if (type == "FLOAT") {
		return {"floatConst", lexeme};
	}

	if (type == "OP") {
		int code = std::stoi(value);
		std::string symbol = operatorFromCode(code);
		if (symbol.empty()) {
			throw std::runtime_error("Unknown operator code: " + std::to_string(code));
		}
		return {symbol, lexeme};
	}

	if (type == "SE") {
		int code = std::stoi(value);
		std::string symbol = separatorFromCode(code);
		if (symbol.empty()) {
			throw std::runtime_error("Unknown separator code: " + std::to_string(code));
		}
		return {symbol, lexeme};
	}

	if (type == "ERROR") {
		return {"ERROR", lexeme};
	}

	throw std::runtime_error("Unsupported token tag: " + descriptor);
}

std::vector<Token> readTokenStream(const fs::path &path) {
	std::ifstream input(path);
	if (!input) {
		throw std::runtime_error("Failed to open " + path.string());
	}
	std::vector<Token> tokens;
	std::string line;
	while (std::getline(input, line)) {
		if (trim(line).empty()) {
			continue;
		}
		size_t tabPos = line.find('\t');
		if (tabPos == std::string::npos) {
			continue;
		}
		std::string lexeme = line.substr(0, tabPos);
		std::string tag = line.substr(tabPos + 1);
		tokens.push_back(decodeToken(trim(lexeme), trim(tag)));
	}
	tokens.push_back({"EOF", "EOF"});
	return tokens;
}

void writeTrace(const fs::path &path, const std::vector<std::string> &trace) {
	std::ofstream output(path);
	if (!output) {
		throw std::runtime_error("Failed to write " + path.string());
	}
	for (const auto &line : trace) {
		output << line << '\n';
	}
}

void dumpAst(std::ostream &os, const AstNode &node, int depth) {
	os << std::string(depth * 2, ' ') << node.symbol;
	if (!node.lexeme.empty()) {
		os << " (" << node.lexeme << ')';
	}
	os << '\n';
	for (const auto &child : node.children) {
		dumpAst(os, *child, depth + 1);
	}
}

void writeAst(const fs::path &path, const AstNode *root) {
	std::ofstream output(path);
	if (!output) {
		throw std::runtime_error("Failed to write " + path.string());
	}
	if (root) {
		dumpAst(output, *root, 0);
	}
}

} // namespace

int main(int argc, char *argv[]) {
	try {
		fs::path workspace = fs::current_path();
		fs::path inputDir = (argc > 1) ? fs::path(argv[1])
									   : workspace / "test_sources" / "syntax_sources" / "syn_test";
		fs::path outputDir = (argc > 2) ? fs::path(argv[2])
										: workspace / "test_sources" / "syntax_sources" / "syn_output";

		if (!fs::exists(inputDir)) {
			throw std::runtime_error("Input directory not found: " + inputDir.string());
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
			throw std::runtime_error("No .sy files found under " + inputDir.string());
		}

		SlrParser parser;
		if (std::getenv("SLR_DEBUG")) {
			parser.dumpDiagnostics();
		}

		for (const auto &source : sources) {
			std::vector<Token> tokens = readTokenStream(source);
			ParseResult parseResult = parser.parse(tokens);
			fs::path tracePath = outputDir / source.stem();
			tracePath += ".out";
			writeTrace(tracePath, parseResult.trace);
			fs::path astPath = outputDir / source.stem();
			astPath += ".ast";
			writeAst(astPath, parseResult.ast.get());
			std::cout << "Parsed " << source.filename() << " -> " << tracePath.filename()
					  << " and " << astPath.filename() << '\n';
		}
	} catch (const std::exception &ex) {
		std::cerr << "Syntax analyzer failed: " << ex.what() << '\n';
		return 1;
	}

	return 0;
}

