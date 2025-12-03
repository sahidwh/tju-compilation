// Lexical analyzer for the simplified C-- language.
// Builds NFAs for identifiers/integers/floats, determinizes and minimizes them,
// then uses the resulting DFAs to tokenize every source file under
// test_sources/lex_sources/lex_test. Results are written to lex_output.

#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <queue>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace fs = std::filesystem;

struct Position {
	int line = 1;
	int column = 1;
};

struct Cursor {
	size_t index = 0;
	int line = 1;
	int column = 1;
};

struct TokenRecord {
	std::string lexeme;
	std::string tag;
};

class SymbolTable {
public:
	struct Entry {
		std::string lexeme;
		std::string category;
	};

	void clear() {
		entries_.clear();
		index_.clear();
	}

	int intern(const std::string &lexeme, const std::string &category) {
		const std::string key = category + ":" + lexeme;
		auto it = index_.find(key);
		if (it != index_.end()) {
			return it->second;
		}

		const int id = static_cast<int>(entries_.size());
		entries_.push_back({lexeme, category});
		index_[key] = id;
		return id;
	}

	const std::vector<Entry> &entries() const {
		return entries_;
	}

private:
	std::vector<Entry> entries_;
	std::unordered_map<std::string, int> index_;
};

class NFA {
public:
	explicit NFA(size_t alphabetSize) : alphabetSize_(alphabetSize) {}

	int addState(bool accepting) {
		const int id = static_cast<int>(accepting_.size());
		accepting_.push_back(accepting);
		transitions_.push_back(std::vector<std::vector<int>>(alphabetSize_));
		epsilon_.push_back({});
		if (startState_ < 0) {
			startState_ = id;
		}
		return id;
	}

	void setStartState(int id) {
		startState_ = id;
	}

	void setAccepting(int id, bool accepting) {
		accepting_[id] = accepting;
	}

	void addTransition(int from, int symbol, int to) {
		transitions_[from][symbol].push_back(to);
	}

	void addEpsilonTransition(int from, int to) {
		epsilon_[from].push_back(to);
	}

	size_t alphabetSize() const {
		return alphabetSize_;
	}

	int startState() const {
		return startState_;
	}

	size_t stateCount() const {
		return accepting_.size();
	}

	bool isAccepting(int id) const {
		return accepting_[id];
	}

	const std::vector<std::vector<std::vector<int>>> &transitions() const {
		return transitions_;
	}

	const std::vector<std::vector<int>> &epsilonTransitions() const {
		return epsilon_;
	}

private:
	size_t alphabetSize_;
	int startState_ = -1;
	std::vector<bool> accepting_;
	std::vector<std::vector<std::vector<int>>> transitions_;
	std::vector<std::vector<int>> epsilon_;
};

class DFA {
public:
	explicit DFA(size_t alphabetSize = 0)
		: alphabetSize_(alphabetSize), startState_(0) {}

	int addState(bool accepting) {
		const int id = static_cast<int>(accepting_.size());
		accepting_.push_back(accepting);
		transitions_.push_back(std::vector<int>(alphabetSize_, -1));
		return id;
	}

	void setAccepting(int id, bool accepting) {
		accepting_[id] = accepting;
	}

	void setTransition(int from, int symbol, int to) {
		transitions_[from][symbol] = to;
	}

	void setStartState(int id) {
		startState_ = id;
	}

	size_t alphabetSize() const {
		return alphabetSize_;
	}

	size_t stateCount() const {
		return accepting_.size();
	}

	const std::vector<std::vector<int>> &transitions() const {
		return transitions_;
	}

	const std::vector<bool> &accepting() const {
		return accepting_;
	}

	int startState() const {
		return startState_;
	}

	int match(const std::string &text, size_t startPos,
			  const std::function<int(char)> &classifier) const {
		if (transitions_.empty()) {
			return -1;
		}

		int state = startState_;
		size_t cursor = startPos;
		int lastAccept = -1;
		int consumed = 0;

		while (cursor < text.size()) {
			const int symbol = classifier(text[cursor]);
			if (symbol < 0 || static_cast<size_t>(symbol) >= alphabetSize_) {
				break;
			}
			const int nextState = transitions_[state][symbol];
			if (nextState < 0) {
				break;
			}
			state = nextState;
			++cursor;
			++consumed;
			if (accepting_[state]) {
				lastAccept = consumed;
			}
		}

		return lastAccept;
	}

	static DFA determinize(const NFA &nfa);
	static DFA minimize(const DFA &dfa);

private:
	size_t alphabetSize_ = 0;
	int startState_ = 0;
	std::vector<bool> accepting_;
	std::vector<std::vector<int>> transitions_;
};

namespace {

struct VectorHasher {
	size_t operator()(const std::vector<int> &vec) const noexcept {
		size_t seed = 0;
		for (int value : vec) {
			seed ^= static_cast<size_t>(value) + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
		}
		return seed;
	}
};

std::vector<int> epsilonClosure(const NFA &nfa, const std::vector<int> &states) {
	std::vector<int> closure;
	std::vector<bool> visited(nfa.stateCount(), false);
	std::queue<int> pending;

	for (int state : states) {
		if (!visited[state]) {
			visited[state] = true;
			closure.push_back(state);
			pending.push(state);
		}
	}

	while (!pending.empty()) {
		const int state = pending.front();
		pending.pop();
		for (int target : nfa.epsilonTransitions()[state]) {
			if (!visited[target]) {
				visited[target] = true;
				closure.push_back(target);
				pending.push(target);
			}
		}
	}

	std::sort(closure.begin(), closure.end());
	closure.erase(std::unique(closure.begin(), closure.end()), closure.end());
	return closure;
}

std::vector<int> moveOnSymbol(const NFA &nfa, const std::vector<int> &states, int symbol) {
	std::vector<int> moved;
	for (int state : states) {
		const auto &targets = nfa.transitions()[state][symbol];
		moved.insert(moved.end(), targets.begin(), targets.end());
	}
	std::sort(moved.begin(), moved.end());
	moved.erase(std::unique(moved.begin(), moved.end()), moved.end());
	return moved;
}

void stepCursor(Cursor &cursor, char ch) {
	++cursor.index;
	if (ch == '\n' || ch == '\r') {
		++cursor.line;
		cursor.column = 1;
	} else {
		++cursor.column;
	}
}

std::string toLower(std::string value) {
	std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) {
		return static_cast<char>(std::tolower(c));
	});
	return value;
}

bool isAlpha(unsigned char ch) {
	return std::isalpha(ch) != 0;
}

bool isDigit(unsigned char ch) {
	return std::isdigit(ch) != 0;
}

} // namespace

DFA DFA::determinize(const NFA &nfa) {
	if (nfa.stateCount() == 0) {
		return DFA(nfa.alphabetSize());
	}

	DFA dfa(nfa.alphabetSize());
	const std::vector<int> startClosure = epsilonClosure(nfa, {nfa.startState()});
	if (startClosure.empty()) {
		return dfa;
	}

	std::queue<std::vector<int>> queue;
	std::unordered_map<std::vector<int>, int, VectorHasher> index;

	const auto addState = [&](const std::vector<int> &subset) {
		bool accepting = false;
		for (int state : subset) {
			if (nfa.isAccepting(state)) {
				accepting = true;
				break;
			}
		}
		const int id = dfa.addState(accepting);
		index[subset] = id;
		queue.push(subset);
		return id;
	};

	const int startId = addState(startClosure);
	dfa.setStartState(startId);

	while (!queue.empty()) {
		const std::vector<int> subset = queue.front();
		queue.pop();
		const int fromId = index[subset];

		for (size_t symbol = 0; symbol < nfa.alphabetSize(); ++symbol) {
			const std::vector<int> moved = moveOnSymbol(nfa, subset, static_cast<int>(symbol));
			if (moved.empty()) {
				continue;
			}
			const std::vector<int> closure = epsilonClosure(nfa, moved);
			if (closure.empty()) {
				continue;
			}

			auto it = index.find(closure);
			int targetId;
			if (it == index.end()) {
				targetId = addState(closure);
			} else {
				targetId = it->second;
			}
			dfa.setTransition(fromId, static_cast<int>(symbol), targetId);
		}
	}

	return dfa;
}

DFA DFA::minimize(const DFA &dfa) {
	const size_t stateCount = dfa.stateCount();
	if (stateCount == 0) {
		return dfa;
	}

	std::vector<bool> reachable(stateCount, false);
	std::queue<int> pending;
	reachable[dfa.startState()] = true;
	pending.push(dfa.startState());

	while (!pending.empty()) {
		const int state = pending.front();
		pending.pop();
		for (size_t symbol = 0; symbol < dfa.alphabetSize(); ++symbol) {
			const int target = dfa.transitions()[state][symbol];
			if (target >= 0 && !reachable[target]) {
				reachable[target] = true;
				pending.push(target);
			}
		}
	}

	std::vector<std::vector<bool>> distinguishable(stateCount, std::vector<bool>(stateCount, false));
	for (size_t i = 0; i < stateCount; ++i) {
		if (!reachable[i]) {
			continue;
		}
		for (size_t j = 0; j < i; ++j) {
			if (!reachable[j]) {
				continue;
			}
			if (dfa.accepting()[i] != dfa.accepting()[j]) {
				distinguishable[i][j] = distinguishable[j][i] = true;
			}
		}
	}

	bool updated;
	do {
		updated = false;
		for (size_t i = 0; i < stateCount; ++i) {
			if (!reachable[i]) {
				continue;
			}
			for (size_t j = 0; j < i; ++j) {
				if (!reachable[j] || distinguishable[i][j]) {
					continue;
				}
				for (size_t symbol = 0; symbol < dfa.alphabetSize(); ++symbol) {
					const int ti = dfa.transitions()[i][symbol];
					const int tj = dfa.transitions()[j][symbol];
					if (ti == tj) {
						continue;
					}
					if (ti < 0 || tj < 0) {
						if (ti != tj) {
							distinguishable[i][j] = distinguishable[j][i] = true;
							updated = true;
						}
						break;
					}
					const int a = std::max(ti, tj);
					const int b = std::min(ti, tj);
					if (distinguishable[a][b]) {
						distinguishable[i][j] = distinguishable[j][i] = true;
						updated = true;
						break;
					}
				}
			}
		}
	} while (updated);

	std::vector<int> representative(stateCount, -1);
	int nextId = 0;
	for (size_t i = 0; i < stateCount; ++i) {
		if (!reachable[i]) {
			continue;
		}
		bool assigned = false;
		for (size_t j = 0; j < i; ++j) {
			if (!reachable[j]) {
				continue;
			}
			if (!distinguishable[i][j]) {
				representative[i] = representative[j];
				assigned = true;
				break;
			}
		}
		if (!assigned) {
			representative[i] = nextId++;
		}
	}

	DFA minimized(dfa.alphabetSize());
	for (int i = 0; i < nextId; ++i) {
		minimized.addState(false);
	}
	minimized.setStartState(representative[dfa.startState()]);

	for (size_t oldState = 0; oldState < stateCount; ++oldState) {
		const int rep = representative[oldState];
		if (rep < 0) {
			continue;
		}
		if (dfa.accepting()[oldState]) {
			minimized.setAccepting(rep, true);
		}
		for (size_t symbol = 0; symbol < dfa.alphabetSize(); ++symbol) {
			const int target = dfa.transitions()[oldState][symbol];
			if (target < 0) {
				continue;
			}
			const int targetRep = representative[target];
			if (targetRep >= 0) {
				minimized.setTransition(rep, static_cast<int>(symbol), targetRep);
			}
		}
	}

	return minimized;
}

class Lexer {
public:
	Lexer();

	std::vector<TokenRecord> tokenize(const std::string &input) {
		symbolTable_.clear();
		std::vector<TokenRecord> tokens;
		Cursor cursor;

		while (cursor.index < input.size()) {
			char ch = input[cursor.index];
			const unsigned char uch = static_cast<unsigned char>(ch);

			if (std::isspace(uch)) {
				stepCursor(cursor, ch);
				continue;
			}

			if (consumeLineComment(input, cursor)) {
				continue;
			}
			if (consumeBlockComment(input, cursor, tokens)) {
				continue;
			}

			if (isAlpha(uch) || ch == '_') {
				processIdentifier(input, cursor, tokens);
				continue;
			}

			if (isDigit(uch) || (ch == '.' && cursor.index + 1 < input.size() &&
								  isDigit(static_cast<unsigned char>(input[cursor.index + 1])))) {
				if (processNumber(input, cursor, tokens)) {
					continue;
				}
			}

			if (processOperator(input, cursor, tokens)) {
				continue;
			}
			if (processSeparator(input, cursor, tokens)) {
				continue;
			}

			emitErrorToken(std::string(1, ch), {cursor.line, cursor.column}, tokens);
			stepCursor(cursor, ch);
		}

		return tokens;
	}

	const SymbolTable &symbolTable() const {
		return symbolTable_;
	}

private:
	static int classifyIdentifier(char ch) {
		const unsigned char uch = static_cast<unsigned char>(ch);
		if (isAlpha(uch)) {
			return 0;
		}
		if (isDigit(uch)) {
			return 1;
		}
		if (ch == '_') {
			return 2;
		}
		return -1;
	}

	static int classifyInteger(char ch) {
		return isDigit(static_cast<unsigned char>(ch)) ? 0 : -1;
	}

	static int classifyFloat(char ch) {
		if (isDigit(static_cast<unsigned char>(ch))) {
			return 0;
		}
		if (ch == '.') {
			return 1;
		}
		return -1;
	}

	void processIdentifier(const std::string &input, Cursor &cursor, std::vector<TokenRecord> &tokens) {
		const size_t startIndex = cursor.index;
		const int length = identifierDfa_.match(input, cursor.index, classifyIdentifier);
		if (length <= 0) {
			emitErrorToken(std::string(1, input[cursor.index]), {cursor.line, cursor.column}, tokens);
			stepCursor(cursor, input[cursor.index]);
			return;
		}

		const std::string lexeme = input.substr(startIndex, static_cast<size_t>(length));
		std::string lowered = toLower(lexeme);
		auto it = keywordCodes_.find(lowered);
		if (it != keywordCodes_.end()) {
			std::ostringstream oss;
			oss << "<KW," << it->second << ">";
			tokens.push_back({lexeme, oss.str()});
		} else {
			symbolTable_.intern(lexeme, "IDN");
			tokens.push_back({lexeme, "<IDN," + lexeme + ">"});
		}
		advanceCursor(cursor, input, static_cast<size_t>(length));
	}

	bool processNumber(const std::string &input, Cursor &cursor, std::vector<TokenRecord> &tokens) {
		const size_t startIndex = cursor.index;
		const int floatLen = floatDfa_.match(input, cursor.index, classifyFloat);
		if (floatLen > 0) {
			const std::string lexeme = input.substr(startIndex, static_cast<size_t>(floatLen));
			symbolTable_.intern(lexeme, "FLOAT");
			tokens.push_back({lexeme, "<FLOAT," + lexeme + ">"});
			advanceCursor(cursor, input, static_cast<size_t>(floatLen));
			return true;
		}

		const int intLen = integerDfa_.match(input, cursor.index, classifyInteger);
		if (intLen > 0) {
			const std::string lexeme = input.substr(startIndex, static_cast<size_t>(intLen));
			symbolTable_.intern(lexeme, "INT");
			tokens.push_back({lexeme, "<INT," + lexeme + ">"});
			advanceCursor(cursor, input, static_cast<size_t>(intLen));
			return true;
		}

		return false;
	}

	bool processOperator(const std::string &input, Cursor &cursor, std::vector<TokenRecord> &tokens) {
		static const std::vector<std::pair<std::string, int>> compoundOps = {
			{"==", 17}, {"!=", 20}, {"<=", 18}, {">=", 19}, {"&&", 21}, {"||", 22}
		};

		for (const auto &entry : compoundOps) {
			const auto &symbol = entry.first;
			if (cursor.index + symbol.size() > input.size()) {
				continue;
			}
			if (input.compare(cursor.index, symbol.size(), symbol) == 0) {
				std::ostringstream oss;
				oss << "<OP," << entry.second << ">";
				tokens.push_back({symbol, oss.str()});
				advanceCursor(cursor, input, symbol.size());
				return true;
			}
		}

		const auto singleIt = operatorCodes_.find(std::string(1, input[cursor.index]));
		if (singleIt != operatorCodes_.end()) {
			std::ostringstream oss;
			oss << "<OP," << singleIt->second << ">";
			tokens.push_back({std::string(1, input[cursor.index]), oss.str()});
			advanceCursor(cursor, input, 1);
			return true;
		}
		return false;
	}

	bool processSeparator(const std::string &input, Cursor &cursor, std::vector<TokenRecord> &tokens) {
		const auto it = separatorCodes_.find(input[cursor.index]);
		if (it == separatorCodes_.end()) {
			return false;
		}
		std::ostringstream oss;
		oss << "<SE," << it->second << ">";
		tokens.push_back({std::string(1, input[cursor.index]), oss.str()});
		advanceCursor(cursor, input, 1);
		return true;
	}

	bool consumeLineComment(const std::string &input, Cursor &cursor) {
		if (input[cursor.index] != '/' || cursor.index + 1 >= input.size() || input[cursor.index + 1] != '/') {
			return false;
		}
		advanceCursor(cursor, input, 2);
		while (cursor.index < input.size()) {
			const char ch = input[cursor.index];
			if (ch == '\n') {
				break;
			}
			stepCursor(cursor, ch);
		}
		return true;
	}

	bool consumeBlockComment(const std::string &input, Cursor &cursor, std::vector<TokenRecord> &tokens) {
		if (input[cursor.index] != '/' || cursor.index + 1 >= input.size() || input[cursor.index + 1] != '*') {
			return false;
		}
		const Position startPos{cursor.line, cursor.column};
		advanceCursor(cursor, input, 2);
		bool closed = false;
		while (cursor.index < input.size()) {
			if (input[cursor.index] == '*' && cursor.index + 1 < input.size() && input[cursor.index + 1] == '/') {
				advanceCursor(cursor, input, 2);
				closed = true;
				break;
			}
			stepCursor(cursor, input[cursor.index]);
		}
		if (!closed) {
			emitErrorToken("/*", startPos, tokens);
		}
		return true;
	}

	void emitErrorToken(const std::string &lexeme, const Position &pos, std::vector<TokenRecord> &tokens) {
		std::ostringstream oss;
		oss << "<ERROR," << pos.line << "," << pos.column << ">";
		tokens.push_back({lexeme, oss.str()});
	}

	void advanceCursor(Cursor &cursor, const std::string &text, size_t length) {
		for (size_t i = 0; i < length && cursor.index < text.size(); ++i) {
			stepCursor(cursor, text[cursor.index]);
		}
	}

	static NFA buildIdentifierNfa() {
		NFA nfa(3);
		const int s0 = nfa.addState(false);
		const int s1 = nfa.addState(true);
		nfa.setStartState(s0);
		nfa.addTransition(s0, 0, s1);
		nfa.addTransition(s0, 2, s1);
		nfa.addTransition(s1, 0, s1);
		nfa.addTransition(s1, 1, s1);
		nfa.addTransition(s1, 2, s1);
		return nfa;
	}

	static NFA buildIntegerNfa() {
		NFA nfa(1);
		const int s0 = nfa.addState(false);
		const int s1 = nfa.addState(true);
		nfa.setStartState(s0);
		nfa.addTransition(s0, 0, s1);
		nfa.addTransition(s1, 0, s1);
		return nfa;
	}

	static NFA buildFloatNfa() {
		// Accepts patterns like digits '.' digits or '.' digits.
		NFA nfa(2);
		const int s0 = nfa.addState(false);
		const int digitsBefore = nfa.addState(false);
		const int dotAfterDigits = nfa.addState(false);
		const int digitsAfterDot = nfa.addState(true);
		const int leadingDot = nfa.addState(false);
		nfa.setStartState(s0);

		nfa.addTransition(s0, 0, digitsBefore);
		nfa.addTransition(digitsBefore, 0, digitsBefore);
		nfa.addTransition(digitsBefore, 1, dotAfterDigits);
		nfa.addTransition(dotAfterDigits, 0, digitsAfterDot);
		nfa.addTransition(digitsAfterDot, 0, digitsAfterDot);

		nfa.addTransition(s0, 1, leadingDot);
		nfa.addTransition(leadingDot, 0, digitsAfterDot);

		return nfa;
	}

	DFA identifierDfa_;
	DFA integerDfa_;
	DFA floatDfa_;
	SymbolTable symbolTable_;

	const std::unordered_map<std::string, int> keywordCodes_ = {
		{"int", 1},   {"void", 2},  {"return", 3}, {"const", 4},
		{"main", 5},  {"float", 6}, {"if", 7},     {"else", 8}
	};

	const std::unordered_map<std::string, int> operatorCodes_ = {
		{"+", 9},  {"-", 10}, {"*", 11}, {"/", 12}, {"%", 13},
		{"=", 14}, {">", 15}, {"<", 16}
	};

	const std::unordered_map<char, int> separatorCodes_ = {
		{'(', 23}, {')', 24}, {'{', 25}, {'}', 26}, {';', 27}, {',', 28}
	};
};

Lexer::Lexer()
	: identifierDfa_(DFA::minimize(DFA::determinize(buildIdentifierNfa()))),
	  integerDfa_(DFA::minimize(DFA::determinize(buildIntegerNfa()))),
	  floatDfa_(DFA::minimize(DFA::determinize(buildFloatNfa()))) {}

std::string readFile(const fs::path &path) {
	std::ifstream input(path);
	if (!input) {
		throw std::runtime_error("Failed to open " + path.string());
	}
	std::ostringstream buffer;
	buffer << input.rdbuf();
	return buffer.str();
}

void writeTokens(const fs::path &path, const std::vector<TokenRecord> &tokens) {
	std::ofstream output(path);
	if (!output) {
		throw std::runtime_error("Failed to write " + path.string());
	}
	for (const auto &token : tokens) {
		output << token.lexeme << '\t' << token.tag << '\n';
	}
}

int main(int argc, char *argv[]) {
	try {
		fs::path workspace = fs::current_path();
		fs::path inputDir = (argc > 1) ? fs::path(argv[1])
									   : workspace / "test_sources" / "lex_sources" / "lex_test";
		fs::path outputDir = (argc > 2) ? fs::path(argv[2])
										: workspace / "test_sources" / "lex_sources" / "lex_output";

		if (!fs::exists(inputDir)) {
			std::cerr << "Input directory not found: " << inputDir << '\n';
			return 1;
		}
		fs::create_directories(outputDir);

		std::vector<fs::path> sources;
		for (const auto &entry : fs::directory_iterator(inputDir)) {
			if (!entry.is_regular_file()) {
				continue;
			}
			if (entry.path().extension() == ".sy") {
				sources.push_back(entry.path());
			}
		}

		if (sources.empty()) {
			std::cerr << "No .sy files found in " << inputDir << '\n';
			return 1;
		}

		std::sort(sources.begin(), sources.end());

		Lexer lexer;
		for (const auto &sourcePath : sources) {
			const std::string content = readFile(sourcePath);
			const std::vector<TokenRecord> tokens = lexer.tokenize(content);
			fs::path outputPath = outputDir / sourcePath.stem();
			outputPath += ".out";
			writeTokens(outputPath, tokens);
			std::cout << "Generated tokens for " << sourcePath.filename() << " -> "
					  << outputPath.filename() << '\n';
		}
	} catch (const std::exception &ex) {
		std::cerr << "Lexer failed: " << ex.what() << '\n';
		return 1;
	}

	return 0;
}

