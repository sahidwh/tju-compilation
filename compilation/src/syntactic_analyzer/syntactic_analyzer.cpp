// slr_parser.cpp
// SLR parser implementing the grammar from the uploaded PDF (C-- subset).
// Usage: ./slr_parser /abs/path/to/token_file [ /abs/path/to/output_file ]
//
// Compile: g++ -std=c++17 slr_parser.cpp -O2 -o slr_parser

#include <bits/stdc++.h>
using namespace std;

/*
  Implementation notes (short):
  - We define the grammar (BNF) inside the code according to the PDF appendix.
  - We perform elimination of left recursion (general algorithm).
  - Compute FIRST/FOLLOW sets.
  - Build canonical LR(0) item sets (closure/goto).
  - Build SLR ACTION/GOTO tables using FOLLOW sets for reduce entries.
  - Read token file lines produced by the lexical analyzer (lexeme<TAB><TYPE,attr>).
    We map token lexeme -> grammar terminal string:
      - keywords like "int", "void", "return", "const", "main", "float", "if", "else" -> themselves
      - identifier -> "Ident"
      - integer -> "IntConst"
      - float -> "floatConst"
      - operators/separators use the lexeme text directly e.g. "==", "(", ")", "{", "}", ";", ",", "+", "-", "*", "/", "%", "=", ">", "<", "<=", ">=", "!=", "&&", "||"
  - The parser logs actions as: index \t stack_symbols#next_input \t action
*/

struct Production {
    string lhs;
    vector<string> rhs;
    Production(){}
    Production(const string& L, const vector<string>& R): lhs(L), rhs(R) {}
};

static vector<Production> prods; // list of productions (indexable)
static set<string> nonterminals_set;
static set<string> terminals_set;
static string START_SYMBOL = "Program"; // start as in PDF; we'll augment with S'

// helper: trim
static inline string trim(const string &s) {
    size_t a = s.find_first_not_of(" \t\r\n");
    if (a==string::npos) return "";
    size_t b = s.find_last_not_of(" \t\r\n");
    return s.substr(a, b-a+1);
}

static vector<string> split_ws(const string &s) {
    vector<string> r;
    string cur;
    for (size_t i=0;i<s.size();++i) {
        char c = s[i];
        if (isspace((unsigned char)c)) {
            if (!cur.empty()) { r.push_back(cur); cur.clear(); }
        } else {
            cur.push_back(c);
        }
    }
    if (!cur.empty()) r.push_back(cur);
    return r;
}

// Build grammar (BNF) programmatically based on the PDF appendix; expand starred constructs
void build_initial_grammar() {
    // We'll use names exactly matching PDF: Ident, IntConst, floatConst are terminals representing tokens.
    // Many terminals are literal strings like "int", "(", ")", "+", etc.

    auto add = [&](const string &L, const vector<string> &R){
        prods.emplace_back(L, R);
        nonterminals_set.insert(L);
        for (auto &sym : R) {
            if (!(sym.size()>0 && ( (sym[0] == '\'' && sym.back() == '\'') || // quoted literal
                    (isupper((unsigned char)sym[0])==0 && // starts with lowercase or symbol -> treat as terminal
                    false)
                ))) {
                // We don't rely on quotes in our internal representation.
            }
        }
    };

    // We'll express terminals as plain strings like int, Ident, IntConst, '(' etc.
    // For constructs with repetition (X*), introduce helper nonterminals (right recursive to avoid immediate left recursion)
    // compUnit -> (decl | funcDef)* EOF
    // Transform:
    // compUnit -> compItems EOF
    // compItems -> ε | element compItems
    // element -> decl | funcDef

    // 1. Program -> compUnit
    add("Program", {"compUnit"});

    // compUnit
    add("compUnit", {"compItems", "EOF"}); // EOF is a terminal $ marker

    add("compItems", {}); // epsilon case -> empty vector
    add("compItems", {"element", "compItems"});
    add("element", {"decl"});
    add("element", {"funcDef"});

    // decl -> constDecl | varDecl
    add("decl", {"constDecl"});
    add("decl", {"varDecl"});

    // constDecl -> 'const' bType constDef (',' constDef)* ';'
    // expand (',' constDef)* into constDefList
    add("constDecl", {"const", "bType", "constDefList", ";"});
    add("constDefList", {"constDef"});
    add("constDefList", {"constDef", ",", "constDefList"});
    add("constDef", {"Ident", "=", "constInitVal"});
    add("constInitVal", {"constExp"});

    // varDecl -> bType varDef (',' varDef)* ';'
    add("varDecl", {"bType", "varDefList", ";"});
    add("varDefList", {"varDef"});
    add("varDefList", {"varDef", ",", "varDefList"});
    add("varDef", {"Ident"});
    add("varDef", {"Ident", "=", "initVal"});
    add("initVal", {"exp"});

    // funcDef -> funcType Ident '(' (funcFParams)? ')' block;
    add("funcDef", {"funcType", "Ident", "(", "funcFParamsOpt", ")", "block"});
    add("funcFParamsOpt", {}); // epsilon
    add("funcFParamsOpt", {"funcFParams"});
    add("funcType", {"void"});
    add("funcType", {"int"});
    add("funcFParams", {"funcFParam"});
    add("funcFParams", {"funcFParam", ",", "funcFParams"});
    add("funcFParam", {"bType", "Ident"});

    add("bType", {"int"});
    add("bType", {"float"});

    add("block", {"{", "blockItems", "}"});
    add("blockItems", {}); // epsilon
    add("blockItems", {"blockItem", "blockItems"});
    add("blockItem", {"decl"});
    add("blockItem", {"stmt"});

    // stmt productions (keep as in the grammar)
    // stmt -> lVal '=' exp ';' | (exp)? ';' | block | 'if' '(' cond ')' stmt ('else' stmt)? | 'return' (exp)? ';';
    add("stmt", {"lVal", "=", "exp", ";"});
    add("stmt", {"expOpt", ";"});
    add("expOpt", {}); // epsilon
    add("expOpt", {"exp"});
    add("stmt", {"block"});
    add("stmt", {"if", "(", "cond", ")", "stmt", "elseOpt"});
    add("elseOpt", {}); // epsilon
    add("elseOpt", {"else", "stmt"});
    add("stmt", {"return", "expOpt", ";"});

    add("lVal", {"Ident"});
    add("exp", {"addExp"});
    add("cond", {"lOrExp"});

    add("primaryExp", {"(", "exp", ")"});
    add("primaryExp", {"lVal"});
    add("primaryExp", {"number"});

    add("number", {"IntConst"});
    add("number", {"floatConst"});

    add("unaryExp", {"primaryExp"});
    add("unaryExp", {"Ident", "(", "funcRParamsOpt", ")"});
    add("unaryExp", {"unaryOp", "unaryExp"});
    add("unaryOp", {"+"});
    add("unaryOp", {"-"});
    add("unaryOp", {"!"});

    add("funcRParamsOpt", {}); // epsilon
    add("funcRParamsOpt", {"funcRParams"});
    add("funcRParams", {"funcRParam"});
    add("funcRParams", {"funcRParam", ",", "funcRParams"});
    add("funcRParam", {"exp"});

    // Expression families: For now keep left-recursive forms as given (we'll run left recursion elimination later)
    add("mulExp", {"unaryExp"});
    add("mulExp", {"mulExp", "*", "unaryExp"});
    add("mulExp", {"mulExp", "/", "unaryExp"});
    add("mulExp", {"mulExp", "%", "unaryExp"});

    add("addExp", {"mulExp"});
    add("addExp", {"addExp", "+", "mulExp"});
    add("addExp", {"addExp", "-", "mulExp"});

    add("relExp", {"addExp"});
    add("relExp", {"relExp", "<", "addExp"});
    add("relExp", {"relExp", ">", "addExp"});
    add("relExp", {"relExp", "<=", "addExp"});
    add("relExp", {"relExp", ">=", "addExp"});

    add("eqExp", {"relExp"});
    add("eqExp", {"eqExp", "==", "relExp"});
    add("eqExp", {"eqExp", "!=", "relExp"});

    add("lAndExp", {"eqExp"});
    add("lAndExp", {"lAndExp", "&&", "eqExp"});

    add("lOrExp", {"lAndExp"});
    add("lOrExp", {"lOrExp", "||", "lAndExp"});

    add("constExp", {"addExp"});

    // Now identify terminals vs nonterminals sets
    // We'll populate terminals_set by scanning rhs symbols: symbols not in nonterminals_set are terminals.
    // But we must defer until we've collected all productions. We'll do it after returning from this builder.
}

// Utility: print production
string prod_to_string(const Production &p) {
    string s = p.lhs + " ->";
    if (p.rhs.empty()) s += " ε";
    else {
        for (auto &t : p.rhs) s += " " + t;
    }
    return s;
}

// After building grammar, populate terminals_set and nonterminals_set properly.
void finalize_grammar_sets() {
    // nonterminals_set already contains lhs names (we inserted them when adding).
    // Gather RHS symbols; if symbol equals a nonterminal -> fine; else treat as terminal.
    set<string> rhsyms;
    for (auto &p : prods) {
        for (auto &s : p.rhs) {
            rhsyms.insert(s);
        }
    }
    for (auto &s : rhsyms) {
        if (nonterminals_set.count(s) == 0) {
            terminals_set.insert(s);
        }
    }
    // Remove epsilon (represented by empty rhs) -> no terminal for epsilon
    terminals_set.erase(""); // safe
}

// ---------- LEFT RECURSION ELIMINATION (general algorithm) ----------
void eliminate_left_recursion() {
    // We'll perform the classic algorithm:
    // Order nonterminals (vector)
    vector<string> N;
    for (auto &x : nonterminals_set) N.push_back(x);
    // To keep stable order, sort N by name (deterministic)
    sort(N.begin(), N.end());

    // Build mapping from nonterminal to list of productions indices
    auto getProdIndices = [&](const string &A) {
        vector<int> idx;
        for (int i=0;i<(int)prods.size();++i) if (prods[i].lhs == A) idx.push_back(i);
        return idx;
    };

    for (size_t i=0;i<N.size();++i) {
        string Ai = N[i];
        // For j = 0..i-1, replace occurrences Ai -> Aj α by Aj productions
        for (size_t j=0;j<i;++j) {
            string Aj = N[j];
            // Collect productions for Ai and build new list
            vector<Production> new_ai_prods;
            for (auto &p : prods) {
                if (p.lhs != Ai) continue;
                if (!p.rhs.empty() && p.rhs[0] == Aj) {
                    // Ai -> Aj α , replace by Aj -> β  => Ai -> β α for each Aj->β
                    for (auto &pj : prods) {
                        if (pj.lhs == Aj) {
                            vector<string> newrhs;
                            for (auto &sym : pj.rhs) newrhs.push_back(sym);
                            for (size_t t=1;t<p.rhs.size();++t) newrhs.push_back(p.rhs[t]);
                            new_ai_prods.emplace_back(Ai, newrhs);
                        }
                    }
                } else {
                    // keep original
                    new_ai_prods.push_back(p);
                }
            }
            // Remove old Ai productions and append new ones into global prods
            // Build a new prods list replacing Ai productions:
            vector<Production> updated;
            for (auto &p : prods) if (p.lhs != Ai) updated.push_back(p);
            for (auto &p : new_ai_prods) updated.push_back(p);
            prods.swap(updated);
        }
        // Now eliminate immediate left recursion for Ai, if present
        // Partition Ai productions into left-recursive and non-left-recursive
        vector<Production> Ai_prods;
        for (auto &p : prods) if (p.lhs == Ai) Ai_prods.push_back(p);
        vector<Production> left_rec, non_left;
        for (auto &p : Ai_prods) {
            if (!p.rhs.empty() && p.rhs[0] == Ai) left_rec.push_back(p);
            else non_left.push_back(p);
        }
        if (!left_rec.empty()) {
            // We have immediate left recursion. Create Ai'
            string Aiprime = Ai + "_LR";
            // ensure unique by appending numeric suffix if needed
            int k = 0;
            while (nonterminals_set.count(Aiprime)) {
                ++k;
                Aiprime = Ai + "_LR" + to_string(k);
            }
            nonterminals_set.insert(Aiprime);
            // New productions:
            // For each non_left: Ai -> beta Aiprime
            // For each left_rec: Aiprime -> alpha Aiprime (alpha is rhs tail after Ai)
            // Aiprime -> ε
            vector<Production> newlist;
            // remove old Ai prods from prods
            vector<Production> remaining;
            for (auto &p : prods) if (p.lhs != Ai) remaining.push_back(p);
            // add Ai -> beta A'
            for (auto &p : non_left) {
                vector<string> newrhs = p.rhs;
                if (!(newrhs.size()==1 && newrhs[0]=="")) {
                    newrhs.push_back(Aiprime);
                } else {
                    newrhs.clear();
                    newrhs.push_back(Aiprime);
                }
                remaining.emplace_back(Ai, newrhs);
            }
            // add A' productions
            for (auto &p : left_rec) {
                // p.rhs: Ai alpha...
                vector<string> alpha;
                for (size_t t=1;t<p.rhs.size();++t) alpha.push_back(p.rhs[t]);
                alpha.push_back(Aiprime);
                remaining.emplace_back(Aiprime, alpha);
            }
            // add A' -> ε
            remaining.emplace_back(Aiprime, vector<string>()); // empty rhs = epsilon
            prods.swap(remaining);
        }
    }
    // After elimination we should refresh nonterminals_set and terminals_set if necessary.
    // Build nonterminals_set from all lhs again
    nonterminals_set.clear();
    for (auto &p : prods) nonterminals_set.insert(p.lhs);
    // Build terminals set
    terminals_set.clear();
    set<string> rhsyms;
    for (auto &p : prods) for (auto &s : p.rhs) rhsyms.insert(s);
    for (auto &s : rhsyms) if (nonterminals_set.count(s)==0) terminals_set.insert(s);
    terminals_set.erase(""); // epsilon not terminal
}

// ---------- FIRST / FOLLOW ----------
map<string, set<string>> FIRST, FOLLOW;
bool is_terminal(const string &s) {
    if (s.empty()) return false;
    // We'll treat "EOF" and other literal symbols as terminals.
    return terminals_set.count(s) > 0;
}

void compute_FIRST() {
    FIRST.clear();
    // Init: for all symbols (nonterminals+terminals)
    for (auto &A : nonterminals_set) FIRST[A] = {};
    for (auto &t : terminals_set) FIRST[t] = {t};

    bool changed = true;
    while (changed) {
        changed = false;
        for (auto &p : prods) {
            string A = p.lhs;
            // if rhs empty -> epsilon in FIRST(A): represent epsilon by empty-string marker "" in FIRST
            if (p.rhs.empty()) {
                if (FIRST[A].insert("").second) changed = true;
            } else {
                bool allNullable = true;
                for (auto &X : p.rhs) {
                    // copy FIRST[X] - {ε} into FIRST[A]
                    for (auto &sym : FIRST[X]) {
                        if (sym != "") {
                            if (FIRST[A].insert(sym).second) changed = true;
                        }
                    }
                    if (FIRST[X].count("")==0) { allNullable = false; break; }
                }
                if (allNullable) {
                    if (FIRST[A].insert("").second) changed = true;
                }
            }
        }
    }
}

void compute_FOLLOW() {
    FOLLOW.clear();
    for (auto &A : nonterminals_set) FOLLOW[A] = {};
    // For start symbol Program, include EOF marker "$" (we use "EOF" literal)
    FOLLOW[START_SYMBOL].insert("EOF");

    bool changed = true;
    while (changed) {
        changed = false;
        for (auto &p : prods) {
            string A = p.lhs;
            int n = (int)p.rhs.size();
            for (int i=0;i<n;++i) {
                string B = p.rhs[i];
                if (nonterminals_set.count(B)==0) continue;
                // compute FIRST of the suffix
                set<string> first_of_beta;
                bool allNullable = true;
                for (int j=i+1;j<n;++j) {
                    string X = p.rhs[j];
                    for (auto &sym : FIRST[X]) if (sym!="") first_of_beta.insert(sym);
                    if (FIRST[X].count("")==0) { allNullable = false; break; }
                }
                // add FIRST(beta) - {ε} to FOLLOW(B)
                for (auto &t : first_of_beta) {
                    if (FOLLOW[B].insert(t).second) changed = true;
                }
                if (allNullable) {
                    // add FOLLOW(A) to FOLLOW(B)
                    for (auto &t : FOLLOW[A]) {
                        if (FOLLOW[B].insert(t).second) changed = true;
                    }
                }
            }
        }
    }
}

// ---------- LR(0) ITEMS, CLOSURE, GOTO ----------
struct Item {
    int prod_idx;
    int dot; // position of dot in rhs (0..rhs.size())
    Item() {}
    Item(int p, int d): prod_idx(p), dot(d) {}
    bool operator<(const Item &o) const {
        if (prod_idx != o.prod_idx) return prod_idx < o.prod_idx;
        return dot < o.dot;
    }
    bool operator==(const Item &o) const { return prod_idx==o.prod_idx && dot==o.dot; }
};

using ItemSet = set<Item>;

ItemSet closure(const ItemSet &I) {
    ItemSet C = I;
    bool added = true;
    while (added) {
        added = false;
        vector<Item> to_add;
        for (auto it = C.begin(); it != C.end(); ++it) {
            Item item = *it;
            const Production &p = prods[item.prod_idx];
            if (item.dot < (int)p.rhs.size()) {
                string B = p.rhs[item.dot];
                if (nonterminals_set.count(B)) {
                    // For every production B -> gamma, add [B->.gamma]
                    for (int i=0;i<(int)prods.size();++i) {
                        if (prods[i].lhs == B) {
                            Item newIt(i, 0);
                            if (C.count(newIt)==0) {
                                to_add.push_back(newIt);
                            }
                        }
                    }
                }
            }
        }
        for (auto &it : to_add) {
            if (C.insert(it).second) added = true;
        }
    }
    return C;
}

ItemSet goto_items(const ItemSet &I, const string &X) {
    ItemSet J;
    for (auto &it : I) {
        const Production &p = prods[it.prod_idx];
        if (it.dot < (int)p.rhs.size() && p.rhs[it.dot] == X) {
            J.insert(Item(it.prod_idx, it.dot+1));
        }
    }
    return closure(J);
}

// canonical collection
vector<ItemSet> C_collection;
map<ItemSet,int> itemset_index;

void build_LR0_collection() {
    C_collection.clear();
    itemset_index.clear();
    // Augment grammar with S' -> Program
    // We'll insert augmented production at prods[0]
    Production aug("S'", {START_SYMBOL});
    // place augmented at front
    prods.insert(prods.begin(), aug);
    // rebuild nonterminals_set
    nonterminals_set.insert("S'");
    // recompute terminals set
    terminals_set.clear();
    set<string> rhsyms;
    for (auto &p : prods) for (auto &s : p.rhs) if (!s.empty()) rhsyms.insert(s);
    for (auto &s : rhsyms) if (nonterminals_set.count(s)==0) terminals_set.insert(s);

    ItemSet I0;
    I0.insert(Item(0,0)); // S'->.Program
    ItemSet C0 = closure(I0);
    C_collection.push_back(C0);
    itemset_index[C0] = 0;

    bool changed = true;
    while (changed) {
        changed = false;
        for (size_t i=0;i<C_collection.size();++i) {
            ItemSet I = C_collection[i];
            // For all grammar symbols X (terminals+nonterminals), compute goto
            set<string> symbols;
            for (auto &it : I) {
                auto &p = prods[it.prod_idx];
                if (it.dot < (int)p.rhs.size()) symbols.insert(p.rhs[it.dot]);
            }
            for (auto &X : symbols) {
                ItemSet g = goto_items(I, X);
                if (g.empty()) continue;
                if (!itemset_index.count(g)) {
                    int idx = (int)C_collection.size();
                    C_collection.push_back(g);
                    itemset_index[g] = idx;
                    changed = true;
                }
            }
        }
    }
}

// ACTION and GOTO tables
// ACTION: map state -> terminal -> action (shift to state j, reduce by prod r, accept, or error)
// GOTO: map state -> nonterminal -> next state
struct Action {
    enum Type { NONE, SHIFT, REDUCE, ACCEPT, ERR } type = NONE;
    int val = -1; // shift: next state; reduce: production index; accept: -
    Action(): type(NONE), val(-1) {}
};

vector< unordered_map<string, Action> > ACTION;
vector< unordered_map<string, int> > GOTO;

void build_SLR_table() {
    int N = (int)C_collection.size();
    ACTION.assign(N, {});
    GOTO.assign(N, {});
    // For each state i and each item
    for (int i=0;i<N;++i) {
        auto &I = C_collection[i];
        for (auto &it : I) {
            const Production &p = prods[it.prod_idx];
            // If A -> α . a β and a is terminal, then ACTION[i,a] = shift goto(i,a)
            if (it.dot < (int)p.rhs.size()) {
                string a = p.rhs[it.dot];
                if (is_terminal(a)) {
                    ItemSet g = goto_items(I, a);
                    if (itemset_index.count(g)) {
                        int j = itemset_index[g];
                        Action act; act.type = Action::SHIFT; act.val = j;
                        ACTION[i][a] = act;
                    }
                } else {
                    // nonterminal: fill GOTO
                    ItemSet g = goto_items(I, a);
                    if (itemset_index.count(g)) {
                        int j = itemset_index[g];
                        GOTO[i][a] = j;
                    }
                }
            } else {
                // dot at end: A -> α .
                if (p.lhs == "S'") {
                    // Accept on EOF
                    Action acc; acc.type = Action::ACCEPT;
                    ACTION[i]["EOF"] = acc;
                } else {
                    // For each terminal a in FOLLOW(A), ACTION[i,a] = reduce by production it.prod_idx
                    int prod_index = it.prod_idx;
                    string A = p.lhs;
                    for (auto &a : FOLLOW[A]) {
                        Action r; r.type = Action::REDUCE; r.val = prod_index;
                        // Insert if not set. If conflict arises, SLR grammar might be ambiguous.
                        if (ACTION[i].count(a)) {
                            // conflict -> we'll keep the existing one (but also print warning)
                            // To be safe, if it's different, mark error (we do not crash)
                            if (!(ACTION[i][a].type == r.type && ACTION[i][a].val == r.val)) {
                                // leave existing (but print)
                                // cerr << "SLR table conflict at state " << i << " on symbol " << a << "\n";
                            }
                        } else {
                            ACTION[i][a] = r;
                        }
                    }
                }
            }
        }
    }
}

// ---------- Token file parsing and mapping to terminals ----------
string map_lexeme_to_terminal(const string &lexeme, const string &token_meta) {
    // token_meta is contents inside <...> e.g. KW,1 or IDN,a or OP,11 or SE,24 or INT,10 or FLOAT,1
    // prefer lexeme text itself if it's a known literal (like "int","(", "==", "+", etc.)
    string s = lexeme;
    if (s == "int" || s == "void" || s == "return" || s == "const" || s == "main" || s == "float" || s == "if" || s == "else") return s;
    // parentheses & punctuation & operators: treat lexeme as terminal if single/multi-symbol
    static const unordered_set<string> known_syms = {
        "(", ")", "{", "}", ";", ",",
        "+", "-", "*", "/", "%", "=", ">", "<", "==", "<=", ">=", "!=", "&&", "||"
    };
    if (known_syms.count(s)) return s;
    // If lexeme looks like integer (digits) -> IntConst
    bool all_digits = !s.empty() && all_of(s.begin(), s.end(), [](char c){ return isdigit((unsigned char)c); });
    bool is_float_pattern = false;
    if (!all_digits) {
        // simple float detection: digits '.' digits
        size_t dot = s.find('.');
        if (dot != string::npos) {
            string a = s.substr(0,dot), b = s.substr(dot+1);
            if (!a.empty() && !b.empty() && all_of(a.begin(), a.end(), [](char c){return isdigit((unsigned char)c);}) && all_of(b.begin(), b.end(), [](char c){return isdigit((unsigned char)c);}))
                is_float_pattern = true;
        }
    }
    if (all_digits) return "IntConst";
    if (is_float_pattern) return "floatConst";
    // If token_meta indicates IDN -> Ident
    string meta = token_meta;
    if (!meta.empty()) {
        // split by comma
        auto pos = meta.find(',');
        string kind = meta;
        if (pos!=string::npos) kind = meta.substr(0,pos);
        if (kind == "IDN") return "Ident";
        if (kind == "INT") return "IntConst";
        if (kind == "FLOAT") return "floatConst";
        if (kind == "KW") {
            // attribute gives code (but lexeme already handled)
            return s;
        }
        if (kind == "OP" || kind == "SE") {
            return s;
        }
    }
    // default: if lexeme looks like identifier (starting alpha or '_'), map to Ident
    if (!s.empty() && (isalpha((unsigned char)s[0]) || s[0]=='_')) return "Ident";
    // fallback: treat lexeme itself as terminal
    return s;
}

vector<string> read_tokens_from_file(const string &path) {
    vector<string> res;
    ifstream ifs(path);
    if (!ifs) {
        cerr << "错误：无法打开 token 文件: " << path << "\n";
        return res;
    }
    string line;
    while (getline(ifs, line)) {
        if (trim(line).empty()) continue;
        // split by tab. first part lexeme, second contains <...>
        string lexeme;
        string meta;
        size_t tabpos = line.find('\t');
        if (tabpos == string::npos) {
            // try splitting by spaces
            stringstream ss(line);
            ss >> lexeme;
            string rest; getline(ss, rest);
            meta = trim(rest);
        } else {
            lexeme = trim(line.substr(0, tabpos));
            meta = trim(line.substr(tabpos+1));
        }
        // meta trim angle brackets
        string inner;
        if (!meta.empty()) {
            size_t a = meta.find('<');
            size_t b = meta.rfind('>');
            if (a!=string::npos && b!=string::npos && b>a) inner = meta.substr(a+1, b-a-1);
            else inner = meta;
        }
        string term = map_lexeme_to_terminal(lexeme, inner);
        if (!term.empty()) res.push_back(term);
    }
    // Append EOF terminal
    res.push_back("EOF");
    return res;
}

// ---------- Parser runtime ----------
struct StackEntry {
    int state;
    string symbol; // symbol pushed (for debugging)
    StackEntry(int s=0,const string &sym=""): state(s), symbol(sym) {}
};

void run_parser(const vector<string> &input_tokens, ostream &out) {
    // stack initially contains state 0
    vector<int> state_stack;
    vector<string> sym_stack;
    state_stack.push_back(0);
    int ip = 0;
    int step = 1;

    auto top_state = [&](){ return state_stack.back(); };
    while (true) {
        int st = top_state();
        string a = (ip < (int)input_tokens.size() ? input_tokens[ip] : "EOF");
        // determine action
        Action act;
        if (ACTION[st].count(a)) act = ACTION[st][a];
        else {
            // action not found: error
            out << step << "\t";
            // print stack symbols (top-right)
            string stacks;
            for (size_t i=0;i<sym_stack.size();++i) {
                if (i) stacks += " ";
                stacks += sym_stack[i];
            }
            if (stacks.empty()) stacks = to_string(state_stack.back());
            out << stacks << "#" << a << "\t";
            out << "error\n";
            return;
        }
        if (act.type == Action::SHIFT) {
            // move / shift
            int j = act.val;
            // push symbol a and state j
            sym_stack.push_back(a);
            state_stack.push_back(j);
            out << step << "\t";
            // show stack symbol sequence (space separated)
            string stacks;
            for (size_t i=0;i<sym_stack.size();++i) {
                if (i) stacks += " ";
                stacks += sym_stack[i];
            }
            if (stacks.empty()) stacks = to_string(state_stack.back());
            out << stacks << "#" << (ip<(int)input_tokens.size() ? input_tokens[ip] : "EOF") << "\t";
            out << "move\n";
            ip++;
            step++;
            continue;
        } else if (act.type == Action::REDUCE) {
            int pidx = act.val;
            const Production &p = prods[pidx];
            // pop |rhs| symbols
            int k = (int)p.rhs.size();
            for (int i=0;i<k;++i) {
                if (!sym_stack.empty()) sym_stack.pop_back();
                if (!state_stack.empty()) state_stack.pop_back();
            }
            // new top state
            int t = state_stack.back();
            // push LHS symbol
            sym_stack.push_back(p.lhs);
            // goto[t, p.lhs]
            if (GOTO[t].count(p.lhs)==0) {
                out << step << "\t";
                string stacks;
                for (size_t i=0;i<sym_stack.size();++i) {
                    if (i) stacks += " ";
                    stacks += sym_stack[i];
                }
                out << stacks << "#" << (ip<(int)input_tokens.size() ? input_tokens[ip] : "EOF") << "\t";
                out << "error\n";
                return;
            }
            int newstate = GOTO[t][p.lhs];
            state_stack.push_back(newstate);
            out << step << "\t";
            string stacks;
            for (size_t i=0;i<sym_stack.size();++i) {
                if (i) stacks += " ";
                stacks += sym_stack[i];
            }
            out << stacks << "#" << (ip<(int)input_tokens.size() ? input_tokens[ip] : "EOF") << "\t";
            out << "reduction (" << p.lhs << " ->";
            if (p.rhs.empty()) out << " ε";
            else {
                for (auto &x : p.rhs) out << " " << x;
            }
            out << ")\n";
            step++;
            continue;
        } else if (act.type == Action::ACCEPT) {
            out << step << "\t";
            string stacks;
            for (size_t i=0;i<sym_stack.size();++i) {
                if (i) stacks += " ";
                stacks += sym_stack[i];
            }
            out << stacks << "#" << (ip<(int)input_tokens.size() ? input_tokens[ip] : "EOF") << "\t";
            out << "accept\n";
            return;
        } else {
            out << step << "\t";
            string stacks;
            for (size_t i=0;i<sym_stack.size();++i) {
                if (i) stacks += " ";
                stacks += sym_stack[i];
            }
            out << stacks << "#" << (ip<(int)input_tokens.size() ? input_tokens[ip] : "EOF") << "\t";
            out << "error\n";
            return;
        }
    }
}

// ---------- Main ----------
int main(int argc, char** argv) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    if (!(argc==2 || argc==3)) {
        cerr << "用法: " << argv[0] << " /abs/path/to/token_file [ /abs/path/to/output_file ]\n";
        return 1;
    }
    string token_path = argv[1];
    string out_path;
    bool use_out = false;
    if (argc==3) { out_path = argv[2]; use_out = true; }

    // 1) Build grammar
    build_initial_grammar();
    finalize_grammar_sets();
    // 2) Eliminate left recursion (general)
    eliminate_left_recursion();
    // make sure START_SYMBOL exists (Program)
    if (nonterminals_set.count(START_SYMBOL)==0) {
        cerr << "Grammar missing start symbol Program\n";
        return 1;
    }

    // 3) Compute FIRST and FOLLOW
    compute_FIRST();
    compute_FOLLOW();

    // 4) Build LR(0) collection and SLR table
    build_LR0_collection();
    compute_FIRST(); // not strictly needed again
    compute_FOLLOW();
    build_SLR_table();

    // 5) Read tokens
    vector<string> tokens = read_tokens_from_file(token_path);
    if (tokens.empty()) {
        cerr << "没有读取到 token 或文件为空\n";
        return 1;
    }

    // 6) Run parser and output trace
    if (use_out) {
        ofstream ofs(out_path);
        if (!ofs) {
            cerr << "无法写入输出文件: " << out_path << "\n";
            return 1;
        }
        run_parser(tokens, ofs);
        ofs.close();
        cout << "解析完成，输出写入: " << out_path << "\n";
    } else {
        run_parser(tokens, cout);
    }
    return 0;
}
