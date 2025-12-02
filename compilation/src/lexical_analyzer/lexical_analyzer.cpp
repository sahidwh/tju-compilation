// lexical_analyzer.cpp
// 等价移植自用户上传的 lexer.py（已读取并参照）。
// 支持：./lexical_analyzer
//       ./lexical_analyzer /abs/path/to/input
//       ./lexical_analyzer /abs/path/to/input /abs/path/to/output

#include <bits/stdc++.h>
using namespace std;

struct TokenSpec {
    string regex;   // 原始正则（ECMAScript 风格）
    string type;    // 'KEYWORD', 'IDN', 'INT', 'FLOAT', 'SYMBOL', 'WHITESPACE'
    string desc;    // 描述（未直接使用，仅保留）
};

static const vector<TokenSpec> TOKEN_SPECS = {
    { R"(int)", "KEYWORD", "int" },
    { R"(void)", "KEYWORD", "void" },
    { R"(return)", "KEYWORD", "return" },
    { R"(const)", "KEYWORD", "const" },
    { R"(main)", "KEYWORD", "main" },
    { R"(float)", "KEYWORD", "float" },
    { R"(if)", "KEYWORD", "if" },
    { R"(else)", "KEYWORD", "else" },
    { R"(&&)", "SYMBOL", "逻辑与" },
    { R"(\|\|)", "SYMBOL", "逻辑或" },
    { R"(==)", "SYMBOL", "等于" },
    { R"(<=)", "SYMBOL", "小于等于" },
    { R"(>=)", "SYMBOL", "大于等于" },
    { R"(!=)", "SYMBOL", "不等于" },
    { R"([0-9]+\.[0-9]+)", "FLOAT", "浮点数" },
    { R"([0-9]+\.)", "FLOAT", "浮点数（尾点）" },
    { R"(\.[0-9]+)", "FLOAT", "浮点数（首点）" },
    { R"([0-9]+)", "INT", "整数" },
    { R"([+*/%=<>(){};,-])", "SYMBOL", "符号" },
    { R"([a-zA-Z_][a-zA-Z_0-9]*)", "IDN", "标识符" },
    { R"([ \t\n\r]+)", "WHITESPACE", "空白符" }
};

// 关键字、运算符、分隔符映射（与原 Python 保持一致）
static const unordered_map<string,int> KEYWORD_MAP = {
    {"void",2}, {"int",1}, {"return",3}, {"const",4},
    {"main",5}, {"float",6}, {"if",7}, {"else",8}
};
static const unordered_map<string,int> OPERATOR_MAP = {
    {"==",17}, {"!=",20}, {"<=",18}, {">=",19}, {"&&",21}, {"||",22},
    {"+",9}, {"-",10}, {"*",11}, {"/",12}, {"%",13}, {"=",14}, {">",15}, {"<",16}
};
static const unordered_map<char,int> SEPARATOR_MAP = {
    {'(',23}, {')',24}, {'{',25}, {'}',26}, {';',27}, {',',28}
};

static inline string to_lower_s(const string &s) {
    string r = s;
    for (char &c : r) c = (char)tolower((unsigned char)c);
    return r;
}

int main(int argc, char** argv) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    // 输入/输出处理
    string source_code;
    bool use_input_file = false;
    bool use_output_file = false;
    string input_path, output_path;

    if (argc == 3) {
        input_path = argv[1];
        output_path = argv[2];
        use_input_file = true;
        use_output_file = true;
    } else if (argc == 2) {
        input_path = argv[1];
        use_input_file = true;
        use_output_file = false;
    } else if (argc == 1) {
        use_input_file = false;
        use_output_file = false;
    } else {
        cerr << "用法：./lexical_analyzer [源文件路径] [输出文件路径]\n";
        return 1;
    }

    if (use_input_file) {
        ifstream ifs(input_path, ios::in);
        if (!ifs) {
            cerr << "错误：找不到文件 '" << input_path << "'\n";
            return 1;
        }
        ostringstream ss;
        ss << ifs.rdbuf();
        source_code = ss.str();
    } else {
        // 从 stdin 读取全部
        {
            ostringstream ss;
            ss << cin.rdbuf();
            source_code = ss.str();
        }
    }

    // 准备输出流
    ofstream ofs;
    ostream* out = &cout;
    if (use_output_file) {
        ofs.open(output_path, ios::out);
        if (!ofs) {
            cerr << "错误：无法写入输出文件 '" << output_path << "'\n";
            return 1;
        }
        out = &ofs;
    }

    // 预构建 std::regex 列表（在模式前加 ^ 保证从当前位置开始匹配）
    vector<regex> regex_list;
    regex_list.reserve(TOKEN_SPECS.size());
    for (const auto &ts : TOKEN_SPECS) {
        string pat = "^(" + ts.regex + ")";
        try {
            regex r(pat, std::regex::ECMAScript);
            regex_list.push_back(r);
        } catch (const std::regex_error &e) {
            cerr << "正则编译错误: " << e.what() << "  pattern=" << pat << "\n";
            return 1;
        }
    }

    // 开始词法分析（按 Python 程序的行为：跳过空白、最长匹配、规则优先）
    vector<pair<string,string>> tokens;
    int line = 1;
    int col = 1;
    size_t curr_pos = 0;
    size_t code_len = source_code.size();

    while (curr_pos < code_len) {
        char c = source_code[curr_pos];
        // 跳过空白字符（与原 Python 行/列更新一致）
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            if (c == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
            curr_pos += 1;
            continue;
        }

        string remaining = source_code.substr(curr_pos);

        // 在当前位置对所有 token 规则做 anchored 匹配，选择最长的匹配；长度相同取规则优先级更高（索引小）
        size_t best_len = 0;
        int best_idx = -1;
        string best_match;

        for (size_t i = 0; i < regex_list.size(); ++i) {
            smatch m;
            // 使用 regex_search（由于 pattern 带 ^，它只会在开头匹配）
            if (regex_search(remaining, m, regex_list[i]) && m.size() >= 1) {
                string matched = m.str(1);
                if (matched.size() > best_len) {
                    best_len = matched.size();
                    best_idx = (int)i;
                    best_match = matched;
                } else if (matched.size() == best_len && best_idx != -1) {
                    // 长度相同则保持较小的 rule idx（已满足因为按顺序遍历）
                    // 这里不需要额外处理，因为我们只替换当 found > best_len。
                } else if (best_idx == -1 && matched.size() > 0) {
                    // 首次发现
                    best_len = matched.size();
                    best_idx = (int)i;
                    best_match = matched;
                }
            }
        }

        if (best_idx != -1 && best_len > 0) {
            string lexeme = best_match;
            string token_type = TOKEN_SPECS[best_idx].type;

            // 记录起始行列（用于错误报告时）
            int start_line = line;
            int start_col = col;

            // 更新行列（与 Python 逻辑一致：逐字符更新）
            for (char ch : lexeme) {
                if (ch == '\n') {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            }

            // 分类处理（与 Python 行为一致）
            if (token_type == "KEYWORD" || (token_type == "IDN" && KEYWORD_MAP.count(to_lower_s(lexeme)))) {
                string lw = to_lower_s(lexeme);
                int kw_code = KEYWORD_MAP.at(lw);
                (*out) << lexeme << "\t" << "<KW," << kw_code << ">\n";
            } else if (token_type == "IDN") {
                (*out) << lexeme << "\t" << "<IDN," << lexeme << ">\n";
            } else if (token_type == "INT") {
                (*out) << lexeme << "\t" << "<INT," << lexeme << ">\n";
            } else if (token_type == "FLOAT") {
                (*out) << lexeme << "\t" << "<FLOAT," << lexeme << ">\n";
            } else if (token_type == "SYMBOL") {
                // 符号可能是操作符或分隔符，或非法符号
                if (OPERATOR_MAP.count(lexeme)) {
                    int op_code = OPERATOR_MAP.at(lexeme);
                    (*out) << lexeme << "\t" << "<OP," << op_code << ">\n";
                } else if (lexeme.size() == 1 && SEPARATOR_MAP.count(lexeme[0])) {
                    int se_code = SEPARATOR_MAP.at(lexeme[0]);
                    (*out) << lexeme << "\t" << "<SE," << se_code << ">\n";
                } else {
                    // 未知的符号视作错误，报告起始位置（与Python类似）
                    (*out) << lexeme << "\t" << "<ERROR," << start_line << "," << start_col << ">\n";
                }
            } else {
                // 对于 WHITESPACE（通常之前已经跳过了），或其他未识别类型
                if (token_type == "WHITESPACE") {
                    // 忽略（Python 在主循环一开始已跳过空白）
                } else {
                    // 作为一般 token 输出（防御性）
                    (*out) << lexeme << "\t" << "<" << token_type << "," << lexeme << ">\n";
                }
            }

            curr_pos += best_len;
        } else {
            // 没有匹配到任何 token：单字符错误
            char invalid_char = source_code[curr_pos];
            (*out) << invalid_char << "\t" << "<ERROR," << line << "," << col << ">\n";
            if (invalid_char == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
            curr_pos += 1;
        }
    }

    // flush and close if necessary
    if (ofs.is_open()) ofs.close();
    return 0;
}
