# Compilation Project Quickstart / 项目快速上手

This project contains three standalone stages of a tiny SysY-style compiler: lexer, parser, and IR generator. Each stage can be built and executed from the repository root (`/home/username/compilation`). Below are the exact commands and runtime options in both English and Chinese.

## 1. Lexical Analyzer / 词法分析器

**Build / 编译**
```bash
cd /home/username/compilation
g++ -std=c++17 -Wall -Wextra -pedantic src/lexical_analyzer/lexical_analyzer.cpp -o lexical_analyzer
```

**Run / 运行**
```bash
./lexical_analyzer [input_dir] [output_dir]
```
- Default input directory (默认输入目录): `test_sources/lex_sources/lex_test`
- Default output directory (默认输出目录): `test_sources/lex_sources/lex_output`
- The program scans every `.sy` file under the input directory and writes `<name>.out` token lists into the output directory.
- 程序会遍历输入目录下的所有 `.sy` 文件，将词法分析结果写入输出目录中的 `<name>.out` 文件。

## 2. Syntactic Analyzer / 语法分析器

**Build / 编译**
```bash
cd /home/username/compilation
g++ -std=c++17 -Wall -Wextra -pedantic src/syntactic_analyzer/syntactic_analyzer.cpp -o syntactic_analyzer
```

**Run / 运行**
```bash
./syntactic_analyzer [token_dir] [output_dir]
```
- Default token input (默认输入): `test_sources/syntax_sources/syn_test`
- Default outputs (默认输出): `test_sources/syntax_sources/syn_output`
- For each `.sy` file, the parser reads the token stream produced by the lexer, emits a shift/reduce trace `<name>.out`, and dumps an AST `<name>.ast`.
- 语法分析器会读取 `.sy` 文件（由词法分析器生成的 token 序列），输出移进/规约过程 `<name>.out` 以及抽象语法树 `<name>.ast`。

## 3. Intermediate Code Generator / 中间代码生成器

**Build / 编译**
```bash
cd /home/username/compilation
g++ -std=c++17 -Wall -Wextra -pedantic \
    src/intermediate_code_generator/intermediate_code_generator.cpp \
    compiler_ir-master/src/*.cpp \
    -Isrc -Icompiler_ir-master/include \
    -o intermediate_code_generator
```

**Run / 运行**
```bash
./intermediate_code_generator [ast_dir] [output_dir]
```
- Default AST input (默认 AST 输入): `test_sources/intermediate_sources/inter_test`
- Default LLVM IR output (默认 IR 输出): `test_sources/intermediate_sources/inter_output`
- The generator consumes AST dumps from the parser, traverses them, and emits LLVM-like `.ll` modules.
- 中间代码生成器读取语法分析器输出的 AST，遍历语法树并生成 LLVM IR `.ll` 文件。

## Notes / 注意事项
- All commands assume a Linux bash shell with `g++` 11+.
- If you provide custom directories on the command line, ensure they already contain the expected `.sy` or `.ast` files; results will be written alongside in the specified output folder.
- 测试人员可按需替换输入目录以验证更多案例，但请保证输出目录存在或可创建。
