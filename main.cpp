#include "Exec/Exec.h"
#include "Lexer/Lexer.h"
#include "Parse/Parser.h"

#include <fstream>

int main(int argc, char **argv) {
  std::ifstream File("/Users/suk/dev/cawk/main.awk");
  std::string Source((std::istreambuf_iterator<char>(File)),
                     std::istreambuf_iterator<char>());
  cawk::Lexer Lex(Source);
  cawk::Parser Parse(Lex);
  auto AST = Parse.parse();
  cawk::Exec Exe;
  Exe.run(AST);
}
