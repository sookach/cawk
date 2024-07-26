#include "Exec/Exec.h"
#include "Exec/IO.h"
#include "Lexer/Lexer.h"
#include "Parse/Parser.h"

#include <filesystem>

static int cawk_main(int Argc, char **Argv) {
  assert(Argc == 2);
  auto Source = [Argv]() {
    if (std::filesystem::exists(Argv[1])) {
      cawk::InputFile Source(Argv[1]);
      return Source.toString();
    }
    return std::string(Argv[1]);
  }();
  cawk::Lexer Lex(Source);
  cawk::Parser Parse(Lex);
  auto AST = Parse.parse();
  cawk::Exec::load(AST, {});
  cawk::Exec::exec();
  return EXIT_SUCCESS;
}

int main(int argc, char **argv) { return cawk_main(argc, argv); }
