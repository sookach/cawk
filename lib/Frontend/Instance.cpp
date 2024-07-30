#include "Frontend/Instance.h"
#include "Exec/Exec.h"
#include "Lexer/Lexer.h"
#include "Parse/Parser.h"

using namespace cawk;

int Instance::execute() {
  Lexer Lex(Source);
  Parser Parse(Lex);
  Exec::load(Parse.parse(), {});
  Exec::exec();
  return EXIT_SUCCESS;
}