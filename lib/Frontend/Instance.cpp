#include "Frontend/Instance.h"
#include "Exec/Exec.h"
#include "Lexer/Lexer.h"
#include "Parse/Parser.h"
#include "Sema/SemaDecl.h"
#include "Sema/SemaType.h"

using namespace cawk;

int Instance::execute() {
  Lexer Lex(Source);
  Parser Parse(Lex);
  auto TLU = Parse.parse();
  Exec::load(TLU, {});
  Exec::exec();
  return EXIT_SUCCESS;
}