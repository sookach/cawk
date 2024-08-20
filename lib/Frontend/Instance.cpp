#include "Frontend/Instance.h"
#include "AST/ASTPrinter.h"
#include "AST/ASTVisitor.h"
#include "Exec/Exec.h"
#include "Lexer/Lexer.h"
#include "Parse/Parser.h"
#include "Sema/Sema.h"

using namespace cawk;

int Instance::execute() {
  Lexer Lex(Source);
  Parser Parse(Lex);
  auto TLU = Parse.parse();
  ASTPrinter Printer;
  Sema(Diags).check(TLU);
  return EXIT_SUCCESS;
}