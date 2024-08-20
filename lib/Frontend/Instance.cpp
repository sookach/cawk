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
  ASTPrinter Printer;
  auto TLU = Parse.parse();
  Sema Semantic(Diags);
  Semantic.check(TLU);
  Printer.traverse(TLU);
  Diags.printErrors(Source);
  return EXIT_SUCCESS;
}