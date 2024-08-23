#include "Frontend/Instance.h"
#include "AST/ASTPrinter.h"
#include "AST/ASTVisitor.h"
#include "Exec/Exec.h"
#include "Lexer/Lexer.h"
#include "Parse/Parser.h"
#include "Sema/Sema.h"

using namespace cawk;

int Instance::execute() {
  Lexer Lex(Source, Diags);
  Parser Parse(Lex, Diags);
  ASTPrinter Printer;
  auto ParseResult = Parse.parse();
  if (!ParseResult.isValid()) {
    Diags.printErrors(Source);
    return EXIT_FAILURE;
  }
  Sema Semantic(Diags);
  Semantic.check(ParseResult.getAs<TranslationUnitDecl>());
  //   Printer.traverse(TLU);
  Diags.printErrors(Source);
  return EXIT_SUCCESS;
}