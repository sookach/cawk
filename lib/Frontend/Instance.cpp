#include "Frontend/Instance.h"
#include "AST/ASTPrinter.h"
#include "AST/ASTVisitor.h"
#include "Exec/Exec.h"
#include "Lexer/Lexer.h"
#include "Parse/Parser.h"
#include "Sema/Sema.h"
#include "Sema/SymbolResolver.h"

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
  Printer.traverse(ParseResult.getAs<TranslationUnitDecl>());
  Semantic.check(ParseResult.getAs<TranslationUnitDecl>());
  Diags.printErrors(Source);
  SymbolResolver Resolver(Diags);
  Resolver.check(ParseResult.getAs<TranslationUnitDecl>());
  Exec::load(ParseResult.getAs<TranslationUnitDecl>(), {});
  Exec::exec();
  return EXIT_SUCCESS;
}