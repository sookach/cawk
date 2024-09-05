#include "Frontend/Instance.h"
#include "AST/ASTPrinter.h"
#include "AST/ASTVisitor.h"
#include "Exec/Exec.h"
#include "Lexer/Lexer.h"
#include "Parse/Parser.h"
#include "Sema/Sema.h"
#include "Sema/SymbolResolver.h"

#include <exception>

using namespace cawk;

int Instance::execute() {
  if (!CmdLine.parse())
    return EXIT_FAILURE;
  auto Source = [this] {
    if (CmdLine.getSourceArg() == -1) {
      std::string Source;
      for (std::string Progfile : CmdLine.getProfFiles()) {
        InputFile File(Progfile);
        Source += File.toString();
      }
      return Source;
    }
    return std::string(CmdLine.getArgv()[CmdLine.getSourceArg()]);
  }();
  Lexer Lex(Source, Diags);
  Parser Parse(Lex, Diags);
  ASTPrinter Printer;
  auto ParseResult = Parse.parse();
  if (!ParseResult.isValid()) {
    Diags.printErrors(Source);
    return EXIT_FAILURE;
  }
  Sema Semantic(Diags);
  // Printer.traverse(ParseResult.getAs<TranslationUnitDecl>());
  Semantic.check(ParseResult.getAs<TranslationUnitDecl>());
  Diags.printErrors(Source);
  SymbolResolver Resolver(Diags);
  Resolver.check(ParseResult.getAs<TranslationUnitDecl>());
  //   Printer.traverse(ParseResult.getAs<TranslationUnitDecl>());
  auto Globals = Resolver.getGlobals();
  //   Globals["ARGC"]->setValue(Value(std::size(CmdLine.getARGV())));
  //   for (int I = 0; auto &Arg : CmdLine.getARGV()) {
  // Globals["ARGV"]->getValue()->operator[](Value(I++))->setValue(Value(Arg));
  //   }
  Exec Executor(Diags, ParseResult.getAs<TranslationUnitDecl>(), {},
                Resolver.getGlobals());
  Executor();
  return EXIT_SUCCESS;
}