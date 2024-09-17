#include "Frontend/Instance.h"
#include "AST/ASTPrinter.h"
#include "CodeGen/CodeGen.h"
#include "Exec/Exec.h"
#include "Parse/Parser.h"
#include "Transforms/DeadCodeElimination.h"

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
  //   Printer.traverse(ParseResult.getAs<TranslationUnitDecl>());
  DCEPass DCE;
  DCE.traverse(ParseResult.getAs<TranslationUnitDecl>());
  //   Printer.traverse(ParseResult.getAs<TranslationUnitDecl>());
  Diags.printErrors(Source);
  //   Printer.traverse(ParseResult.getAs<TranslationUnitDecl>());
  auto Functions = Parse.getFunctions();
  return EXIT_SUCCESS;
}