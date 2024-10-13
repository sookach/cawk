#include "cawk/Frontend/Instance.h"
#include "cawk/AST/ASTPrinter.h"
#include "cawk/CodeGen/CodeGen.h"
#include "cawk/Exec/Exec.h"
#include "cawk/Parse/Parser.h"
#include "cawk/Transforms/DeadCodeElimination.h"

using namespace cawk;

int Instance::execute() {
  if (!CmdLine.parse())
    return EXIT_FAILURE;
  auto Source = [this] {
    if (CmdLine.getSourceArg() == -1) {
      std::string Source;
      for (std::string Progfile : CmdLine.getProgFiles()) {
        InputFile File(Progfile);
        Source += File.toString();
      }
      return Source;
    }
    return std::string(CmdLine.getArgv()[CmdLine.getSourceArg()]);
  }();
  Lexer Lex(Source, Diags);
  Parser Parse(Lex, Diags);
  auto ParseResult = Parse.parse();
  if (!ParseResult.isValid()) {
    Diags.printErrors(Source);
    return EXIT_FAILURE;
  }
  CodeGen Gen;
  auto [Code, Constants] =
      Gen.emitByteCode(static_cast<TranslationUnitDecl *>(ParseResult.get()));
  ExecutionEngine Exec(Code, Constants);
  if (CmdLine.getAssignments().contains("ir")) {
    Exec.dumpCode();
  }
  auto Result = Exec.run();
  outs().printf("Result: %f\n", Result).flush();

  auto Functions = Parse.getFunctions();
  return EXIT_SUCCESS;
}