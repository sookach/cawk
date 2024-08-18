#include "Sema/Sema.h"
#include "AST/ASTPrinter.h"

#include <cstdio>

using namespace cawk;

bool Sema::check(TranslationUnitDecl *T) {
  ASTPrinter Printer;
  Printer.traverse(T);
  std::printf("SymbolResolver: %s\n", Resolver.check(T) ? "pass" : "fail");
  Printer.traverse(T);
  std::printf("SemaType: %s\n", TypeSema.check(T) ? "pass" : "fail");
  std::printf("SemaLValue: %s\n", LValueSema.check(T) ? "pass" : "fail");
  std::printf("SemaControlFlow: %s\n",
              ControlFlowSema.check(T) ? "pass" : "fail");
  return true;
}