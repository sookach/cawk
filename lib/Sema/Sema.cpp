#include "Sema/Sema.h"
#include "AST/ASTPrinter.h"
#include "Sema/SemaControlFlow.h"
#include "Sema/SemaLValue.h"
#include "Sema/SemaType.h"
#include "Sema/SymbolResolver.h"

#include <cstdio>

using namespace cawk;

bool Sema::check(TranslationUnitDecl *T) {
  SemaType TypeSema(Diags);
  SemaLValue LValueSema(Diags);
  SemaControlFlow ControlFlowSema(Diags);
  SymbolResolver Resolver(Diags);
  ASTPrinter Printer;
  Printer.traverse(T);

  std::printf("SymbolResolver: %s\n", Resolver.check(T) ? "pass" : "fail");
  std::printf("SemaType: %s\n", TypeSema.check(T) ? "pass" : "fail");
  std::printf("SemaLValue: %s\n", LValueSema.check(T) ? "pass" : "fail");
  std::printf("SemaControlFlow: %s\n",
              ControlFlowSema.check(T) ? "pass" : "fail");
  return true;
}