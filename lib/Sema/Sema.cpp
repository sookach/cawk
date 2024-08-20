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
  bool Result = true;

  Result = Resolver.check(T);
  //   std::printf("SymbolResolver: %s\n", Result ? "pass" : "fail");

  Result = TypeSema.check(T);
  //   std::printf("SemaType: %s\n", Result ? "pass" : "fail");

  Result = LValueSema.check(T);
  //   std::printf("SemaLValue: %s\n", Result ? "pass" : "fail");

  Result = ControlFlowSema.check(T);
  //   std::printf("SemaControlFlow: %s\n", Result ? "pass" : "fail");

  return true;
}