#include "Sema/Sema.h"
#include "AST/ASTPrinter.h"
#include "Sema/SemaControlFlow.h"
#include "Sema/SemaLValue.h"
#include "Sema/SemaType.h"
#include "Sema/SymbolResolver.h"

#include <cstdio>

using namespace cawk;

bool checkType(TypeKind T1, TypeKind T2) {
  if (T1 == T2 || T1 == NullTy || T2 == NullTy)
    return true;
  if (T1 == NumberTy && T2 == StringTy || T1 == StringTy && T2 == NumberTy)
    return true;
  return false;
}

bool checkType(Expr *E, TypeKind T) { return checkType(E->getType(), T); }

void Sema::startFunction() {
  CtrlFlow.enterFunction();
  ReturnTypes.emplace_back();
}

DeclResult Sema::check(FunctionDecl *F) {
  CtrlFlow.exitFunction();
  if (Symbols.contains(std::string(F->getName())))
    return false;
  for (TypeKind T : ReturnTypes.back())
    F->addReturnType(T);
  ReturnTypes.pop_back();
  Symbols.addGlobal(std::string(F->getName()), new Value(F));
  return F;
}

StmtResult Sema::check(BreakStmt *S) {
  if (!CtrlFlow.isInLoop())
    return false;
  return S;
}

StmtResult Sema::check(ContinueStmt *S) {
  if (!CtrlFlow.isInLoop())
    return false;
  return S;
}

void Sema::startDoStatement() { CtrlFlow.enterLoop(); }

StmtResult Sema::check(DoStmt *S) {
  CtrlFlow.exitLoop();
  return S;
}

void Sema::startForStatement() { CtrlFlow.enterLoop(); }

StmtResult Sema::check(ForStmt *S) {
  CtrlFlow.exitLoop();
  if (S->getCond() != nullptr && !checkType(S->getCond(), NumberTy))
    return false;
  return S;
}

StmtResult Sema::check(ForRangeStmt *S) {
  CtrlFlow.exitLoop();
  if (S->getLoopVar() != nullptr && !checkType(S->getLoopVar(), NumberTy) ||
      S->getRange() != nullptr && !checkType(S->getRange(), ArrayTy))
    return false;
  return S;
}

StmtResult Sema::check(PrintStmt *P) {
  for (Expr *E : P->getArgs())
    if (!checkType(E, StringTy))
      return false;
  return P;
}

StmtResult Sema::check(ReturnStmt *R) {
  if (!CtrlFlow.isInFunction())
    return false;

  if (R->getValue() == nullptr) {
    ReturnTypes.back().insert(NullTy);
  } else {
    ReturnTypes.back().insert(R->getValue()->getType());
  }

  return R;
}

void Sema::startWhileStatement() { CtrlFlow.enterLoop(); }

void Sema::check(WhileStmt *W) {
  CtrlFlow.exitLoop();
  if (W->getCond() != nullptr && !checkType(W->getCond(), NumberTy))
    return false;
  return W;
}

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

  return true;
}