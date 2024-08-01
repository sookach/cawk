#include "Sema/SemaLValue.h"

#include "Support/Support.h"

using namespace cawk;

bool SemaLValue::visit(Decl *D) {
  switch (D->getKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Decl::DK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(D))
    CASE(Function, FunctionDecl);
    CASE(ParamVar, ParamVarDecl);
    CASE(Rule, RuleDecl);
    CASE(TranslationUnit, TranslationUnitDecl);
    CASE(Var, VarDecl);
#undef CASE
  }
}

bool SemaLValue::visit(FunctionDecl *F) { return visit(F->getBody()); }

bool SemaLValue::visit(ParamVarDecl *P) { return true; }

bool SemaLValue::visit(RuleDecl *R) {
  if (R->getPattern() != nullptr && !visit(R->getPattern()))
    return false;
  if (R->getAction() != nullptr)
    return visit(R->getAction());
  return true;
}

bool SemaLValue::visit(TranslationUnitDecl *T) {
  for (Decl *D : T->getDecls())
    if (!visit(D))
      return false;
  return true;
}

bool SemaLValue::visit(VarDecl *V) { return true; }

bool SemaLValue::visit(Stmt *S) {
  switch (S->getKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Stmt::SK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(S))
    CASE(Break, BreakStmt);
    CASE(Compound, CompoundStmt);
    CASE(Continue, ContinueStmt);
    CASE(Delete, DeleteStmt);
    CASE(Do, DoStmt);
    CASE(Exit, ExitStmt);
    CASE(For, ForStmt);
    CASE(ForRange, ForRangeStmt);
    CASE(If, IfStmt);
    CASE(Next, NextStmt);
    CASE(Nextfile, NextfileStmt);
    CASE(Print, PrintStmt);
    CASE(Return, ReturnStmt);
    CASE(Value, ValueStmt);
    CASE(While, WhileStmt);
#undef CASE
  }
}

bool SemaLValue::visit(BreakStmt *B) { return true; }

bool SemaLValue::visit(CompoundStmt *C) {
  for (Stmt *S : C->getBody())
    if (!visit(S))
      return false;
  return true;
}

bool SemaLValue::visit(ContinueStmt *C) { return true; }

bool SemaLValue::visit(DeleteStmt *D) {
  if (!visit(D->getArgument()) || !D->getArgument()->isLValue())
    return false;
  return true;
}

bool SemaLValue::visit(DoStmt *D) {
  return visit(D->getBody()) && visit(D->getCond());
}

bool SemaLValue::visit(ExitStmt *E) {
  if (E->getValue() != nullptr)
    return visit(E->getValue());
  return true;
}

bool SemaLValue::visit(ForStmt *F) {
  if (F->getInit() != nullptr && !visit(F->getInit()))
    return false;
  if (F->getCond() != nullptr && !visit(F->getCond()))
    return false;
  if (F->getInc() != nullptr && !visit(F->getInc()))
    return false;
  if (F->getBody() != nullptr && !visit(F->getBody()))
    return false;
  return true;
}

bool SemaLValue::visit(ForRangeStmt *F) {
  if (!visit(F->getLoopVar()) || !F->getLoopVar()->isLValue())
    return false;

  if (!visit(F->getRange()) || !F->getRange()->isLValue())
    return false;

  if (F->getBody() != nullptr)
    return F->getBody();

  return true;
}

bool SemaLValue::visit(IfStmt *I) {
  if (!visit(I->getCond()))
    return false;
  if (I->getThen() != nullptr && !visit(I->getThen()))
    return false;
  if (I->getElse() != nullptr && !visit(I->getElse()))
    return false;
  return true;
}

bool SemaLValue::visit(NextStmt *N) { return true; }

bool SemaLValue::visit(NextfileStmt *N) { return true; }

bool SemaLValue::visit(PrintStmt *P) {
  for (Expr *E : P->getArgs())
    if (!visit(E))
      return false;
  return true;
}

bool SemaLValue::visit(ReturnStmt *R) {
  if (R->getValue() != nullptr)
    return visit(R->getValue());
  return true;
}

bool SemaLValue::visit(ValueStmt *V) { return visit(V->getValue()); }

bool SemaLValue::visit(WhileStmt *W) {
  return visit(W->getCond()) && visit(W->getBody());
}

bool SemaLValue::visit(Expr *E) {
  switch (E->getKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Expr::EK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(E));
    CASE(ArraySubscript, ArraySubscriptExpr);
    CASE(BinaryOperator, BinaryOperator);
    CASE(Call, CallExpr);
    CASE(DeclRef, DeclRefExpr);
    CASE(FloatingLiteral, FloatingLiteral);
    CASE(RegexLiteral, RegexLiteral);
    CASE(StringLiteral, StringLiteral);
    CASE(UnaryOperator, UnaryOperator);
#undef CASE
  default:
    return true;
  }
}

bool SemaLValue::visit(ArraySubscriptExpr *A) {
  A->markAsLValue();
  return true;
}

bool SemaLValue::visit(BinaryOperator *B) {
  if (!B->getLHS() || !B->getRHS())
    return false;

  switch (B->getOpcode().getKind()) {
  default:
    break;
  case tok::equal:
  case tok::plusequal:
  case tok::minusequal:
  case tok::starequal:
  case tok::slashequal:
  case tok::percentequal:
  case tok::caretequal:
  case tok::starstarequal:
    if (!B->getLHS()->isLValue())
      return false;
    B->markAsLValue();
  }
  return true;
}

bool SemaLValue::visit(CallExpr *C) {
  if (!visit(C->getCallee()) || !C->getCallee()->isLValue())
    return false;
  for (Expr *E : C->getArgs())
    visit(E);
  return true;
}

bool SemaLValue::visit(DeclRefExpr *D) {
  D->markAsLValue();
  return true;
}

bool SemaLValue::visit(FloatingLiteral *F) { return true; }

bool SemaLValue::visit(RegexLiteral *R) { return true; }

bool SemaLValue::visit(StringLiteral *S) { return true; }

bool SemaLValue::visit(UnaryOperator *U) { return true; }

bool SemaLValue::check(TranslationUnitDecl *T) { return visit(T); }