#include "Sema/SemaDecl.h"

#include "Support/Support.h"

using namespace cawk;

bool SemaDecl::visit(Decl *D) {
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

bool SemaDecl::visit(FunctionDecl *F) {
  assert(!FunctionMap.contains(F->getIdentifier().getIdentifier()));
  FunctionMap[F->getIdentifier().getIdentifier()] = F;
  FunctionDecls.push_back(F);
  return true;
}

bool SemaDecl::visit(ParamVarDecl *P) { return true; }

bool SemaDecl::visit(RuleDecl *R) {
  if (R->getPattern() != nullptr && !visit(R->getPattern()))
    return false;
  if (R->getAction() != nullptr)
    return visit(R->getAction());
  return true;
}

bool SemaDecl::visit(TranslationUnitDecl *T) {
  for (Decl *D : T->getDecls())
    if (isa<FunctionDecl>(D))
      visit(D);

  for (Decl *D : T->getDecls())
    if (isa<RuleDecl>(D))
      visit(D);

  return true;
}

bool SemaDecl::visit(VarDecl *V) { return true; }

bool SemaDecl::visit(Stmt *S) {
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

bool SemaDecl::visit(BreakStmt *B) { return true; }

bool SemaDecl::visit(CompoundStmt *C) {
  for (Stmt *S : C->getBody())
    if (!visit(S))
      return false;
  return true;
}

bool SemaDecl::visit(ContinueStmt *C) { return true; }

bool SemaDecl::visit(DeleteStmt *D) { return visit(D->getArgument()); }

bool SemaDecl::visit(DoStmt *D) {
  return visit(D->getBody()) && visit(D->getCond());
}

bool SemaDecl::visit(ExitStmt *E) {
  if (E->getValue() != nullptr)
    return visit(E->getValue());
  return true;
}

bool SemaDecl::visit(ForStmt *F) {
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

bool SemaDecl::visit(ForRangeStmt *F) {
  if (F->getBody() != nullptr)
    return visit(F->getBody());
  return true;
}

bool SemaDecl::visit(IfStmt *I) {
  if (!visit(I->getCond()))
    return false;
  if (I->getThen() != nullptr && !visit(I->getThen()))
    return false;
  if (I->getElse() != nullptr && !visit(I->getElse()))
    return false;
  return true;
}

bool SemaDecl::visit(NextStmt *N) { return true; }

bool SemaDecl::visit(NextfileStmt *N) { return true; }

bool SemaDecl::visit(PrintStmt *P) {
  for (Expr *E : P->getArgs())
    if (!visit(E))
      return false;
  return true;
}

bool SemaDecl::visit(ReturnStmt *R) {
  if (R->getValue() != nullptr)
    return visit(R->getValue());
  return true;
}

bool SemaDecl::visit(ValueStmt *V) { return visit(V->getValue()); }

bool SemaDecl::visit(WhileStmt *W) {
  return visit(W->getCond()) && visit(W->getBody());
}

bool SemaDecl::visit(Expr *E) {
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

bool SemaDecl::visit(ArraySubscriptExpr *A) { return true; }

bool SemaDecl::visit(BinaryOperator *B) { return true; }

bool SemaDecl::visit(CallExpr *C) { return true; }

bool SemaDecl::visit(DeclRefExpr *D) { return true; }

bool SemaDecl::visit(FloatingLiteral *F) { return true; }

bool SemaDecl::visit(RegexLiteral *R) { return true; }

bool SemaDecl::visit(StringLiteral *S) { return true; }

bool SemaDecl::visit(UnaryOperator *U) { return true; }

StringMap<FunctionDecl *> SemaDecl::getFunctionMap() { return FunctionMap; }

std::vector<FunctionDecl *> SemaDecl::getFunctionDecls() {
  return FunctionDecls;
}

std::vector<RuleDecl *> SemaDecl::getRuleDecls() { return RuleDecls; }