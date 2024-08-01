#include "Sema/SemaDecl.h"

#include "Support/Support.h"

using namespace cawk;

bool SemaDecl::visit(FunctionDecl *F) {
  if (!GlobalRefs.try_emplace(std::string(F->getName()), F))
    return false;

  LocalRefs.clear();
  return true;
}

bool SemaDecl::visit(ParamVarDecl *P) {
  if (!LocalRefs.try_emplace(std::string(P->getName())), P)
    return false;
}

bool SemaDecl::visit(RuleDecl *R) { return true; }

bool SemaDecl::visit(TranslationUnitDecl *T) { return true; }

bool SemaDecl::visit(VarDecl *V) { return true; }

bool SemaDecl::visit(BreakStmt *B) { return true; }

bool SemaDecl::visit(CompoundStmt *C) { return true; }

bool SemaDecl::visit(ContinueStmt *C) { return true; }

bool SemaDecl::visit(DeleteStmt *D) {
    
}

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

bool SemaDecl::check(TranslationUnitDecl *T) { return visit(T); }

std::unordered_map<std::string_view, FunctionDecl *>
SemaDecl::getFunctionMap() {
  return FunctionMap;
}

std::vector<FunctionDecl *> SemaDecl::getFunctionDecls() {
  return FunctionDecls;
}

std::vector<RuleDecl *> SemaDecl::getRuleDecls() { return RuleDecls; }