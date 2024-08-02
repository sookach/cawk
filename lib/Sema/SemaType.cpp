#include "Sema/SemaType.h"

#include "Support/Support.h"

using namespace cawk;

#include "Sema/SemaType.h"

#include "Support/Support.h"

using namespace cawk;

static bool areTypesConvertible(type::TypeKind FromType,
                                type::TypeKind ToType) {
  if (FromType == type::null || FromType == type::any || ToType == type::null ||
      ToType == type::any)
    return true;
  return FromType == ToType;
}

bool SemaType::visit(FunctionDecl *F) { return true; }

bool SemaType::visit(ParamVarDecl *P) { return true; }

bool SemaType::visit(RuleDecl *R) {
  if (R->getPattern() != nullptr &&
      !areTypesConvertible(R->getPattern()->getType(), type::primitive))
    return false;
  return true;
}

bool SemaType::visit(TranslationUnitDecl *T) { return true; }

bool SemaType::visit(VarDecl *V) { return true; }

bool SemaType::visit(BreakStmt *B) { return true; }

bool SemaType::visit(CompoundStmt *C) { return true; }

bool SemaType::visit(ContinueStmt *C) { return true; }

bool SemaType::visit(DeleteStmt *D) {
  return areTypesConvertible(D->getArgument()->getType(), type::array);
}

bool SemaType::visit(DoStmt *D) { return true; }

bool SemaType::visit(ExitStmt *E) {
  return areTypesConvertible(E->getValue()->getType(), type::primitive);
}

bool SemaType::visit(ForStmt *F) {
  if (F->getCond() != nullptr &&
      !areTypesConvertible(F->getCond()->getType(), type::primitive))
    return false;
  return true;
}

bool SemaType::visit(ForRangeStmt *F) {
  return areTypesConvertible(F->getLoopVar()->getType(), type::primitive) &&
         areTypesConvertible(F->getRange()->getType(), type::array);
}

bool SemaType::visit(IfStmt *I) {
  return areTypesConvertible(I->getCond()->getType(), type::primitive);
}

bool SemaType::visit(NextStmt *N) { return true; }

bool SemaType::visit(NextfileStmt *N) { return true; }

bool SemaType::visit(PrintStmt *P) {
  for (Expr *E : P->getArgs())
    if (!areTypesConvertible(E->getType(), type::primitive))
      return false;
  return true;
}

bool SemaType::visit(ReturnStmt *R) {
  if (R->getValue() != nullptr)
    return areTypesConvertible(R->getValue()->getType(), type::primitive);
  return true;
}

bool SemaType::visit(ValueStmt *V) { return true; }

bool SemaType::visit(WhileStmt *W) {
  return areTypesConvertible(W->getCond()->getType(), type::primitive);
}

bool SemaType::visit(ArraySubscriptExpr *A) {
  if (!areTypesConvertible(A->getLHS()->getType(), type::array))
    return false;

  for (Expr *E : A->getRHS())
    if (!areTypesConvertible(E->getType(), type::primitive))
      return false;

  return true;
}

bool SemaType::visit(BinaryOperator *B) {
  return areTypesConvertible(B->getLHS()->getType(), B->getRHS()->getType());
}

bool SemaType::visit(CallExpr *C) {
  for (Expr *E : C->getArgs())
    if (!areTypesConvertible(E->getType(), type::primitive))
      return false;
  return true;
}

bool SemaType::visit(DeclRefExpr *D) { return true; }

bool SemaType::visit(FloatingLiteral *F) { return true; }

bool SemaType::visit(RegexLiteral *R) { return true; }

bool SemaType::visit(StringLiteral *S) { return true; }

bool SemaType::visit(UnaryOperator *U) { return true; }

bool SemaType::check(TranslationUnitDecl *T) { return true; }