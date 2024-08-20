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

static bool isConvertibleTo(Expr *E, type::TypeKind Type) {
  return areTypesConvertible(E->getType(), Type);
}

bool SemaType::visit(RuleDecl *R) {
  if (R->getPattern() != nullptr &&
      !areTypesConvertible(R->getPattern()->getType(), type::primitive))
    return false;
  return true;
}

bool SemaType::visit(DeleteStmt *D) {
  return areTypesConvertible(D->getArgument()->getType(), type::array);
}

bool SemaType::visit(DoStmt *D) {
  return areTypesConvertible(D->getCond()->getType(), type::primitive);
}

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

bool SemaType::visit(WhileStmt *W) {
  return areTypesConvertible(W->getCond()->getType(), type::primitive);
}

bool SemaType::visit(ArraySubscriptExpr *A) {
  if (!areTypesConvertible(A->getLHS()->getType(), type::array)) {
    Diags.addError(getLineNumber(A), diag::sema_primitive_subscript,
                   toString(A->getLHS()->getType()));
    return false;
  }

  A->getLHS()->setType(type::array);

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

bool SemaType::visit(FloatingLiteral *F) {
  F->setType(type::primitive);
  return true;
}

bool SemaType::visit(RegexLiteral *R) {
  R->setType(type::primitive);
  return true;
}

bool SemaType::visit(StringLiteral *S) {
  S->setType(type::primitive);
  return true;
}

bool SemaType::visit(UnaryOperator *U) {
  switch (U->getOpcode().getKind()) {
  default:
    return true;
  case tok::minus:
  case tok::plus:
  case tok::exclaim:
  case tok::plusplus:
  case tok::minusminus:
    if (areTypesConvertible(U->getSubExpr()->getType(), type::primitive))
      return true;
    Diags.addError(U->getOpcode().getLine(), diag::sema_invalid_operand_type,
                   U->getOpcode().getLiteralData(),
                   toString(U->getSubExpr()->getType()));
    return false;
  }
}

bool SemaType::check(TranslationUnitDecl *T) { return traverse(T); }