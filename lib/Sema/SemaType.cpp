#include "Sema/SemaType.h"

#include "Support/Support.h"

using namespace cawk;

#include "Sema/SemaType.h"

#include "Support/Support.h"

using namespace cawk;

std::string toString(TypeKind Type) {
  switch (Type) {
  case NullTy:
    return "null";
  case NumberTy:
    return "number";
  case StringTy:
    return "string";
  case ArrayTy:
    return "array";
  case FuncionTy:
    return "function";
  }
}

bool SemaType::checkType(TypeKind T1, TypeKind T2) {
  if (T1 == T2 || T1 == NullTy || T2 == NullTy)
    return true;
  if (T1 == NumberTy && T2 == StringTy || T1 == StringTy && T2 == NumberTy)
    return true;
  return false;
}

bool checkType(Expr *E, TypeKind Type) { return checkType(E->getType(), Type); }

bool SemaType::visit(RuleDecl *R) {
  if (R->getPattern() != nullptr && !checkType(R->getPattern(), NumberTy))
    return false;
  return true;
}

bool SemaType::visit(DeleteStmt *D) {
  return checkType(D->getArgument(), ArrayTy);
}

bool SemaType::visit(DoStmt *D) { return checkType(D->getCond(), NumberTy); }

bool SemaType::visit(ExitStmt *E) { return checkType(E->getValue(), NumberTy); }

bool SemaType::visit(ForStmt *F) {
  if (F->getCond() != nullptr && !checkType(F->getCond(), NumberTy))
    return false;
  return true;
}

bool SemaType::visit(ForRangeStmt *F) {
  return checkType(F->getLoopVar(), NumberTy) &&
         checkType(F->getRange(), ArrayTy);
}

bool SemaType::visit(IfStmt *I) { return checkType(I->getCond(), NumberTy); }

bool SemaType::visit(PrintStmt *P) {
  for (Expr *E : P->getArgs())
    if (!checkType(E->getType(), StringTy))
      return false;
  return true;
}

bool SemaType::visit(ReturnStmt *R) {
  if (R->getValue() != nullptr &&
      !checkType(R->getValue()->getType(), NumberTy))
    return false;
  return true;
}

bool SemaType::visit(WhileStmt *W) {
  return checkType(W->getCond()->getType(), NumberTy);
}

bool SemaType::visit(ArraySubscriptExpr *A) {
  if (!checkType(A->getLHS(), ArrayTy)) {
    Diags.addError(A->getSourceRange(), diag::sema_primitive_subscript,
                   toString(A->getLHS()->getType()));
    return false;
  }

  A->getLHS()->setType(ArrayTy);

  for (Expr *E : A->getRHS())
    if (!checkType(E->getType(), NumberTy))
      return false;

  A->setType(NumberTy);
  return true;
}

bool SemaType::visit(BinaryOperator *B) {
  if (!checkType(B->getLHS(), NumberTy) || !checkType(B->getRHS(), NumberTy)) {
    Diags.addError(B->getSourceRange(), diag::sema_invalid_operand_types, "+",
                   toString(B->getLHS()->getType()),
                   toString(B->getRHS()->getType()));
    return false;
  }

  B->setType(NumberTy);
  return true;
}

bool SemaType::visit(CallExpr *C) {

  C->setType(NumberTy);
  return true;
}

bool SemaType::visit(FloatingLiteral *F) {
  F->setType(NumberTy);
  return true;
}

bool SemaType::visit(RegexLiteral *R) {
  R->setType(StringTy);
  return true;
}

bool SemaType::visit(StringLiteral *S) {
  S->setType(StringTy);
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
    if (checkType(U->getSubExpr()->getType(), cat::Scalar)) {
      U->setType(NumberTy);
      return true;
    }
    Diags.addError(U->getSourceRange(), diag::sema_invalid_operand_type,
                   U->getOpcode().getLiteralData(),
                   toString(U->getSubExpr()->getType()));
    return false;
  }
}

bool SemaType::check(TranslationUnitDecl *T) { return traverse(T); }