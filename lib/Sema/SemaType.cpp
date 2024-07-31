#include "Sema/SemaType.h"

#include "Support/Support.h"

using namespace cawk;

// bool equal(SemaType::TypeKind T1, SemaType::TypeKind T2) {
//   switch (T1) {
//   default:
//     cawk_unreachable("invalid type comparison");
//   case SemaType::T:
//     return T2 == TK_Primitive || T2 == TK_PrimitiveOrArray || T2 == TK_Null;
//   case TK_Array:
//     return T2 == TK_Array || T2 == TK_PrimitiveOrArray || T2 == TK_Null;
//   case TK_PrimitiveOrArray:
//     return T2 == TK_Primitive || T2 == TK_Array || T2 == TK_PrimitiveOrArray
//     ||
//            T2 == TK_Null;
//   }
// }

// static TypeKind resultType(TypeKind T1, TypeKind T2) {
//   return T1 == T2 ? T1
//          : T1 == TK_Any || T2 == TK_Any
//              ? TK_Any
//              : (assert(0 && "invalid types passed to resultType"), TK_Any);
// }

// template <typename T, typename... Ts>
// static bool is(TypeKind Type, T K, Ts... Ks) {
//   if (Type == K)
//     return true;

//   if constexpr (sizeof...(Ks) != 0)
//     return is(Type, Ks...);

//   return false;
// }

bool SemaType::visit(Decl *D) {
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

bool SemaType::visit(FunctionDecl *F) {
  Locals = decltype(Locals)();
  for (ParamVarDecl *P : F->getParams())
    Locals[P->getIdentifier().getIdentifier()] = TK_Null;
  return visit(F->getBody());
}

bool SemaType::visit(ParamVarDecl *P) { return true; }

bool SemaType::visit(RuleDecl *R) {
  if (R->getPattern() != nullptr && !visit(R->getPattern()))
    return false;
  if (R->getAction() != nullptr)
    return visit(R->getAction());
  return true;
}

bool SemaType::visit(TranslationUnitDecl *T) {
  for (Decl *D : T->getDecls()) {
    if (isa<FunctionDecl>(D)) {
      auto F = static_cast<FunctionDecl *>(D);
      auto Iden = F->getIdentifier().getIdentifier();
      Globals[Iden] = TK_Function;
      FunctionPrototypes[Iden].resize(std::size(F->getParams()), TK_Null);
    }
  }

  for (Decl *D : T->getDecls())
    if (!visit(D))
      return false;

  return true;
}

bool SemaType::visit(VarDecl *V) { return true; }

bool SemaType::visit(Stmt *S) {
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

bool SemaType::visit(BreakStmt *B) { return true; }

bool SemaType::visit(CompoundStmt *C) {
  for (Stmt *S : C->getBody())
    if (!visit(S))
      return false;
  return true;
}

bool SemaType::visit(ContinueStmt *C) { return true; }

bool SemaType::visit(DeleteStmt *D) {
  TypeKind T = visit(D->getArgument());
  return T == TK_Array;
}

bool SemaType::visit(DoStmt *D) {
  return visit(D->getBody()) && visit(D->getCond());
}

bool SemaType::visit(ExitStmt *E) {
  if (E->getValue() != nullptr)
    return visit(E->getValue()) == TK_Primitive;
  return true;
}

bool SemaType::visit(ForStmt *F) {
  if (F->getInit() != nullptr && !visit(F->getInit()))
    return false;
  if (F->getCond() != nullptr && visit(F->getCond()) != TK_Primitive)
    return false;
  if (F->getInc() != nullptr && !visit(F->getInc()))
    return false;
  if (F->getBody() != nullptr && !visit(F->getBody()))
    return false;
  return true;
}

bool SemaType::visit(ForRangeStmt *F) {
  if (F->getBody() != nullptr)
    return visit(F->getBody());
  return true;
}

bool SemaType::visit(IfStmt *I) {
  if (visit(I->getCond()) != TK_Primitive)
    return false;
  if (I->getThen() != nullptr && !visit(I->getThen()))
    return false;
  if (I->getElse() != nullptr && !visit(I->getElse()))
    return false;
  return true;
}

bool SemaType::visit(NextStmt *N) { return true; }

bool SemaType::visit(NextfileStmt *N) { return true; }

bool SemaType::visit(PrintStmt *P) {
  for (Expr *E : P->getArgs())
    if (visit(E) != TK_Primitive)
      return false;
  return true;
}

bool SemaType::visit(ReturnStmt *R) {
  if (R->getValue() != nullptr)
    return visit(R->getValue()) == TK_Primitive;
  return true;
}

bool SemaType::visit(ValueStmt *V) { return visit(V->getValue()); }

bool SemaType::visit(WhileStmt *W) {
  return visit(W->getCond()) == TK_Primitive && visit(W->getBody());
}

SemaType::TypeKind SemaType::visit(Expr *E) {
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
  }
}

SemaType::TypeKind SemaType::visit(ArraySubscriptExpr *A) {
  assert(isa<DeclRefExpr>(A->getLHS()));
  auto LHSIden =
      static_cast<DeclRefExpr *>(A->getLHS())->getIdentifier().getIdentifier();

  if (TypeKind T = getType(LHSIden); T != TK_Array)
    return TK_Error;

  setType(LHSIden, TK_Array);

  for (Expr *E : A->getRHS())
    if (visit(E) != TK_Primitive)
      return TK_Error;

  return TK_Primitive;
}

SemaType::TypeKind SemaType::visit(BinaryOperator *B) {
  switch (B->getOpcode().getKind()) {
  default: // concatenation.
  case tok::plus:
  case tok::minus:
  case tok::star:
  case tok::slash:
  case tok::percent:
  case tok::caret:
  case tok::starstar:
  case tok::plusequal:
  case tok::minusequal:
  case tok::starequal:
  case tok::slashequal:
  case tok::percentequal:
  case tok::caretequal:
  case tok::starstarequal:
  case tok::ampamp:
  case tok::pipepipe:
    if (visit(B->getLHS()) != TK_Primitive ||
        visit(B->getRHS()) != TK_Primitive)
      return TK_Error;
    return TK_Primitive;
  case tok::equal:
  case tok::exclaimequal: {
    auto T1 = visit(B->getLHS()), T2 = visit(B->getRHS());
    if (T1 != T2)
      return TK_Error;
    return T1;
  }
  }
}

SemaType::TypeKind SemaType::visit(CallExpr *C) { return TK_Primitive; }

SemaType::TypeKind SemaType::visit(DeclRefExpr *D) {
  return getType(D->getIdentifier().getIdentifier());
}

SemaType::TypeKind SemaType::visit(FloatingLiteral *F) { return TK_Primitive; }

SemaType::TypeKind SemaType::visit(RegexLiteral *R) { return TK_Primitive; }

SemaType::TypeKind SemaType::visit(StringLiteral *S) { return TK_Primitive; }

SemaType::TypeKind SemaType::visit(UnaryOperator *U) {
  if (visit(U->getSubExpr()) != TK_Primitive)
    return TK_Error;
  return TK_Primitive;
}