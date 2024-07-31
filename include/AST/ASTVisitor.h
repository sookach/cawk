#pragma once

#include "AST/AST.h"

namespace cawk {
namespace trav {
enum TraversalKind { Preorder, Postorder, RecursiveDescent };
};

template <typename Derived, trav::TraversalKind Traversal,
          bool CheckNull = false>
class ASTVisitor {
public:
  bool isPreorder() {
    return Traversal == trav::Preorder || Traversal == trav::RecursiveDescent;
  }

  bool isPostorder() {
    return Traversal == trav::Postorder || Traversal == trav::RecursiveDescent;
  }

  bool visit(Decl *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;
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

  bool visit(FunctionDecl *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(F);

    for (ParamVarDecl *P : F->getParams())
      visit(P);

    visit(F->getBody());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(F);

    return true;
  }

  bool visit(ParamVarDecl *P) {
    if constexpr (CheckNull)
      if (P == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(P);
    return true;
  }

  bool visit(RuleDecl *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(R);

    visit(R->getPattern());
    visit(R->getAction());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(R);

    return true;
  }

  bool visit(TranslationUnitDecl *T) {
    if constexpr (CheckNull)
      if (T == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(T);

    for (Decl *D : T->getDecls())
      visit(D);

    if (isPostorder())
      static_cast<Derived *>(this)->visit(T);

    return true;
  }

  bool visit(Stmt *S) {
    if constexpr (CheckNull)
      if (S == nullptr)
        return true;
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

  bool visit(BreakStmt *B) {
    if constexpr (CheckNull)
      if (B == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(B);
    return true;
  }

  bool visit(ContinueStmt *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(C);
    return true;
  }

  bool visit(CompoundStmt *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(C);

    for (Stmt *S : C->getBody())
      visit(S);

    if (isPostorder())
      static_cast<Derived *>(this)->visit(C);

    return true;
  }

  bool visit(DeleteStmt *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(D);

    visit(D->getArgument());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(D);

    return true;
  }

  bool visit(DoStmt *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(D);

    visit(D->getBody());
    visit(D->getCond());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(D);

    return true;
  }

  bool visit(ExitStmt *E) {
    if constexpr (CheckNull)
      if (E == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(E);

    visit(E->getValue());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(E);

    return true;
  }

  bool visit(ForStmt *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(F);

    visit(F->getInit());
    visit(F->getCond());
    visit(F->getInc());
    visit(F->getBody());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(F);

    return true;
  }

  bool visit(ForRangeStmt *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(F);

    visit(F->getLoopVar());
    visit(F->getRange());
    visit(F->getBody());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(F);

    return true;
  }

  bool visit(IfStmt *I) {
    if constexpr (CheckNull)
      if (I == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(I);

    visit(I->getCond());
    visit(I->getThen());
    visit(I->getElse());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(I);

    return true;
  }

  bool visit(NextStmt *N) {
    if constexpr (CheckNull)
      if (N == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(N);
    return true;
  }

  bool visit(NextfileStmt *N) {
    if constexpr (CheckNull)
      if (N == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(N);
    return true;
  }

  bool visit(PrintStmt *P) {
    if constexpr (CheckNull)
      if (P == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(P);

    for (Expr *E : P->getArgs())
      visit(E);

    if (isPostorder())
      static_cast<Derived *>(this)->visit(P);

    return true;
  }

  bool visit(ReturnStmt *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(R);

    visit(R->getValue());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(R);

    return true;
  }

  bool visit(ValueStmt *V) {
    if constexpr (CheckNull)
      if (V == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(V);

    visit(V->getValue());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(V);

    return true;
  }

  bool visit(WhileStmt *W) {
    if constexpr (CheckNull)
      if (W == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(W);

    visit(W->getCond());
    visit(W->getBody());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(W);

    return true;
  }

  bool visit(Expr *E) {
    if constexpr (CheckNull)
      if (E == nullptr)
        return true;
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

  bool visit(ArraySubscriptExpr *A) {
    if constexpr (CheckNull)
      if (A == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(A);

    visit(A->getLHS());
    for (Expr *E : A->getRHS())
      visit(E);

    if (isPostorder())
      static_cast<Derived *>(this)->visit(A);

    return true;
  }

  bool visit(BinaryOperator *B) {
    if constexpr (CheckNull)
      if (B == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(B);

    visit(B->getLHS());
    visit(B->getRHS());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(B);

    return true;
  }

  bool visit(CallExpr *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(C);

    visit(C->getCallee());

    for (Expr *E : C->getArgs())
      visit(E);

    if (isPostorder())
      static_cast<Derived *>(this)->visit(C);

    return true;
  }

  bool visit(DeclRefExpr *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(D);
    return true;
  }

  bool visit(FloatingLiteral *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(F);
    return true;
  }

  bool visit(RegexLiteral *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(R);
    return true;
  }

  bool visit(StringLiteral *S) {
    if constexpr (CheckNull)
      if (S == nullptr)
        return true;

    static_cast<Derived *>(this)->visit(S);
    return true;
  }

  bool visit(UnaryOperator *U) {
    if constexpr (CheckNull)
      if (U == nullptr)
        return true;

    if (isPreorder())
      static_cast<Derived *>(this)->visit(U);

    visit(U->getSubExpr());

    if (isPostorder())
      static_cast<Derived *>(this)->visit(U);

    return true;
  }
};
} // namespace cawk