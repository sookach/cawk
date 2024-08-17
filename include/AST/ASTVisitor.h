#pragma once

#include "AST/AST.h"

namespace cawk {
namespace trav {
enum TraversalKind { Preorder, Postorder, RecursiveDescent, None };
};

template <typename Derived, trav::TraversalKind Traversal,
          bool CheckNull = false>
class ASTVisitor {
protected:
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

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(F);

    for (ParamVarDecl *P : F->getParams())
      visit(P);

    visit(F->getBody());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(F);

    return true;
  }

  bool visit(ParamVarDecl *P) {
    if constexpr (CheckNull)
      if (P == nullptr)
        return true;

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(P) &&
             static_cast<Derived *>(this)->template visit<false>(P);
    } else {
      return static_cast<Derived *>(this)->visit(P);
    }
  }

  bool visit(RuleDecl *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(R);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(R);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(R);

    visit(R->getPattern());
    visit(R->getAction());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(R);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(R);

    return true;
  }

  bool visit(TranslationUnitDecl *T) {
    if constexpr (CheckNull)
      if (T == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(T);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(T);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(T);

    for (Decl *D : T->getDecls())
      visit(D);

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(T);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(T);

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

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(B) &&
             static_cast<Derived *>(this)->template visit<false>(B);
    } else {
      return static_cast<Derived *>(this)->visit(B);
    }
  }

  bool visit(ContinueStmt *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(C) &&
             static_cast<Derived *>(this)->template visit<false>(C);
    } else {
      return static_cast<Derived *>(this)->visit(C);
    }
  }

  bool visit(CompoundStmt *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(C);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(C);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(C);

    for (Stmt *S : C->getBody())
      visit(S);

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(C);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(C);

    return true;
  }

  bool visit(DeleteStmt *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(D);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(D);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(D);

    visit(D->getArgument());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(D);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(D);

    return true;
  }

  bool visit(DoStmt *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(D);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(D);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(D);

    visit(D->getBody());
    visit(D->getCond());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(D);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(D);

    return true;
  }

  bool visit(ExitStmt *E) {
    if constexpr (CheckNull)
      if (E == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(E);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(E);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(E);

    visit(E->getValue());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(E);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(E);

    return true;
  }

  bool visit(ForStmt *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(F);

    visit(F->getInit());
    visit(F->getCond());
    visit(F->getInc());
    visit(F->getBody());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(F);

    return true;
  }

  bool visit(ForRangeStmt *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(F);

    visit(F->getLoopVar());
    visit(F->getRange());
    visit(F->getBody());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(F);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(F);

    return true;
  }

  bool visit(IfStmt *I) {
    if constexpr (CheckNull)
      if (I == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(I);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(I);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(I);

    visit(I->getCond());
    visit(I->getThen());
    visit(I->getElse());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(I);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(I);

    return true;
  }

  bool visit(NextStmt *N) {
    if constexpr (CheckNull)
      if (N == nullptr)
        return true;

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(N) &&
             static_cast<Derived *>(this)->template visit<false>(N);
    } else {
      return static_cast<Derived *>(this)->visit(N);
    }
  }

  bool visit(NextfileStmt *N) {
    if constexpr (CheckNull)
      if (N == nullptr)
        return true;

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(N) &&
             static_cast<Derived *>(this)->template visit<false>(N);
    } else {
      return static_cast<Derived *>(this)->visit(N);
    }
  }

  bool visit(PrintStmt *P) {
    if constexpr (CheckNull)
      if (P == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(P);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(P);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(P);

    for (Expr *E : P->getArgs())
      visit(E);

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(P);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(P);

    return true;
  }

  bool visit(ReturnStmt *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(R);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(R);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(R);

    visit(R->getValue());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(R);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(R);

    return true;
  }

  bool visit(ValueStmt *V) {
    if constexpr (CheckNull)
      if (V == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(V);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(V);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(V);

    visit(V->getValue());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(V);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(V);

    return true;
  }

  bool visit(WhileStmt *W) {
    if constexpr (CheckNull)
      if (W == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(W);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(W);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(W);

    visit(W->getCond());
    visit(W->getBody());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(W);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(W);

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

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(A);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(A);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(A);

    visit(A->getLHS());
    for (Expr *E : A->getRHS())
      visit(E);

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(A);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(A);

    return true;
  }

  bool visit(BinaryOperator *B) {
    if constexpr (CheckNull)
      if (B == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(B);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(B);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(B);

    visit(B->getLHS());
    visit(B->getRHS());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(B);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(B);

    return true;
  }

  bool visit(CallExpr *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(C);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(C);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(C);

    visit(C->getCallee());

    for (Expr *E : C->getArgs())
      visit(E);

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(C);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(C);

    return true;
  }

  bool visit(DeclRefExpr *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(D) &&
             static_cast<Derived *>(this)->template visit<false>(D);
    } else {
      return static_cast<Derived *>(this)->visit(D);
    }
  }

  bool visit(FloatingLiteral *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(F) &&
             static_cast<Derived *>(this)->template visit<false>(F);
    } else {
      return static_cast<Derived *>(this)->visit(F);
    }
  }

  bool visit(RegexLiteral *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(R) &&
             static_cast<Derived *>(this)->template visit<false>(R);
    } else {
      return static_cast<Derived *>(this)->visit(R);
    }
  }

  bool visit(StringLiteral *S) {
    if constexpr (CheckNull)
      if (S == nullptr)
        return true;

    if constexpr (Traversal == trav::RecursiveDescent) {
      return static_cast<Derived *>(this)->template visit<true>(S) &&
             static_cast<Derived *>(this)->template visit<false>(S);
    } else {
      return static_cast<Derived *>(this)->visit(S);
    }
  }

  bool visit(UnaryOperator *U) {
    if constexpr (CheckNull)
      if (U == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(U);

    if constexpr (Traversal == trav::Preorder)
      static_cast<Derived *>(this)->visit(U);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<true>(U);

    visit(U->getSubExpr());

    if constexpr (Traversal == trav::Postorder)
      static_cast<Derived *>(this)->visit(U);

    if constexpr (Traversal == trav::RecursiveDescent)
      static_cast<Derived *>(this)->template visit<false>(U);

    return true;
  }

public:
  bool traverse(Decl *D) { return visit(D); }
  bool traverse(Stmt *S) { return visit(S); }
  bool traverse(Expr *E) { return visit(E); }
};
} // namespace cawk