#pragma once

#include "AST/AST.h"
#include <type_traits>

namespace cawk {
namespace trav {
enum TraversalKind { Preorder, Postorder, RecursiveDescent, None };
};

template <typename Derived, trav::TraversalKind Traversal,
          bool CheckNull = false, bool ShortCircuit = true,
          bool RequireImpl = false>
class ASTVisitor {
  template <typename T> static consteval bool hasVisit() {
    return requires(Derived D, T *P) {
      { D.visit(P) } -> std::same_as<bool>;
    };
  }

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

    if constexpr (RequireImpl || hasVisit<FunctionDecl>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(F))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(F);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent)
        static_cast<Derived *>(this)->template visit<true>(F);
    }

    for (VarDecl *V : F->getParams()) {
      if constexpr (ShortCircuit) {
        if (!visit(V))
          return false;
      } else {
        visit(V);
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(F->getBody()))
        return false;
    } else {
      visit(F->getBody());
    }

    if constexpr (RequireImpl || hasVisit<FunctionDecl>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(F);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(F);
    }

    return true;
  }

  bool visit(RuleDecl *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(R);

    if constexpr (RequireImpl || hasVisit<RuleDecl>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(R))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(R);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(R))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(R);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(R->getPattern()) || !visit(R->getAction()))
        return false;
    } else {
      visit(R->getPattern());
      visit(R->getAction());
    }

    if constexpr (RequireImpl || hasVisit<RuleDecl>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(R);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(R);
    }

    return true;
  }

  bool visit(TranslationUnitDecl *T) {
    if constexpr (CheckNull)
      if (T == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(T);

    if constexpr (RequireImpl || hasVisit<TranslationUnitDecl>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(T))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(T);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(T))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(T);
        }
      }
    }

    for (Decl *D : T->getDecls()) {
      if constexpr (ShortCircuit) {
        if (!visit(D))
          return false;
      } else {
        visit(D);
      }
    }

    if constexpr (RequireImpl || hasVisit<TranslationUnitDecl>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(T);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(T);
    }

    return true;
  }

  bool visit(VarDecl *V) {
    if constexpr (CheckNull)
      if (V == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(V);

    if constexpr (RequireImpl || hasVisit<VarDecl>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(V))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(V);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(V))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(V);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(V->getDeclRefExpr()))
        return false;
    } else {
      visit(V->getDeclRefExpr());
    }

    if constexpr (RequireImpl || hasVisit<VarDecl>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(V);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(V);
    }

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
      CASE(Null, NullStmt);
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

    if constexpr (RequireImpl || hasVisit<BreakStmt>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(B) &&
               static_cast<Derived *>(this)->template visit<false>(B);
      else
        return static_cast<Derived *>(this)->visit(B);
    }
    return true;
  }

  bool visit(ContinueStmt *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<ContinueStmt>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(C) &&
               static_cast<Derived *>(this)->template visit<false>(C);
      else
        return static_cast<Derived *>(this)->visit(C);
    }
    return true;
  }

  bool visit(CompoundStmt *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(C);

    if constexpr (RequireImpl || hasVisit<CompoundStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(C))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(C);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(C))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(C);
        }
      }
    }

    for (Stmt *S : C->getBody()) {
      if constexpr (ShortCircuit) {
        if (!visit(S))
          return false;
      } else {
        visit(S);
      }
    }

    if constexpr (RequireImpl || hasVisit<CompoundStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(C);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(C);
    }

    return true;
  }

  bool visit(DeleteStmt *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(D);

    if constexpr (RequireImpl || hasVisit<DeleteStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(D))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(D);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(D))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(D);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(D->getArgument()))
        return false;
    } else {
      visit(D->getArgument());
    }

    if constexpr (RequireImpl || hasVisit<DeleteStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(D);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(D);
    }

    return true;
  }

  bool visit(DoStmt *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(D);

    if constexpr (RequireImpl || hasVisit<DoStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(D))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(D);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(D))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(D);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(D->getBody()) || !visit(D->getCond()))
        return false;
    } else {
      visit(D->getBody());
      visit(D->getCond());
    }

    if constexpr (RequireImpl || hasVisit<DoStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(D);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(D);
    }

    return true;
  }

  bool visit(ExitStmt *E) {
    if constexpr (CheckNull)
      if (E == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(E);

    if constexpr (RequireImpl || hasVisit<ExitStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(E))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(E);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(E))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(E);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(E->getValue()))
        return false;
    } else {
      visit(E->getValue());
    }

    if constexpr (RequireImpl || hasVisit<ExitStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(E);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(E);
    }

    return true;
  }

  bool visit(ForStmt *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(F);

    if constexpr (RequireImpl || hasVisit<ForStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(F))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(F);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(F))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(F);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(F->getInit()) || !visit(F->getCond()) || !visit(F->getInc()) ||
          !visit(F->getBody()))
        return false;
    } else {
      visit(F->getInit());
      visit(F->getCond());
      visit(F->getInc());
      visit(F->getBody());
    }

    if constexpr (RequireImpl || hasVisit<ForStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(F);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(F);
    }

    return true;
  }

  bool visit(ForRangeStmt *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(F);

    if constexpr (RequireImpl || hasVisit<ForRangeStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(F))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(F);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(F))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(F);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(F->getLoopVar()) || !visit(F->getRange()) ||
          !visit(F->getBody()))
        return false;
    } else {
      visit(F->getLoopVar());
      visit(F->getRange());
      visit(F->getBody());
    }

    if constexpr (RequireImpl || hasVisit<ForRangeStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(F);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(F);
    }

    return true;
  }

  bool visit(IfStmt *I) {
    if constexpr (CheckNull)
      if (I == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(I);

    if constexpr (RequireImpl || hasVisit<IfStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(I))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(I);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(I))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(I);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(I->getCond()) || !visit(I->getThen()) || !visit(I->getElse()))
        return false;
    } else {
      visit(I->getCond());
      visit(I->getThen());
      visit(I->getElse());
    }

    if constexpr (RequireImpl || hasVisit<IfStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(I);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(I);
    }

    return true;
  }

  bool visit(NextStmt *N) {
    if constexpr (CheckNull)
      if (N == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<NextStmt>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(N) &&
               static_cast<Derived *>(this)->template visit<false>(N);
      else
        return static_cast<Derived *>(this)->visit(N);
    }
    return true;
  }

  bool visit(NextfileStmt *N) {
    if constexpr (CheckNull)
      if (N == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<NextfileStmt>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(N) &&
               static_cast<Derived *>(this)->template visit<false>(N);
      else
        return static_cast<Derived *>(this)->visit(N);
    }
    return true;
  }

  bool visit(NullStmt *N) {
    if constexpr (CheckNull)
      if (N == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<NullStmt>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(N) &&
               static_cast<Derived *>(this)->template visit<false>(N);
      else
        return static_cast<Derived *>(this)->visit(N);
    }
    return true;
  }

  bool visit(PrintStmt *P) {
    if constexpr (CheckNull)
      if (P == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(P);

    if constexpr (RequireImpl || hasVisit<PrintStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(P))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(P);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(P))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(P);
        }
      }
    }

    for (Expr *E : P->getArgs()) {
      if constexpr (ShortCircuit) {
        if (!visit(E))
          return false;
      } else {
        visit(E);
      }
    }

    if constexpr (RequireImpl || hasVisit<PrintStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(P);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(P);
    }

    return true;
  }

  bool visit(ReturnStmt *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(R);

    if constexpr (RequireImpl || hasVisit<ReturnStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(R))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(R);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(R))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(R);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(R->getValue()))
        return false;
    } else {
      visit(R->getValue());
    }

    if constexpr (RequireImpl || hasVisit<ReturnStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(R);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(R);
    }

    return true;
  }

  bool visit(ValueStmt *V) {
    if constexpr (CheckNull)
      if (V == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(V);

    if constexpr (RequireImpl || hasVisit<ValueStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(V))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(V);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(V))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(V);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(V->getValue()))
        return false;
    } else {
      visit(V->getValue());
    }

    if constexpr (RequireImpl || hasVisit<ValueStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(V);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(V);
    }

    return true;
  }

  bool visit(WhileStmt *W) {
    if constexpr (CheckNull)
      if (W == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(W);

    if constexpr (RequireImpl || hasVisit<WhileStmt>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(W))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(W);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(W))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(W);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(W->getCond()) || !visit(W->getBody()))
        return false;
    } else {
      visit(W->getCond());
      visit(W->getBody());
    }

    if constexpr (RequireImpl || hasVisit<WhileStmt>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(W);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(W);
    }

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
      CASE(Begin, BeginKeyword);
      CASE(Call, CallExpr);
      CASE(DeclRef, DeclRefExpr);
      CASE(End, EndKeyword);
      CASE(FloatingLiteral, FloatingLiteral);
      CASE(Lambda, LambdaExpr);
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

    if constexpr (RequireImpl || hasVisit<ArraySubscriptExpr>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(A))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(A);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(A))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(A);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(A->getLHS()))
        return false;
    } else {
      visit(A->getLHS());
    }

    for (Expr *E : A->getRHS()) {
      if constexpr (ShortCircuit) {
        if (!visit(E))
          return false;
      } else {
        visit(E);
      }
    }

    if constexpr (RequireImpl || hasVisit<ArraySubscriptExpr>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(A);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(A);
    }

    return true;
  }

  bool visit(BeginKeyword *B) {
    if constexpr (CheckNull)
      if (B == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<BeginKeyword>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(B) &&
               static_cast<Derived *>(this)->template visit<false>(B);
      else
        return static_cast<Derived *>(this)->visit(B);
    }
    return true;
  }

  bool visit(BinaryOperator *B) {
    if constexpr (CheckNull)
      if (B == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(B);

    if constexpr (RequireImpl || hasVisit<BinaryOperator>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(B))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(B);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(B))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(B);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(B->getLHS()) || !visit(B->getRHS()))
        return false;
    } else {
      visit(B->getLHS());
      visit(B->getRHS());
    }

    if constexpr (RequireImpl || hasVisit<BinaryOperator>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(B);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(B);
    }

    return true;
  }

  bool visit(CallExpr *C) {
    if constexpr (CheckNull)
      if (C == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(C);

    if constexpr (RequireImpl || hasVisit<CallExpr>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(C))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(C);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(C))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(C);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(C->getCallee()))
        return false;
    } else {
      visit(C->getCallee());
    }

    for (Expr *E : C->getArgs()) {
      if constexpr (ShortCircuit) {
        if (!visit(E))
          return false;
      } else {
        visit(E);
      }
    }

    if constexpr (RequireImpl || hasVisit<CallExpr>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(C);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(C);
    }

    return true;
  }

  bool visit(DeclRefExpr *D) {
    if constexpr (CheckNull)
      if (D == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<DeclRefExpr>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(D) &&
               static_cast<Derived *>(this)->template visit<false>(D);
      else
        return static_cast<Derived *>(this)->visit(D);
    }
    return true;
  }

  bool visit(FloatingLiteral *F) {
    if constexpr (CheckNull)
      if (F == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<FloatingLiteral>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(F) &&
               static_cast<Derived *>(this)->template visit<false>(F);
      else
        return static_cast<Derived *>(this)->visit(F);
    }
    return true;
  }

  bool visit(LambdaExpr *L) {
    if constexpr (CheckNull)
      if (L == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(L);

    if constexpr (RequireImpl || hasVisit<LambdaExpr>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(L))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(L);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent)
        static_cast<Derived *>(this)->template visit<true>(L);
    }

    for (VarDecl *V : L->getParams()) {
      if constexpr (ShortCircuit) {
        if (!visit(V))
          return false;
      } else {
        visit(V);
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(L->getBody()))
        return false;
    } else {
      visit(L->getBody());
    }

    if constexpr (RequireImpl || hasVisit<LambdaExpr>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(L);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(L);
    }

    return true;
  }

  bool visit(EndKeyword *E) {
    if constexpr (CheckNull)
      if (E == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<BeginKeyword>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(E) &&
               static_cast<Derived *>(this)->template visit<false>(E);
      else
        return static_cast<Derived *>(this)->visit(E);
    }
    return true;
  }

  bool visit(RegexLiteral *R) {
    if constexpr (CheckNull)
      if (R == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<RegexLiteral>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(R) &&
               static_cast<Derived *>(this)->template visit<false>(R);
      else
        return static_cast<Derived *>(this)->visit(R);
    }
    return true;
  }

  bool visit(StringLiteral *S) {
    if constexpr (CheckNull)
      if (S == nullptr)
        return true;

    if constexpr (RequireImpl || hasVisit<StringLiteral>()) {
      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<true>(S) &&
               static_cast<Derived *>(this)->template visit<false>(S);
      else
        return static_cast<Derived *>(this)->visit(S);
    }
    return true;
  }

  bool visit(UnaryOperator *U) {
    if constexpr (CheckNull)
      if (U == nullptr)
        return true;

    if constexpr (Traversal == trav::None)
      return static_cast<Derived *>(this)->visit(U);

    if constexpr (RequireImpl || hasVisit<UnaryOperator>()) {
      if constexpr (Traversal == trav::Preorder) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->visit(U))
            return false;
        } else {
          static_cast<Derived *>(this)->visit(U);
        }
      }

      if constexpr (Traversal == trav::RecursiveDescent) {
        if constexpr (ShortCircuit) {
          if (!static_cast<Derived *>(this)->template visit<true>(U))
            return false;
        } else {
          static_cast<Derived *>(this)->template visit<true>(U);
        }
      }
    }

    if constexpr (ShortCircuit) {
      if (!visit(U->getSubExpr()))
        return false;
    } else {
      visit(U->getSubExpr());
    }

    if constexpr (RequireImpl || hasVisit<UnaryOperator>()) {
      if constexpr (Traversal == trav::Postorder)
        return static_cast<Derived *>(this)->visit(U);

      if constexpr (Traversal == trav::RecursiveDescent)
        return static_cast<Derived *>(this)->template visit<false>(U);
    }

    return true;
  }

public:
  bool traverse(Decl *D) { return visit(D); }
  bool traverse(Stmt *S) { return visit(S); }
  bool traverse(Expr *E) { return visit(E); }
};
} // namespace cawk