#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Exec/Value.h"
#include "Support/Support.h"

#include <cmath>
#include <ranges>

namespace cawk {
class Exec {
  std::unordered_map<std::string, FunctionDecl> Functions;
  std::unordered_map<std::string, Value> GlobalSymbolTable;
  std::unordered_map<std::string, Value> LocalSymbolTable;

public:
  void Visit(TranslationUnitDecl *T) {
    for (Decl *D : T->getDecls() | std::views::filter([](Decl *D) {
                     return isa<RuleDecl>(D);
                   }))
      Visit(static_cast<RuleDecl *>(D));
  }

  void Visit(RuleDecl *R) {
    if (Visit(R->getPattern()))
      Visit(R->getAction());
  }

  void Visit(Stmt *S) {
    switch (S->getKind()) {
#if defined(CASE)
      static_assert(false);
#else
#define CASE(KIND, CLASS)                                                      \
  case Stmt::SK_##KIND:                                                        \
    return Visit(static_cast<CLASS *>(S))
#endif
      CASE(Break, BreakStmt);
      CASE(Continue, ContinueStmt);
      CASE(Compound, CompoundStmt);
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

  Value Visit(Expr *E) {
    switch (E->getKind()) {
#if defined(CASE)
      static_assert(false);
#else
#define CASE(KIND, CLASS)                                                      \
  case Expr::EK_##KIND:                                                        \
    return Visit(static_cast<CLASS *>(E));
#endif
      CASE(ArraySubscript, ArraySubscriptExpr)
      CASE(BinaryOperator, BinaryOperator)
      CASE(Call, CallExpr)
      CASE(DeclRef, DeclRefExpr)
      CASE(FloatingLiteral, FloatingLiteral)
      CASE(RegexLiteral, RegexLiteral);
      CASE(StringLiteral, StringLiteral)
      CASE(UnaryOperator, UnaryOperator)
#undef CASE
    }
  }

  Value Visit(ArraySubscriptExpr *A) { return A->getLHS()[A->getRHS()]; }

  Value Visit(BinaryOperator *B) {
    switch (B->getOpcode().getKind()) {
    default:
      assert(0 && "unimplemented operation");
      exit(EXIT_FAILURE);
#define CASE(TOK, OP)                                                          \
  case TOK:                                                                    \
    return Visit(B->getLHS()) OP Visit(B->getRHS())
      CASE(tok::plus, +);
      CASE(tok::minus, -);
      CASE(tok::star, *);
      CASE(tok::slash, /);
    }
  }

#if 0
#endif
};
} // namespace cawk
