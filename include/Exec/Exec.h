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
  Value ReturnValue;

public:
  void visit(TranslationUnitDecl *T) {
    for (Decl *D : T->getDecls() | std::views::filter([](Decl *D) {
                     return isa<RuleDecl>(D);
                   }))
      visit(static_cast<RuleDecl *>(D));
  }

  void visit(RuleDecl *R) {
    if (visit(R->getPattern()))
      visit(R->getAction());
  }

  void visit(Stmt *S) {
    switch (S->getKind()) {
#if defined(CASE)
      static_assert(false);
#else
#define CASE(KIND, CLASS)                                                      \
  case Stmt::SK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(S))
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

  Value visit(Expr *E) {
    switch (E->getKind()) {
#if defined(CASE)
      static_assert(false);
#else
#define CASE(KIND, CLASS)                                                      \
  case Expr::EK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(E));
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

  Value visit(ArraySubscriptExpr *A) {
    return visit(A->getLHS())[visit(A->getRHS())];
  }

  Value visit(BinaryOperator *B) {
    switch (B->getOpcode().getKind()) {
    default:
      assert(0 && "unimplemented operation");
      exit(EXIT_FAILURE);
#define CASE(TOK, OP)                                                          \
  case TOK:                                                                    \
    return visit(B->getLHS()) OP visit(B->getRHS())
      CASE(tok::plus, +);
      CASE(tok::minus, -);
      CASE(tok::star, *);
      CASE(tok::slash, /);
    }
  }

  Value visit(CallExpr *C) {
    assert(isa<DeclRefExpr>(C->getCallee()) && "Invalid function call.");
    auto Callee =
        ptr_cast<DeclRefExpr>(C->getCallee())->getIdentifier().GetLiteralData();
    assert(Functions.contains(Callee.data()) &&
           "awk: calling undefined function");
    const auto &Fn = Functions[Callee.data()];
    const auto &Params = Fn.getParams();
    const auto &Args = C->getArgs();

    assert(std::size(Args) <= std::size(Params) &&
           "awk: function f called with x args, uses only y");

    auto Save = std::move(LocalSymbolTable);
    LocalSymbolTable = {};

    for (int i{}; Expr * E : Args)
      LocalSymbolTamble[Params[i++]->getIdentifier().GetLiteralData().data()] =
          visit(E);

    visit(Fn.getBody());

    LocalSymbolTable = std::move(Save);

    return std::move(ReturnValue);
  }

#if 0
#endif
};
} // namespace cawk
