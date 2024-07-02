#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Exec/Value.h"
#include "Support/Support.h"

#include <cmath>
#include <ranges>
#include <regex>

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
    return Visit(static_cast<CLASS *>(S));
#endif
      CASE(Break, BreakStmt)
      CASE(Continue, ContinueStmt)
      CASE(Compound, CompoundStmt)
      CASE(Do, DoStmt);
      CASE(Exit, ExitStmt)
      CASE(For, ForStmt)
      CASE(ForRange, ForRangeStmt)
      CASE(If, IfStmt)
      CASE(Next, NextStmt)
      CASE(Nextfile, NextfileStmt)
      CASE(Print, PrintStmt)
      CASE(Return, ReturnStmt)
      CASE(Value, ValueStmt)
      CASE(While, WhileStmt)
#undef CASE
    }
  }

  Value *Visit(Expr *E) {
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

  Value *Visit(BinaryOperator *B) {
    switch (B->getOpcode().getKind()) {
    default:
      std::terminate();
    case tok::plus:
      return Number::Create(raw_cast<double>(Visit(B->getLHS())) +
                            raw_cast<double>(Visit(B->getRHS())));
    case tok::minus:
      return Number::Create(raw_cast<double>(Visit(B->getLHS())) -
                            raw_cast<double>(Visit(B->getRHS())));
    case tok::star:
      return Number::Create(raw_cast<double>(Visit(B->getLHS())) *
                            raw_cast<double>(Visit(B->getRHS())));
    case tok::slash:
      return Number::Create(raw_cast<double>(Visit(B->getLHS())) /
                            raw_cast<double>(Visit(B->getRHS())));
    case tok::caret:
      return Number::Create(std::pow(raw_cast<double>(Visit(B->getLHS())),
                                     raw_cast<double>(Visit(B->getRHS()))));
    case tok::tilde:
      return Number::Create(std::regex_search(
          raw_cast<std::string>(Visit(B->getLHS())),
          std::regex(raw_cast<std::string>(Visit(B->getRHS())))));
    case tok::exclaimtilde:
      return Number::Create(!std::regex_search(
          raw_cast<std::string>(Visit(B->getLHS())),
          std::regex(raw_cast<std::string>(Visit(B->getRHS())))));
    case tok::kw_in:
      break;
    case tok::ampamp:
      return Number::Create(raw_cast<bool>(Visit(B->getLHS())) &&
                            raw_cast<bool>(Visit(B->getRHS())));
    case tok::pipepipe:
      return Number::Create(raw_cast<bool>(Visit(B->getLHS())) ||
                            raw_cast<bool>(Visit(B->getRHS())));
    }
  }

#if 0
#endif
};
} // namespace cawk
