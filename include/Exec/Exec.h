#pragma once

#include "AST/AST.h"
#include "Exec/Value.h"
#include "Support/Support.h"

#include <cmath>
#include <ranges>
#include <regex>

namespace cawk {
class Exec {
  std::unordered_map<std::string_view, Stmt *> Functions;

public:
  void Visit(TranslationUnitDecl *T) {
    for (Decl *D : T->GetDecls() | std::views::filter([](Decl *D) {
                     return isa<RuleDecl>(D);
                   }))
      Visit(static_cast<RuleDecl *>(D));
  }

  void Visit(RuleDecl *R) {
    if (Visit(R->GetPattern()))
      Visit(R->GetAction());
  }

  void Visit(Stmt *S) {}

  Value *Visit(Expr *E) {
    switch (E->GetKind()) {
#if defined(CASE)
      static_assert(false);
#else
#define CASE(KIND, CLASS)                                                      \
  case Expr::EK_##KIND:                                                        \
    return Visit(static_cast<CLASS *>(E));
#endif
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
    switch (B->GetOpcode().GetKind()) {
    default:
      std::terminate();
    case tok::plus:
      return Number::Create(raw_cast<double>(Visit(B->GetLHS())) +
                            raw_cast<double>(Visit(B->GetRHS())));
    case tok::minus:
      return Number::Create(raw_cast<double>(Visit(B->GetLHS())) -
                            raw_cast<double>(Visit(B->GetRHS())));
    case tok::star:
      return Number::Create(raw_cast<double>(Visit(B->GetLHS())) *
                            raw_cast<double>(Visit(B->GetRHS())));
    case tok::slash:
      return Number::Create(raw_cast<double>(Visit(B->GetLHS())) /
                            raw_cast<double>(Visit(B->GetRHS())));
    case tok::caret:
      return Number::Create(std::pow(raw_cast<double>(Visit(B->GetLHS())),
                                     raw_cast<double>(Visit(B->GetRHS()))));
    case tok::tilde:
      return Number::Create(std::regex_search(
          raw_cast<std::string>(Visit(B->GetLHS())),
          std::regex(raw_cast<std::string>(Visit(B->GetRHS())))));
    case tok::ampamp:
      return Number::Create(raw_cast<bool>(Visit(B->GetLHS())) &&
                            raw_cast<bool>(Visit(B->GetRHS())));
    case tok::pipepipe:
      return Number::Create(raw_cast<bool>(Visit(B->GetLHS())) ||
                            raw_cast<bool>(Visit(B->GetRHS())));
    }
  }

#if 0


class Stmt;
class BreakStmt;
class ContinueStmt;
class CompoundStmt;
class DeclStmt;
class DoStmt;
class ExitStmt;
class ForStmt;
class ForRangeStmt;
class IfStmt;
class NextStmt;
class NextfileStmt;
class PrintStmt;
class ReturnStmt;
class ValueStmt;
class WhileStmt;

class Expr;
class BinaryOperator;
class CallExpr;
class DeclRefExpr;
class FloatingLiteral;
class StringLiteral;
class UnaryOperator;
#endif
};
} // namespace cawk
