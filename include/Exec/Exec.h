#pragma once

#include "AST/AST.h"
#include "Exec/Value.h"
#include "Support/Support.h"

#include <ranges>

namespace cawk {
class Exec {
  std::unordered_map<std::string_view, Stmt *> Functions;

public:
  void Visit(TranslationUnitDecl *T) {
    for (Decl *D : T->GetDecls() | std::views::filter([](Decl *D) {
                     return isa<Decl::DK_Rule>(*D);
                   }))
      Visit(static_cast<RuleDecl *>(D));
  }

  void Visit(RuleDecl *R) {
    if (Visit(R->GetPattern()))
      Visit(R->GetAction());
  }

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

#if 0

class Decl;
class TranslationUnitDecl;
class RuleDecl;
class FunctionDecl;
class VarDecl;
class ParamVarDecl;

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
