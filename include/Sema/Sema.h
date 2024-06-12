#pragma once

#include "AST/AST.h"
#include "Support/Support.h"
#include <ranges>

namespace cawk {
class Sema {
  std::unordered_map<std::string, FunctionDecl *> Functions;
  std::unordered_map<std::string, Decl *> GlobalVars;

  class CheckType {
  private:
    enum TypeKind { TK_Primitive, TK_Composite };

    using Result = std::pair<bool, TypeKind>;

    std::unordered_map<std::string, FunctionDecl *> Functions;
    std::unordered_map<std::string, Decl *> GlobalVars;
    std::unordered_map<std::string, TypeKind> GlobalTypes;
    std::unordered_map<std::string, TypeKind> LocalTypes;
    std::unordered_map<std::string, std::pair<TypeKind, Sequence<TypeKind>>>
        FunctionTypes;

    Result Check(Decl *D) {
      switch (D->GetKind()) {
      case Decl::DK_TranslationUnit:
        return Check(static_cast<TranslationUnitDecl *>(D));
      case Decl::DK_Function:
        return Check(static_cast<FunctionDecl *>(D));
      case Decl::DK_Rule:
        return Check(static_cast<RuleDecl *>(D));
      case Decl::DK_Var:
        return Check(static_cast<VarDecl *>(D));
      case Decl::DK_ParamVar:
        return Check(static_cast<ParamVarDecl *>(D));
      }
    }

    Result Check(FunctionDecl *D) { return Check(D->GetBody()); }

    Result Check(Stmt *S) {
      switch (S->GetKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Stmt::SK_##KIND:                                                        \
    return Check(static_cast<CLASS *>(S))
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

    Result Check(BreakStmt *S) { return {true, {}}; }

    Result Check(ContinueStmt *S) { return {true, {}}; }

    Result Check(CompoundStmt *S) {
      return {std::ranges::fold_left(
                  S->GetBody(), true,
                  [this](bool B, Stmt *S) { return B && Check(S).first; }),
              {}};
    }

    Result Check(ExitStmt *S) { return Check(S->GetValue()); }

    Result Check(Expr *E) {
      switch (E->GetKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Expr::EK_##KIND:                                                        \
    return Check(static_cast<CLASS *>(E))
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

    Result Check(ArraySubscriptExpr *E) {
      auto [B1, T1] = Check(E->GetLHS());
      auto [B2, T2] = Check(E->GetRHS());

      return {B1 && B2 && T1 == TK_Composite && T2 == TK_Primitive,
              TK_Primitive};
    }

    Result Check(BinaryOperator *E) {
      auto [B1, T1] = Check(E->GetLHS());
      auto [B2, T2] = Check(E->GetRHS());

      return {B1 && B2 && T1 == TK_Primitive && T2 == TK_Primitive,
              TK_Primitive};
    }

    Result Check(CallExpr *E) {
      if (!isa<DeclRefExpr>(E->GetCallee()))
        return {};

      auto Callee = ptr_cast<DeclRefExpr>(E->GetCallee())->GetIdentifier();
      std::string CalleeName(Callee.GetLiteralData());

      if (!Functions.contains(Callee.GetLiteralData().data()))
        return {};

      if (std::size(E->GetArgs()) >
          std::size(Functions[CalleeName]->GetParams()))
        return {};

      for (auto [Arg, ParamT] :
           std::views::zip(E->GetArgs(), FunctionTypes[CalleeName].second))
        if (auto [B, ArgT] = Check(Arg); !B || ArgT != ParamT)
          return {};

      return {true, FunctionTypes[CalleeName].first};
    }

    Result Check(DeclRefExpr *E) {
      return {true, GlobalTypes[E->GetIdentifier().GetLiteralData().data()]};
    }

    Result Check(FloatingLiteral *E) { return {true, TK_Primitive}; }
  };
};
} // namespace cawk
