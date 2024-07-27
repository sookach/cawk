#pragma once

#include "AST/AST.h"
#include "Exec/SymbolTable.h"
#include "Support/Support.h"

#include <cstdio>
#include <ranges>
#include <string_view>
#include <utility>
#include <vector>

namespace cawk {

class SemaType {
  enum TypeKind {
    TK_Primitive,
    TK_Array,
    TK_Function,
    TK_Any,
  };

  static bool equal(TypeKind T1, TypeKind T2) {
    return T1 == T2 || T1 == TK_Any || T2 == TK_Any;
  }

  static TypeKind resultType(TypeKind T1, TypeKind T2) {
    return T1 == T2 ? T1
           : T1 == TK_Any || T2 == TK_Any
               ? TK_Any
               : (assert(0 && "invalid types passed to resultType"), TK_Any);
  }

  template <typename T, typename... Ts>
  static bool is(TypeKind Type, T K, Ts... Ks) {
    if (Type == K)
      return true;

    if constexpr (sizeof...(Ks) != 0)
      return is(Type, Ks...);

    return false;
  }

  struct TypeResult {
    TypeKind Type;
    bool Ok = false;
  };

  BasicSymbolTable<std::vector<TypeKind>> FunctionPrototypes;
  BasicSymbolTable<TypeKind> Globals;
  BasicSymbolTable<TypeKind> Locals;

public:
  bool visit(Decl *D) {
    switch (D->getKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Decl::DK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(D))
      CASE(TranslationUnit, TranslationUnitDecl);
      CASE(Function, FunctionDecl);
      CASE(Rule, RuleDecl);
#undef CASE
    default:
      return true;
    }
  }

  bool visit(Stmt *S) {
    switch (S->getKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Stmt::SK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(S))
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

  TypeResult visit(Expr *E) {
    switch (E->getKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Expr::EK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(E));
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

  bool visit(TranslationUnitDecl *T) {
    for (Decl *D : T->getDecls() | std::views::filter([](Decl *D) {
                     return isa<FunctionDecl>(D);
                   })) {
      auto F = static_cast<FunctionDecl *>(D);
      auto Iden = F->getIdentifier().getIdentifier();
      if (Globals.contains(Iden)) {
        std::fprintf(stderr,
                     "awk: you can't define function %s more than once\n",
                     Iden.data());
        return false;
      }
      Globals.set(Iden, TK_Function);
    }

    for (Decl *D : T->getDecls())
      if (!visit(D))
        return false;
    return true;
  }

  bool visit(FunctionDecl *F) {
    Locals = BasicSymbolTable<TypeKind>();

    for (ParamVarDecl *P : F->getParams())
      Locals.set(P->getIdentifier().getIdentifier(), TK_Any);

    return visit(F->getBody());
  }

  bool visit(RuleDecl *R) {
    return visit(R->getPattern()).Ok && visit(R->getAction());
  }

  bool visit(BreakStmt *B) { return true; }

  bool visit(ContinueStmt *C) { return true; }

  bool visit(CompoundStmt *C) {
    for (Stmt *S : C->getBody())
      if (!visit(S))
        return false;
    return true;
  }

  bool visit(DeleteStmt *D) {
    return isa<DeclRefExpr>(D->getArgument()) &&
               getType(static_cast<DeclRefExpr *>(D->getArgument())
                           ->getIdentifier()
                           .getIdentifier()) == TK_Array ||
           isa<ArraySubscriptExpr>(D->getArgument());
  }

  bool visit(DoStmt *D) {
    return visit(D->getBody()) && visit(D->getCond()).Ok;
  }

  bool visit(ExitStmt *E) {
    if (E->getValue() != nullptr) {
      auto [Type, Ok] = visit(E->getValue());
      return Ok && is(Type, TK_Primitive, TK_Any);
    }
    return true;
  }

  TypeKind getType(std::string_view S) {
    return Locals.contains(S) ? Locals.get(S) : Globals.get(S);
  }

  void setType(std::string_view S, TypeKind T) {
    if (Locals.contains(S))
      Locals.set(S, T);
    Globals.set(S, T);
  }

  TypeResult visit(ArraySubscriptExpr *A) {
    auto [Type, Ok] = visit(A->getLHS());
    if (!Ok || !equal(Type, TK_Array))
      return {};
    for (Expr *E : A->getRHS()) {
      auto [Type, Ok] = visit(E);
      if (!Ok || !is(Type, TK_Primitive, TK_Any))
        return {};
    }
    return {TK_Any, true};
  }

  TypeResult visit(BinaryOperator *B) {
    auto [LHSType, LHSResult] = visit(B->getLHS());
    auto [RHSType, RHSResult] = visit(B->getRHS());

    if (!LHSResult || !RHSResult)
      return {};

    switch (B->getOpcode().getKind()) {
    case tok::equal:
    case tok::starequal:
    case tok::slashequal:
    case tok::percentequal:
    case tok::plusequal:
    case tok::minusequal:
    case tok::caretequal:
      if (!is(LHSType, TK_Primitive, TK_Any) ||
          !is(RHSType, TK_Primitive, TK_Any))
        return {};
      assert(isa<DeclRefExpr>(B->getLHS()));
      setType(static_cast<DeclRefExpr *>(B->getLHS())
                  ->getIdentifier()
                  .getIdentifier(),
              TK_Primitive);
      return TypeResult(TK_Primitive, true);
    case tok::pipepipe:
    case tok::ampamp:
      if (!is(LHSType, TK_Primitive, TK_Any) ||
          !is(RHSType, TK_Primitive, TK_Any))
        return {};
      return TypeResult(TK_Primitive, true);
    case tok::kw_in:
      if (!is(RHSType, TK_Array, TK_Any))
        return {};
      return TypeResult(TK_Any, true);
    case tok::tilde:
    case tok::exclaimtilde:
    case tok::greater:
    case tok::less:
    case tok::greaterequal:
    case tok::lessequal:
    default: // String concatenation
    case tok::plus:
    case tok::minus:
    case tok::star:
    case tok::slash:
    case tok::percent:
    case tok::caret:
    case tok::starstar:
      return TypeResult(TK_Primitive, true);
      if (!is(LHSType, TK_Primitive, TK_Any) ||
          !is(RHSType, TK_Primitive, TK_Any))
        return {};
      return TypeResult(TK_Primitive, true);
    }
  }

  TypeResult visit(CallExpr *C) {
    auto [Type, Ok] = visit(C->getCallee());
    if (!Ok || !is(Type, TK_Function))
      return {};
    auto Iden = static_cast<DeclRefExpr *>(C->getCallee())
                    ->getIdentifier()
                    .getIdentifier();
    auto ParameterTypes = FunctionPrototypes.get(Iden);
    if (std::size(C->getArgs()) > std::size(ParameterTypes))
      return {};

    for (int I = 0; I != std::size(C->getArgs()); ++I) {
      auto [Type, Ok] = visit(C->getArgs()[I]);
      if (!Ok || !equal(Type, ParameterTypes[I]))
        return {};
    }

    return {TK_Primitive, true};
  }

  TypeResult visit(DeclRefExpr *D) {
    return {getType(D->getIdentifier().getIdentifier()), true};
  }

  TypeResult visit(FloatingLiteral *F) { return {TK_Primitive, true}; }

  TypeResult visit(RegexLiteral *R) { return {TK_Primitive, true}; }

  TypeResult visit(StringLiteral *S) { return {TK_Primitive, true}; }

  TypeResult visit(UnaryOperator *U) {
    auto [Type, Ok] = visit(U->getSubExpr());
    if (!Ok || !is(Type, TK_Any, TK_Primitive))
      return {};
    return {TK_Primitive, true};
  }
};

} // namespace cawk