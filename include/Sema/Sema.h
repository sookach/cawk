#pragma once

#include "AST/AST.h"
#include "Support/StringMap.h"
#include "Support/Support.h"

#include <cstdio>
#include <ranges>
#include <string_view>
#include <utility>
#include <vector>

namespace cawk {

class SemaDecl {
  StringMap<FunctionDecl *> FunctionMap;
  std::vector<FunctionDecl *> FunctionDecls;
  std::vector<RuleDecl *> RuleDecls;

public:
  bool visit(Stmt *S) {
    switch (S->getKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Stmt::SK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(S))
      CASE(Compound, CompoundStmt);
      CASE(Delete, DeleteStmt);
      CASE(Do, DoStmt);
      CASE(Exit, ExitStmt);
      CASE(For, ForStmt);
      CASE(ForRange, ForRangeStmt);
      CASE(If, IfStmt);
      CASE(Print, PrintStmt);
      CASE(Return, ReturnStmt);
      CASE(Value, ValueStmt);
      CASE(While, WhileStmt);
#undef CASE
    default:
      return true;
    }
  }

  bool visit(Expr *E) {
    switch (E->getKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Expr::EK_##KIND:                                                        \
    return visit(static_cast<CLASS *>(E));
      CASE(Call, CallExpr)
#undef CASE
    default:
      return true;
    }
  }

  bool visit(CompoundStmt *C) {
    for (Stmt *S : C->getBody())
      if (!visit(S))
        return false;
    return true;
  }

  bool visit(DeleteStmt *D) { return visit(D->getArgument()); }

  bool visit(DoStmt *D) { return visit(D->getBody()) && visit(D->getCond()); }

  bool visit(ExitStmt *E) {
    if (E->getValue() != nullptr)
      return visit(E->getValue());
    return true;
  }

  bool visit(ForStmt *F) {
    if (F->getInit() != nullptr && !visit(F->getInit()))
      return false;
    if (F->getCond() != nullptr && !visit(F->getCond()))
      return false;
    if (F->getInc() != nullptr && !visit(F->getInc()))
      return false;
    if (F->getBody() != nullptr && !visit(F->getBody()))
      return false;
    return true;
  }

  bool visit(ForRangeStmt *F) {
    if (F->getBody() != nullptr)
      return visit(F->getBody());
    return true;
  }

  bool visit(PrintStmt *P) {
    for (Expr *E : P->getArgs())
      if (!visit(E))
        return false;
    return true;
  }

  bool visit(ReturnStmt *R) {
    if (R->getValue() != nullptr)
      return visit(R->getValue());
    return true;
  }

  bool visit(ValueStmt *V) { return visit(V->getValue()); }

  bool visit(WhileStmt *W) {
    return visit(W->getCond()) && visit(W->getBody());
  }

  bool visit(TranslationUnitDecl *T) {
    for (Decl *D : T->getDecls()) {
      if (isa<FunctionDecl>(D)) {
        auto F = static_cast<FunctionDecl *>(D);
        assert(!FunctionMap.contains(F->getIdentifier().getIdentifier()));
        FunctionMap[F->getIdentifier().getIdentifier()] = F;
        FunctionDecls.push_back(F);
      } else {
        assert(isa<RuleDecl>(D));
        RuleDecls.push_back(static_cast<RuleDecl *>(D));
      }
    }

    for (RuleDecl *R : RuleDecls)
      if (!visit(R))
        return false;

    return true;
  }

  bool visit(RuleDecl *R) {
    if (R->getPattern() != nullptr && !visit(R->getPattern()))
      return false;
    if (R->getAction() != nullptr)
      return visit(R->getAction());
    return true;
  }

  StringMap<FunctionDecl *> getFunctionMap() { return FunctionMap; }

  std::vector<FunctionDecl *> getFunctionDecls() { return FunctionDecls; }

  std::vector<RuleDecl *> getRuleDecls() { return RuleDecls; }
};

class SemaType {
  enum TypeKind {
    TK_Primitive,
    TK_Array,
    TK_PrimitiveOrArray,
    TK_Function,
    TK_Null,
    TK_Any,
  };

  static bool equal(TypeKind T1, TypeKind T2) {
    switch (T1) {
    default:
      cawk_unreachable("invalid type comparison");
    case TK_Primitive:
      return T2 == TK_Primitive || T2 == TK_PrimitiveOrArray || T2 == TK_Null;
    case TK_Array:
      return T2 == TK_Array || T2 == TK_PrimitiveOrArray || T2 == TK_Null;
    case TK_PrimitiveOrArray:
      return T2 == TK_Primitive || T2 == TK_Array ||
             T2 == TK_PrimitiveOrArray || T2 == TK_Null;
    }
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

  StringMap<std::vector<TypeKind>> FunctionPrototypes;
  StringMap<TypeKind> Globals;
  StringMap<TypeKind> Locals;

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

  bool checkTranslationUnit(TranslationUnitDecl *T,
                            std::vector<FunctionDecl *> FunctionDecls,
                            std::vector<RuleDecl *> RuleDecls) {
    return true;
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
      Globals[Iden] = TK_Function;
    }

    for (Decl *D : T->getDecls())
      if (!visit(D))
        return false;
    return true;
  }

  bool visit(FunctionDecl *F) {
    Locals = StringMap<TypeKind>();

    for (ParamVarDecl *P : F->getParams())
      Locals[P->getIdentifier().getIdentifier()] = TK_Null;

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
    case tok::caretequal: {
      assert(isa<DeclRefExpr>(B->getLHS()));
      auto LHSIden = static_cast<DeclRefExpr *>(B->getLHS())
                         ->getIdentifier()
                         .getIdentifier();
      assert(getType(LHSIden) != TK_Array &&
             getType(LHSIden) !=
                 TK_PrimitiveOrArray); // This assertion really shouldn't be
                                       // necessary, might remove in the future.
      setType(LHSIden, TK_Primitive);
      auto [RHSType, RHSOk] = visit(B->getRHS());
      assert(RHSOk && equal(RHSType, TK_Primitive));
      return {TK_Primitive, true};
    }
    case tok::pipepipe:
    case tok::ampamp: {
      auto [LHSType, LHSResult] = visit(B->getLHS());
      auto [RHSType, RHSResult] = visit(B->getRHS());
      assert(LHSResult && RHSResult && LHSType != TK_Array &&
             RHSType != TK_Array);
      return {TK_Primitive, true};
    }
    case tok::kw_in: {
      auto RHSIden = static_cast<DeclRefExpr *>(B->getRHS())
                         ->getIdentifier()
                         .getIdentifier();
      assert(getType(RHSIden) != TK_Primitive);
      return {TK_Primitive, true};
    }
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
    case tok::starstar: {
      auto [LHSType, LHSOk] = visit(B->getLHS());
      auto [RHSType, RHSOk] = visit(B->getRHS());

      assert(LHSOk && RHSOk);
      assert(LHSType != TK_Array && RHSType != TK_Array);
      return {TK_Primitive, true};
    }
    }
  }

  TypeResult visit(CallExpr *C) {
    auto [Type, Ok] = visit(C->getCallee());
    if (!Ok || !is(Type, TK_Function))
      return {};
    auto Iden = static_cast<DeclRefExpr *>(C->getCallee())
                    ->getIdentifier()
                    .getIdentifier();
    auto ParameterTypes = FunctionPrototypes.at(Iden);
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

  TypeKind getType(std::string_view S) {
    return Locals.contains(S)    ? Locals.at(S)
           : Globals.contains(S) ? Globals.at(S)
                                 : TK_Null;
  }

  void setType(std::string_view S, TypeKind T) {
    if (Locals.contains(S))
      Locals[S] = T;
    Globals[S] = T;
  }
};

} // namespace cawk