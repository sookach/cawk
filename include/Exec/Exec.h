#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Exec/SymbolTable.h"
#include "Exec/Value.h"
#include "Support/Format.h"
#include "Support/Support.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <list>
#include <numeric>
#include <ranges>
#include <regex>
#include <utility>

namespace cawk {
class Exec {
  static std::unique_ptr<Exec> Process;

  TranslationUnitDecl *AST;
  BasicSymbolTable<FunctionDecl *> Functions;
  BasicSymbolTable<Value> Globals;
  BasicSymbolTable<Value> Locals;
  std::vector<Value> Fields;
  Value NullValue;
  Value ReturnValue;
  std::uint32_t NestedLevel = 0;
  bool ShouldBreak = false;
  bool ShouldContinue = false;
  bool IsBegin = true;
  bool IsEnd = false;

private:
  Exec() = default;

public:
  static void load(TranslationUnitDecl *T) {
    Process = std::unique_ptr<Exec>(new Exec);

    Process->AST = T;

    for (const Decl *F : T->getDecls() | std::views::filter([](const Decl *D) {
                           return isa<FunctionDecl>(D);
                         }))
      Process->addFunction(ptr_cast<const FunctionDecl>(F));
  }

  static void exec() { Process->operator()(); }

private:
  void addFunction(const FunctionDecl *F) {
    Functions.set(F->getIdentifier().getIdentifier(),
                  const_cast<FunctionDecl *>(F));
  }

  void operator()() {
    IsBegin = true;
    IsEnd = false;
    visit(AST);
    std::swap(IsBegin, IsEnd);
    visit(AST);
  }

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

  void visit(BreakStmt *B) {
    assert(NestedLevel-- != 0 && "awk: break illegal outside of loops");
  }

  void visit(ContinueStmt *C) {
    assert(NestedLevel != 0 && "awk: continue illegal outside of loops");
  }

  void visit(CompoundStmt *C) {
    for (Stmt *S : C->getBody())
      visit(S);
  }

  void visit(DeleteStmt *D) {
    if (isa<DeclRefExpr>(D->getArgument())) {
      getValue(ptr_cast<DeclRefExpr>(D->getArgument())).clear();
    } else if (isa<ArraySubscriptExpr>(D->getArgument())) {
      getValue(ptr_cast<DeclRefExpr>(
                   ptr_cast<ArraySubscriptExpr>(D->getArgument())->getLHS()))
          .erase(
              visit(ptr_cast<ArraySubscriptExpr>(D->getArgument())->getRHS()));
    } else {
      assert(0 && "awk: delete illegal for non-arrays");
      exit(EXIT_FAILURE);
    }
  }

  void visit(DoStmt *D) { assert(0 && "unimplemented"); }

  void visit(ExitStmt *E) { std::exit(visit(E->getValue())); }

  void visit(ForStmt *F) {
    if (F->getInit() != nullptr)
      visit(F->getInit());

    for (;;) {
      if (F->getCond() != nullptr && !visit(F->getCond()))
        break;

      if (F->getBody() != nullptr)
        visit(F->getBody());

      if (std::exchange(ShouldBreak, false))
        break;

      if (F->getInc() != nullptr)
        visit(F->getInc());
    }
  }

  void visit(ForRangeStmt *F) {
    auto LoopVar = F->getLoopVar()->getIdentifier().getLiteralData();
    for (auto &[Key, Value] : getValue(F->getRange()).toArray()) {
      getValue(LoopVar) = Key;
      visit(F->getBody());
    }
  }

  void visit(IfStmt *I) {
    if (visit(I->getCond()))
      visit(I->getThen());
    else if (I->getElse() != nullptr)
      visit(I->getElse());
  }

  void visit(NextStmt *N) { assert(0 && "unimplemented"); }

  void visit(NextfileStmt *N) { assert(0 && "unimplemented"); }

  void visit(PrintStmt *P) {
    assert(P->getOpcode().is(tok::unknown) && P->getOutput() == nullptr &&
           "unimplemented");
    if (P->getIden().is(tok::kw_print)) {
      std::puts(std::ranges::fold_left(P->getArgs(), std::string(),
                                       [this](std::string S, Expr *E) {
                                         return S + visit(E).toString() + ' ';
                                       })
                    .c_str());
    } else {
      if (!std::empty(P->getArgs())) {
        auto Args = std::ranges::fold_left(
            P->getArgs() | std::views::drop(1), std::vector<Value>(),
            [this](std::vector<Value> Args, Expr *E) {
              Args.push_back(visit(E));
              return Args;
            });
        auto S = format(visit(P->getArgs().front()).toString(), Args);
        std::printf("%s", S.c_str());
      }
    }
  }

  void visit(ValueStmt *V) { visit(V->getValue()); }

#if 0
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
#endif

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
      assert(0 && "Invalid binary operation");
      exit(EXIT_FAILURE);
#define CASE(TOK, OP)                                                          \
  case TOK:                                                                    \
    return visit(B->getLHS()) OP visit(B->getRHS())
      CASE(tok::plus, +);
      CASE(tok::minus, -);
      CASE(tok::star, *);
      CASE(tok::slash, /);
      CASE(tok::equalequal, ==);
      CASE(tok::exclaimequal, !=);
    case tok::equal:
      assert(isa<DeclRefExpr>(B->getLHS()) && "Cannot assign to non-lvalue.");
      setValue(ptr_cast<DeclRefExpr>(B->getLHS()), visit(B->getRHS()));
      return getValue(ptr_cast<DeclRefExpr>(B->getLHS()));
#undef CASE
    }
  }

  Value visit(CallExpr *C) {
    assert(isa<DeclRefExpr>(C->getCallee()) && "Invalid function call.");
    auto Callee =
        ptr_cast<DeclRefExpr>(C->getCallee())->getIdentifier().getIdentifier();
    assert(Functions.contains(Callee) && "awk: calling undefined function");
    const auto &Fn = Functions.get(Callee);
    const auto &Params = Fn->getParams();
    const auto &Args = C->getArgs();

    assert(std::size(Args) <= std::size(Params) &&
           "awk: function f called with x args, uses only y");

    auto Save = std::move(Locals);
    Locals = {};

    int I = 0;
    for (Expr *E : Args)
      Locals.set(Params[I++]->getIdentifier().getIdentifier().data(), visit(E));

    for (const auto N = std::size(Params); I < N; ++I)
      Locals.set(Params[I]->getIdentifier().getLiteralData().data(), {});

    visit(const_cast<CompoundStmt *>(Fn->getBody()));

    Locals = std::move(Save);

    return std::move(ReturnValue);
  }

  Value visit(DeclRefExpr *D) {
    switch (D->getIdentifier().getKind()) {
    default:
      return getValue(D);
    case tok::kw_BEGIN:
      return IsBegin;
    case tok::kw_END:
      return IsEnd;
    }
  }

  Value visit(FloatingLiteral *F) {
    return std::stod(F->getValue().getLiteralData().data());
  }

  Value visit(RegexLiteral *R) { return Value(0); }

  Value visit(StringLiteral *S) {
    std::string String(S->getValue().getLiteralData());
    String = std::regex_replace(String, std::regex(R"(\\')"), "'");
    String = std::regex_replace(String, std::regex(R"(\\")"), "\"");
    String = std::regex_replace(String, std::regex(R"(\\\?)"), "?");
    String = std::regex_replace(String, std::regex(R"(\\\\)"), "\\");
    String = std::regex_replace(String, std::regex(R"(\\a)"), "\a");
    String = std::regex_replace(String, std::regex(R"(\\b)"), "\b");
    String = std::regex_replace(String, std::regex(R"(\\f)"), "\f");
    String = std::regex_replace(String, std::regex(R"(\\n)"), "\n");
    String = std::regex_replace(String, std::regex(R"(\\r)"), "\r");
    String = std::regex_replace(String, std::regex(R"(\\t)"), "\t");
    String = std::regex_replace(String, std::regex(R"(\\v)"), "\v");
    return Value(std::string(std::cbegin(String) + 1, std::cend(String) - 1));
  }

  Value visit(UnaryOperator *U) {
    switch (U->getOpcode().getKind()) {
    default:
      assert(0 && "Invalid Unary Operation.");
      exit(EXIT_FAILURE);
    case tok::plusplus: {
      assert(isa<DeclRefExpr>(U->getSubExpr()) &&
             "++ can only be performed on variables.");
      auto Name = ptr_cast<DeclRefExpr>(U->getSubExpr())
                      ->getIdentifier()
                      .getIdentifier();
      return U->getFix() == UnaryOperator::Prefix ? ++getValue(Name)
                                                  : getValue(Name)++;
    }
    case tok::minusminus: {
      assert(isa<DeclRefExpr>(U->getSubExpr()) &&
             "-- can only be performed on variables.");
      auto Name = ptr_cast<DeclRefExpr>(U->getSubExpr())
                      ->getIdentifier()
                      .getLiteralData();
      return U->getFix() == UnaryOperator::Prefix ? --getValue(Name)
                                                  : getValue(Name)--;
    }
    case tok::exclaim:
      return !visit(U->getSubExpr());
    case tok::dollar:
      return getField(visit(U->getSubExpr()).toNumber());
    }
  }

  Value &getValue(std::string_view Name) {
    return Locals.contains(Name)    ? Locals.get(Name)
           : Globals.contains(Name) ? Globals.get(Name)
                                    : NullValue;
  }

  Value &getValue(DeclRefExpr *E) {
    return getValue(E->getIdentifier().getIdentifier());
  }

  void setValue(std::string_view Name, Value V) {
    if (Locals.contains(Name))
      Locals.set(Name, V);
    else
      Globals.set(Name, V);
  }

  void setValue(DeclRefExpr *D, Value V) {
    setValue(D->getIdentifier().getIdentifier(), V);
  }

  Value &getField(std::size_t I) {
    return std::clamp(I, 0UL, std::size(Fields) - 1) == I ? Fields[I]
                                                          : NullValue;
  }
};

} // namespace cawk
