#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Exec/SymbolTable.h"
#include "Exec/Value.h"
#include "Support/Support.h"

#include <algorithm>
#include <cstdint>
#include <list>
#include <ranges>
#include <utility>

namespace cawk {
class Exec {
  SymbolTable<FunctionDecl> Functions;
  SymbolTable<Value> Globals;
  SymbolTable<Value> Locals;
  std::vector<Value> Fields;
  Value NullValue;
  Value ReturnValue;
  std::uint32_t NestedLevel = 0;
  bool ShouldBreak = false;
  bool ShouldContinue = false;
  bool IsBegin = true;
  bool IsEnd = false;

public:
  void run(TranslationUnitDecl *T) {
    visit(T);
    std::swap(IsBegin, IsEnd);
    visit(T);
  }

private:
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
                                         return S + visit(E).toString();
                                       })
                    .c_str());
    } else {
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
        ptr_cast<DeclRefExpr>(C->getCallee())->getIdentifier().getLiteralData();
    assert(Functions.contains(Callee.data()) &&
           "awk: calling undefined function");
    const auto &Fn = Functions.get(Callee.data());
    const auto &Params = Fn.getParams();
    const auto &Args = C->getArgs();

    assert(std::size(Args) <= std::size(Params) &&
           "awk: function f called with x args, uses only y");

    auto Save = std::move(Locals);
    Locals = {};

    int I{};
    for (Expr *E : Args)
      Locals.set(Params[I++]->getIdentifier().getIdentifier().data(), visit(E));

    for (const auto N = std::size(Params); I < N; ++I)
      Locals.set(Params[I]->getIdentifier().getLiteralData().data(), {});

    visit(const_cast<CompoundStmt *>(Fn.getBody()));

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
    return Value(S->getValue().getLiteralData());
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
                      .getLiteralData();
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

  static bool isConversionSpecifier(char C) {
    switch (C) {
    default:
      return false;
    case '%':
    case 'c':
    case 's':
    case 'd':
    case 'i':
    case 'o':
    case 'x':
    case 'X':
    case 'u':
    case 'f':
    case 'F':
    case 'e':
    case 'E':
    case 'a':
    case 'A':
    case 'g':
    case 'G':
    case 'n':
    case 'p':
      return true;
    }
  }

  std::string format(std::span<char> FormatString, std::vector<Value> Args) {
    std::string String;
    static constexpr auto MaxSize = 24'576;
    String.reserve(MaxSize);

    auto It = std::begin(FormatString);

    for (Value &A : Args) {
      It = std::find(It, std::end(FormatString), '%');
      if (It == std::cend(FormatString)) {
        assert(0 && "Invalid format string.");
        exit(EXIT_FAILURE);
      }

      String.append(std::begin(FormatString), It);

      auto Next =
          std::find_if(It + 1, std::end(FormatString), isConversionSpecifier);
      if (Next == std::end(FormatString)) {
        assert(0 && "Invalid format string.");
        exit(EXIT_FAILURE);
      }

      if (*Next == '%') {
        String.push_back('%');
        continue;
      }

      auto Save = std::exchange(*(Next + 1), '\0');
      enum ConversionKind { CK_Number, CK_String, CK_Char };

      auto Kind = *Next == 's' ? CK_String : *Next == 'c' ? CK_Char : CK_Number;

      switch (*Next) {
      default:
        std::snprintf(std::end(String).base(), MaxSize - std::size(String),
                      It.base(), A.getNumber());
        break;
      case 's':
        std::snprintf(std::end(String).base(), MaxSize - std::size(String),
                      It.base(), A.getString().c_str());
        break;
      case 'c':
        if (A.getKind() == Value::VK_Number)
          std::snprintf(std::end(String).base(), MaxSize - std::size(String),
                        It.base(), static_cast<char>(A.getNumber()));
        else if (!std::empty(A.toString()))
          std::snprintf(std::end(String).base(), MaxSize - std::size(String),
                        It.base(), A.toString().front());
      }

      *(Next + 1) = Save;
      Next += 2;
    }

    It = std::find(It, std::end(FormatString), '%');

    if (It != std::end(FormatString)) {
      assert(0 && "Invalid format string.");
      exit(EXIT_FAILURE);
    }

    return std::move(String);
  }
};

} // namespace cawk
