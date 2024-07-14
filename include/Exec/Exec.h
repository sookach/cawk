#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Exec/SymbolTable.h"
#include "Exec/Value.h"
#include "Support/Support.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <list>
#include <numeric>
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

  std::string format(std::string FormatString, std::vector<Value> Args) {
    static constexpr std::string::size_type MaxSize = 10;
    std::array<char, MaxSize> Buffer;

    auto It = std::begin(FormatString) + 1;
    auto End = std::end(FormatString) - 1;
    auto BufferIt = std::begin(Buffer);

    for (Value &A : Args) {
      auto Next = std::find(It, End, '%');
      if (Next == End) {
        assert(0 && "Invalid format string.");
        exit(EXIT_FAILURE);
      }

      std::copy(It, Next, BufferIt);
      BufferIt += Next - It;
      It = Next;

      assert(isConversionSpecifier(*(It + 1)));

      if (*(It + 1) == '%') {
        *(BufferIt++) = '%';
        It += 2;
        continue;
      }

      auto WriteFormat = [&](const char *Fmt, auto Arg) {
        auto N = std::snprintf(
            BufferIt, MaxSize - (BufferIt - std::begin(Buffer)), Fmt, Arg);
        if (N < 0) {
          perror("");
          exit(errno);
        }
        BufferIt += N;
      };

      switch (++It; *(It++)) {
      default:
        assert(0 && "Invalid format string.");
        break;
      case '%':
        *(BufferIt++) = '%';
        break;
      case 'c':
        WriteFormat("%c", A.getKind() == Value::VK_Number
                              ? A.getNumber()
                              : static_cast<int>(A.getString().front()));
        break;
      case 's': {
        auto S = A.toString();
        WriteFormat("%s", S.c_str());
        break;
      }
      case 'd':
      case 'i':
        WriteFormat("%d", static_cast<int>(A.getNumber()));
        break;
      case 'o':
        WriteFormat("%o", static_cast<unsigned int>(A.getNumber()));
        break;
      case 'x':
      case 'X':
        WriteFormat("%x", static_cast<unsigned int>(A.getNumber()));
        break;
      case 'u':
        WriteFormat("%u", static_cast<unsigned int>(A.getNumber()));
        break;
      case 'f':
      case 'F':
        WriteFormat("%f", A.getNumber());
        break;
      case 'e':
      case 'E':
        WriteFormat("%e", A.getNumber());
        break;
      case 'a':
      case 'A':
        WriteFormat("%a", A.getNumber());
        break;
      case 'g':
      case 'G':
        WriteFormat("%g", A.getNumber());
        break;
      case 'h':
        switch (*It++) {
        default:
          assert(0 && "Invalid format string.");
        case 'h':
          switch (*It++) {
          default:
            assert(0 && "Invalid format string.");
          case 'd':
          case 'i':
            WriteFormat("%hhd", static_cast<signed char>(A.getNumber()));
            break;
          case 'o':
            WriteFormat("%hho", static_cast<unsigned char>(A.getNumber()));
            break;
          case 'x':
          case 'X':
            WriteFormat("%hhx", static_cast<unsigned char>(A.getNumber()));
            break;
          case 'u':
            WriteFormat("%hhu", static_cast<unsigned char>(A.getNumber()));
          }
          break;
        case 'd':
        case 'i':
          WriteFormat("%hd", static_cast<short>(A.getNumber()));
          break;
        case 'o':
          WriteFormat("%ho", static_cast<unsigned short>(A.getNumber()));
          break;
        case 'x':
        case 'X':
          WriteFormat("%hx", static_cast<unsigned short>(A.getNumber()));
          break;
        case 'u':
          WriteFormat("%hu", static_cast<unsigned short>(A.getNumber()));
        }
        break;
      case 'l':
        switch (*It++) {
        default:
          assert(0 && "Invalid format string.");
        case 'c':
          WriteFormat("%lc",
                      A.getKind() == Value::VK_Number
                          ? static_cast<std::wint_t>(A.getNumber())
                          : static_cast<std::wint_t>(A.getString().front()));
          break;
        case 's': {
          std::wstring S;
          std::ranges::transform(
              A.toString(), std::back_inserter(S),
              [](char C) { return static_cast<wchar_t>(C); });
          WriteFormat("%ls", S.c_str());
          break;
        }
        case 'd':
        case 'i':
          WriteFormat("%ld", static_cast<long>(A.getNumber()));
          break;
        case 'o':
          WriteFormat("%lo", static_cast<unsigned long>(A.getNumber()));
          break;
        case 'x':
        case 'X':
          WriteFormat("%lx", static_cast<unsigned long>(A.getNumber()));
          break;
        case 'u':
          WriteFormat("%lu", static_cast<unsigned long>(A.getNumber()));
          break;
        case 'f':
        case 'F':
          WriteFormat("%lf", A.getNumber());
          break;
        case 'e':
        case 'E':
          WriteFormat("%le", A.getNumber());
          break;
        case 'a':
        case 'A':
          WriteFormat("%la", A.getNumber());
          break;
        case 'g':
        case 'G':
          WriteFormat("%lg", A.getNumber());
          break;
        case 'l':
          switch (*It++) {
          default:
            assert(0 && "Invalid format string.");
          case 'd':
          case 'i':
            WriteFormat("%lld", static_cast<long long>(A.getNumber()));
            break;
          case 'o':
            WriteFormat("%llo", static_cast<unsigned long long>(A.getNumber()));
            break;
          case 'x':
          case 'X':
            WriteFormat("%llx", static_cast<unsigned long long>(A.getNumber()));
            break;
          case 'u':
            WriteFormat("%llu", static_cast<unsigned long long>(A.getNumber()));
          }
          break;
        }
        break;
      case 'j':
        switch (*It++) {
        default:
          assert(0 && "Invalid format string.");
        case 'd':
        case 'i':
          WriteFormat("%jd", static_cast<std::intmax_t>(A.getNumber()));
          break;
        case 'o':
          WriteFormat("%jo", static_cast<std::uintmax_t>(A.getNumber()));
          break;
        case 'x':
        case 'X':
          WriteFormat("%jx", static_cast<std::uintmax_t>(A.getNumber()));
          break;
        case 'u':
          WriteFormat("%ju", static_cast<std::uintmax_t>(A.getNumber()));
        }
        break;
      case 'z':
        switch (*It++) {
        default:
          assert(0 && "Invalid format string.");
        case 'd':
        case 'i':
          WriteFormat(
              "%zd", static_cast<ssize_t>(A.getNumber())); // signed std::size_t
          break;
        case 'o':
          WriteFormat("%zo", static_cast<std::size_t>(A.getNumber()));
          break;
        case 'x':
        case 'X':
          WriteFormat("%zx", static_cast<std::size_t>(A.getNumber()));
          break;
        case 'u':
          WriteFormat("%zu", static_cast<std::size_t>(A.getNumber()));
        }
        break;
      case 't':
        switch (*It++) {
        default:
          assert(0 && "Invalid format string.");
        case 'd':
        case 'i':
          WriteFormat("%td", static_cast<std::ptrdiff_t>(A.getNumber()));
          break;
        case 'o':
          WriteFormat("%zo", static_cast<std::size_t>(
                                 A.getNumber())); // unsigned std::ptrdiff_t
          break;
        case 'x':
        case 'X':
          WriteFormat("%zx", static_cast<std::size_t>(
                                 A.getNumber())); // unsigned std::ptrdiff_t
          break;
        case 'u':
          WriteFormat("%zu", static_cast<std::size_t>(
                                 A.getNumber())); // unsigned std::ptrdiff_t
        }
        break;
      case 'L':
        switch (*It++) {
        default:
          assert(0 && "Invalid format string.");
        case 'f':
        case 'F':
          WriteFormat("%Lf", static_cast<long double>(A.getNumber()));
          break;
        case 'e':
        case 'E':
          WriteFormat("%Le", static_cast<long double>(A.getNumber()));
          break;
        case 'a':
        case 'A':
          WriteFormat("%La", static_cast<long double>(A.getNumber()));
          break;
        case 'g':
        case 'G':
          WriteFormat("%Lg", static_cast<long double>(A.getNumber()));
        }
      }
    }

    auto Next = std::find(It, End, '%');
    std::move(It, Next, BufferIt);
    BufferIt += Next - It;

    if (Next != End) {
      assert(0 && "Invalid format string.");
      exit(EXIT_FAILURE);
    }

    return {std::begin(Buffer), BufferIt};
  }
};

} // namespace cawk
