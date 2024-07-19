#include "Exec/Exec.h"
#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Support/Format.h"

#include <algorithm>
#include <iostream>
#include <list>
#include <numeric>
#include <ranges>
#include <regex>
#include <utility>

using namespace cawk;

std::unique_ptr<Exec> Exec::Process = nullptr;

void Exec::load(TranslationUnitDecl *T) {
  Process = std::unique_ptr<Exec>(new Exec);

  Process->AST = T;

  for (const Decl *F : T->getDecls() | std::views::filter([](const Decl *D) {
                         return isa<FunctionDecl>(D);
                       }))
    Process->addFunction(ptr_cast<const FunctionDecl>(F));
}

void Exec::exec() { Process->operator()(); }

void Exec::addFunction(const FunctionDecl *F) {
  Functions.set(F->getIdentifier().getIdentifier(),
                const_cast<FunctionDecl *>(F));
}

void Exec::operator()() {
  IsBegin = true;
  IsEnd = false;
  visit(AST);
  std::swap(IsBegin, IsEnd);
  visit(AST);
}

void Exec::visit(TranslationUnitDecl *T) {
  for (Decl *D : T->getDecls() | std::views::filter(
                                     [](Decl *D) { return isa<RuleDecl>(D); }))
    visit(static_cast<RuleDecl *>(D));
}

void Exec::visit(RuleDecl *R) {
  if (visit(R->getPattern()))
    visit(R->getAction());
}

void Exec::visit(Stmt *S) {
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

void Exec::visit(BreakStmt *B) {
  assert(NestedLevel-- != 0 && "awk: break illegal outside of loops");
}

void Exec::visit(ContinueStmt *C) {
  assert(NestedLevel != 0 && "awk: continue illegal outside of loops");
}

void Exec::visit(CompoundStmt *C) {
  for (Stmt *S : C->getBody())
    visit(S);
}

void Exec::visit(DeleteStmt *D) {
  if (isa<DeclRefExpr>(D->getArgument())) {
    getValue(ptr_cast<DeclRefExpr>(D->getArgument())).clear();
  } else if (isa<ArraySubscriptExpr>(D->getArgument())) {
    getValue(ptr_cast<DeclRefExpr>(
                 ptr_cast<ArraySubscriptExpr>(D->getArgument())->getLHS()))
        .erase(visit(ptr_cast<ArraySubscriptExpr>(D->getArgument())->getRHS()));
  } else {
    cawk_unreachable("awk: delete illegal for non-arrays");
  }
}

void Exec::visit(DoStmt *D) { cawk_unreachable("unimplemented"); }

void Exec::visit(ExitStmt *E) { std::exit(visit(E->getValue())); }

void Exec::visit(ForStmt *F) {
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

void Exec::visit(ForRangeStmt *F) {
  auto LoopVar = F->getLoopVar()->getIdentifier().getLiteralData();
  for (auto &[Key, Value] : getValue(F->getRange()).toArray()) {
    getValue(LoopVar) = Key;
    visit(F->getBody());
  }
}

void Exec::visit(IfStmt *I) {
  if (visit(I->getCond()))
    visit(I->getThen());
  else if (I->getElse() != nullptr)
    visit(I->getElse());
}

void Exec::visit(NextStmt *N) { cawk_unreachable("unimplemented"); }

void Exec::visit(NextfileStmt *N) { cawk_unreachable("unimplemented"); }

void Exec::visit(PrintStmt *P) {
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

void Exec::visit(ReturnStmt *R) { cawk_unreachable("unimplemented"); }

void Exec::visit(ValueStmt *V) { visit(V->getValue()); }

void Exec::visit(WhileStmt *W) { cawk_unreachable("unimplemented"); }

Value Exec::visit(Expr *E) {
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

Value Exec::visit(ArraySubscriptExpr *A) {
  return visit(A->getLHS())[visit(A->getRHS())];
}

Value Exec::visit(BinaryOperator *B) {
  switch (B->getOpcode().getKind()) {
  default:
    cawk_unreachable("Invalid binary operation");
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

Value Exec::visit(CallExpr *C) {
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

Value Exec::visit(DeclRefExpr *D) {
  switch (D->getIdentifier().getKind()) {
  default:
    return getValue(D);
  case tok::kw_BEGIN:
    return IsBegin;
  case tok::kw_END:
    return IsEnd;
  }
}

Value Exec::visit(FloatingLiteral *F) {
  return std::stod(F->getValue().getLiteralData().data());
}

Value Exec::visit(RegexLiteral *R) { return Value(0); }

Value Exec::visit(StringLiteral *S) {
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

Value Exec::visit(UnaryOperator *U) {
  switch (U->getOpcode().getKind()) {
  default:
    cawk_unreachable("Invalid Unary Operation.");
    exit(EXIT_FAILURE);
  case tok::plusplus: {
    assert(isa<DeclRefExpr>(U->getSubExpr()) &&
           "++ can only be performed on variables.");
    auto Name =
        ptr_cast<DeclRefExpr>(U->getSubExpr())->getIdentifier().getIdentifier();
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

Value &Exec::getValue(std::string_view Name) {
  return Locals.contains(Name)    ? Locals.get(Name)
         : Globals.contains(Name) ? Globals.get(Name)
                                  : NullValue;
}

Value &Exec::getValue(DeclRefExpr *E) {
  return getValue(E->getIdentifier().getIdentifier());
}

void Exec::setValue(std::string_view Name, Value V) {
  if (Locals.contains(Name))
    Locals.set(Name, V);
  else
    Globals.set(Name, V);
}

void Exec::setValue(DeclRefExpr *D, Value V) {
  setValue(D->getIdentifier().getIdentifier(), V);
}

Value &Exec::getField(std::size_t I) {
  return std::clamp(I, 0UL, std::size(Fields) - 1) == I ? Fields[I] : NullValue;
}
