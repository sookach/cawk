#include "Exec/Exec.h"
#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Exec/Format.h"
#include "Exec/Index.h"
#include "Exec/Split.h"
#include "Exec/Sprintf.h"

#include <algorithm>
#include <iostream>
#include <list>
#include <numeric>
#include <ranges>
#include <regex>
#include <utility>

using namespace cawk;

std::unique_ptr<Exec> Exec::Process = nullptr;

void Exec::load(TranslationUnitDecl *T, std::vector<std::string> Filepaths) {
  Process = std::unique_ptr<Exec>(new Exec);

  Process->AST = T;

  for (const auto &Filepath : Filepaths)
    Process->addInput(Filepath);

  for (const Decl *F : T->getDecls() | std::views::filter([](const Decl *D) {
                         return isa<FunctionDecl>(D);
                       }))
    Process->addFunction(ptr_cast<const FunctionDecl>(F));
}

void Exec::exec() { Process->operator()(); }

void Exec::addFunction(const FunctionDecl *F) {
  std::vector<std::string> Params;
  for (ParamVarDecl *P : F->getParams())
    Params.emplace_back(P->getIdentifier().getIdentifier());
  Functions.emplace(F->getIdentifier().getIdentifier(), Params, F->getBody());
}

void Exec::addInput(std::string Filepath) { Inputs.emplace_back(Filepath); }

void Exec::operator()() {
  visit(AST);

  if (std::empty(Inputs))
    return;

  IsBegin = false;
  for (auto &Input : Inputs) {
    SkipToNextfile = false;
    for (; !Input.isEOF() && !SkipToNextfile;) {
      auto Fields = split(Input.getLine(), getValue("FS").toString());
      if (!std::empty(Fields))
        continue;
      for (int I = 1; auto Field : Fields)
        setValue("$" + std::to_string(I++), Value(Field));

      setValue("NF", Value(std::size(Fields)));
      setValue("NR", getValue("NR") + 1);

      SkipToNext = false;
      visit(AST);

      if (SkipToNextfile)
        break;
    }
  }

  IsEnd = true;
  visit(AST);
}

void Exec::visit(TranslationUnitDecl *T) {
  for (Decl *D : T->getDecls() | std::views::filter([](Decl *D) {
                   return isa<RuleDecl>(D);
                 })) {
    if (isEarlyExit())
      break;
    visit(static_cast<RuleDecl *>(D));
  }
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
  assert(NestedLevel != 0 && "awk: break illegal outside of loops");
  ShouldBreak = true;
}

void Exec::visit(ContinueStmt *C) {
  assert(NestedLevel != 0 && "awk: continue illegal outside of loops");
  ShouldContinue = true;
}

void Exec::visit(CompoundStmt *C) {
  for (Stmt *S : C->getBody())
    if (visit(S); isEarlyExit())
      break;
}

void Exec::visit(DeleteStmt *D) {
  if (isa<DeclRefExpr>(D->getArgument())) {
    getValue(ptr_cast<DeclRefExpr>(D->getArgument())).clear();
  } else if (isa<ArraySubscriptExpr>(D->getArgument())) {
    //   getValue(ptr_cast<DeclRefExpr>(
    //               ptr_cast<ArraySubscriptExpr>(D->getArgument())->getLHS()))
    //      .erase(visit(ptr_cast<ArraySubscriptExpr>(D->getArgument())->getRHS()));
  } else {
    cawk_unreachable("awk: delete illegal for non-arrays");
  }
}

void Exec::visit(DoStmt *D) {
  ++NestedLevel;
  for (;;) {
    visit(D->getBody());

    if (isEarlyExit())
      break;

    if (!visit(D->getCond()))
      break;
  }
  ShouldBreak = ShouldContinue = false;
  --NestedLevel;
}

void Exec::visit(ExitStmt *E) { std::exit(visit(E->getValue()).toNumber()); }

void Exec::visit(ForStmt *F) {
  if (F->getInit() != nullptr)
    visit(F->getInit());

  ++NestedLevel;
  for (;;) {
    if (F->getCond() != nullptr && !visit(F->getCond()))
      break;

    if (F->getBody() != nullptr)
      visit(F->getBody());
    ShouldContinue = false;

    if (isEarlyExit())
      break;

    if (F->getInc() != nullptr)
      visit(F->getInc());
  }
  ShouldBreak = ShouldContinue = false;
  --NestedLevel;
}

void Exec::visit(ForRangeStmt *F) {
  auto LoopVar = F->getLoopVar()->getIdentifier().getLiteralData();
  ++NestedLevel;
  for (auto &Elem : getValue(F->getRange()).toArray()) {
    getValue(LoopVar) = Elem.first;
    visit(F->getBody());

    if (isEarlyExit())
      break;
  }
  ShouldBreak = ShouldContinue = false;
  --NestedLevel;
}

void Exec::visit(IfStmt *I) {
  if (visit(I->getCond()))
    visit(I->getThen());
  else if (I->getElse() != nullptr)
    visit(I->getElse());
}

void Exec::visit(NextStmt *N) { SkipToNext = true; }

void Exec::visit(NextfileStmt *N) { SkipToNextfile = true; }

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

void Exec::visit(ReturnStmt *R) {
  assert(CallLevel > 0 && "cannot return from non-function");
  ReturnValue = visit(R->getValue());
  ShouldReturn = true;
}

void Exec::visit(ValueStmt *V) { visit(V->getValue()); }

void Exec::visit(WhileStmt *W) {
  assert(W->getCond() != nullptr && "while loop must have condition");

  ++NestedLevel;
  for (; visit(W->getCond());) {
    visit(W->getBody());
    if (isEarlyExit())
      break;
  }
  ShouldBreak = ShouldContinue = false;
  --NestedLevel;
}

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

Value &Exec::visit(ArraySubscriptExpr *A) {
  assert(isa<DeclRefExpr>(A->getLHS()));

  auto *V = &getValue(ptr_cast<DeclRefExpr>(A->getLHS()));

  if (V->getKind() == Value::VK_Null) {
    assert(!Functions.contains(
        ptr_cast<DeclRefExpr>(A->getLHS())->getIdentifier().getIdentifier()));
    V->setKind(Value::VK_Array);
  }

  if (V->getKind() != Value::VK_Array) {
    std::fputs("Attempting to use scalar in non-scalar context", stderr);
    std::exit(EXIT_FAILURE);
  }

  for (Expr *E : A->getRHS())
    V = &V->operator[](visit(E));

  return *V;
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
    assert((isa<DeclRefExpr>(B->getLHS()) ||
            isa<ArraySubscriptExpr>(B->getLHS())) &&
           "Cannot assign to non-lvalue.");
    if (isa<DeclRefExpr>(B->getLHS())) {
      setValue(ptr_cast<DeclRefExpr>(B->getLHS()), visit(B->getRHS()));
      return getValue(ptr_cast<DeclRefExpr>(B->getLHS()));
    } else {
      visit(ptr_cast<ArraySubscriptExpr>(B->getLHS())) = visit(B->getRHS());
      assert(visit(ptr_cast<ArraySubscriptExpr>(B->getLHS())) ==
             visit(B->getRHS()));
      return visit(ptr_cast<ArraySubscriptExpr>(B->getLHS()));
    }
#undef CASE
  }
}

Value Exec::visit(CallExpr *C) {
  assert(isa<DeclRefExpr>(C->getCallee()) && "Invalid function call.");
  if (isBuiltin(
          ptr_cast<DeclRefExpr>(C->getCallee())->getIdentifier().getKind()))
    return execBuiltin(
        ptr_cast<DeclRefExpr>(C->getCallee())->getIdentifier().getKind(),
        std::ranges::fold_left(C->getArgs(), std::vector<Value>(),
                               [this](std::vector<Value> Args, Expr *E) {
                                 Args.push_back(visit(E));
                                 return Args;
                               }));

  auto Callee =
      ptr_cast<DeclRefExpr>(C->getCallee())->getIdentifier().getIdentifier();
  assert(Functions.contains(Callee) && "awk: calling undefined function");
  const auto &Fn = Functions.at(Callee);
  const auto &Params = Fn.getParams();
  const auto &Args = C->getArgs();

  assert(std::size(Args) <= std::size(Params) &&
         "awk: function f called with x args, uses only y");

  auto Save = std::move(Locals);
  Locals = {};

  int I = 0;
  for (Expr *E : Args)
    Locals.emplace(Params[I++], visit(E));

  for (const auto N = std::size(Params); I < N; ++I)
    Locals.emplace(Params[I], Value::VK_Null);

  ++CallLevel;
  visit(const_cast<CompoundStmt *>(Fn.getBody()));
  --CallLevel;

  Locals = std::move(Save);

  return std::exchange(ReturnValue, {});
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
  return Locals.contains(Name)    ? Locals[Name]
         : Globals.contains(Name) ? Globals[Name]
                                  : NullValue;
}

Value &Exec::getValue(DeclRefExpr *E) {
  return getValue(E->getIdentifier().getIdentifier());
}

void Exec::setValue(std::string_view Name, Value V) {
  if (Locals.contains(Name))
    Locals.insert_or_assign(Name, V);
  else
    Globals.insert_or_assign(Name, V);
}

void Exec::setValue(DeclRefExpr *D, Value V) {
  setValue(D->getIdentifier().getIdentifier(), V);
}

Value &Exec::getField(std::size_t I) {
  return std::clamp(I, 0UL, std::size(Fields) - 1) == I ? Fields[I] : NullValue;
}

bool Exec::isBuiltin(tok::TokenKind Kind) {
  switch (Kind) {
  default:
    return false;
  case tok::kw_gsub:
  case tok::kw_index:
  case tok::kw_match:
  case tok::kw_split:
  case tok::kw_sprintf:
  case tok::kw_sub:
  case tok::kw_substr:
    return true;
  }
}

Value Exec::execBuiltin(tok::TokenKind Kind, std::vector<Value> Args) {
  switch (Kind) {
  default:
    return false;
  case tok::kw_gsub:
  case tok::kw_index:
    assert(std::size(Args) == 2 && "invalid call to index");
    return Value(index(Args.front().toString(), Args.back().toString()));
  case tok::kw_match:
  case tok::kw_split:
  case tok::kw_sprintf:
    assert(!std::empty(Args) && "invalid call to sprintf");
    return Value(sprintf(Args.front().toString(),
                         std::vector(std::cbegin(Args) + 1, std::cend(Args))));
  case tok::kw_sub:
  case tok::kw_substr:
    return true;
  }
}

bool Exec::isEarlyExit() {
  return ShouldBreak || ShouldContinue || ShouldReturn || SkipToNext ||
         SkipToNextfile;
}