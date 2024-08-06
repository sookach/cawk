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
}

void Exec::exec() { Process->operator()(); }

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

bool Exec::visit(TranslationUnitDecl *T) {
  for (Decl *D : T->getDecls())
    if (RuleDecl *R = dyn_cast<RuleDecl>(D))
      visit(R);
  return true;
}

bool Exec::visit(RuleDecl *R) {
  if (R->getPattern() == nullptr || R->getPattern()->getValue())
    visit(R->getAction());
}

bool Exec::visit(BreakStmt *B) {
  ShouldBreak = true;
  return true;
}

bool Exec::visit(ContinueStmt *C) {
  ShouldContinue = true;
  return true;
}

bool Exec::visit(CompoundStmt *C) {
  for (Stmt *S : C->getBody())
    if (visit(S); isEarlyExit())
      break;
}

bool Exec::visit(DeleteStmt *D) { D->getArgument()->getValue().clear(); }

bool Exec::visit(DoStmt *D) {

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

bool Exec::visit(ExitStmt *E) {
  if (E->getValue() != nullptr) {
    visit(E->getValue());
    std::exit(dyn_cast<Scalar>(E->getValue())->getAs<Value::TK_Number>());
  }
  std::exit(EXIT_SUCCESS);
}

bool Exec::visit(ForStmt *F) {
  if (F->getInit() != nullptr)
    visit(F->getInit());

  for (;;) {
    if (F->getCound() != nullptr) {
      visit(F->getCond());
      if (F->getCond()->getValue()->is(Value::TK_Number) &&
              !F->getCond()->getValue()->get<TK_Number>() ||
          F->getCond()->getValue()->is(Value::TK_String) &&
              !F->getCond()->getValue()->get<TK_String>())
        break;
    }

    if (F->getBody() != nullptr)
      visit(F->getBody());

    if (F->getInc() != nullptr)
      visit(F->getInc());
  }

  return true;
}

bool Exec::visit(ForRangeStmt *F) {
  for (auto &[Key, Val] : F->getRange()->getValue()->getAs<Value::TK_Array>()) {
    F->getLoopVar()->setValue(Scalar(Key));
    visit(F->getBody());
  }
  return true;
}

bool Exec::visit(IfStmt *I) {
  visit(I->getCond());
  if (dyn_cast<Scalar>(I->getCond()->getValue())->isTrue())
    visit(I->getThen());
  else if (I->getElse() != nullptr)
    visit(I->getElse());
  return true;
}

bool Exec::visit(NextStmt *N) { SkipToNext = true; }

bool Exec::visit(NextfileStmt *N) { SkipToNextfile = true; }

bool Exec::visit(PrintStmt *P) {
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

bool Exec::visit(ReturnStmt *R) {
  assert(CallLevel > 0 && "cannot return from non-function");
  ReturnValue = visit(R->getValue());
  ShouldReturn = true;
}

bool Exec::visit(ValueStmt *V) { visit(V->getValue()); }

bool Exec::visit(WhileStmt *W) {
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

bool Exec::visit(ArraySubscriptExpr *A) {
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

bool Exec::visit(BinaryOperator *B) {
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

bool Exec::visit(CallExpr *C) {
  auto IdenKind =
      ptr_cast<DeclRefExpr>(C->getCallee())->getIdentifier().getKind();
  if (isBuiltin(IdenKind)) {
    auto Args =
        std::ranges::fold_left(C->getArgs(), std::vector<Value *>(),
                               [this](std::vector<Value *> Args, Expr *E) {
                                 visit(E);
                                 Args.push_back(E->getValue());
                                 return Args;
                               });
    execBuiltin(IdenKind, Args);
    return true;
  }

  FunctionDecl *Function = C->getFunction();
  auto Params = Function->getParams();
  auto Args = C->getArgs();

  for (int I = 0; I != std::size(Args); ++I) {
    if (Args[I]->getValue()->is(Value::TK_Array))
      Params[I]->setExpr(new (Params[I]->getExpr()) Array(Args[I]));
    else
      Params[I]->setExpr(new (Params[I]->getExpr()) Scalar(Args[I]));
  }

  visit(const_cast<CompoundStmt *>(Fn->getBody()));

  return std::exchange(ReturnValue, {});
}

bool Exec::visit(DeclRefExpr *D) { return true; }

bool Exec::visit(FloatingLiteral *F) {
  F->setValue(Scalar(std::string(F->getLiteral().getLiteralData())));
  F->setValue(F->getValue()->getAs<Value::TK_Number>());
  return true;
}

bool Exec::visit(RegexLiteral *R) { return true; }

bool Exec::visit(StringLiteral *S) {
  std::string String(S->getLiteral().getLiteralData());
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
  S->setValue((String));
}

bool Exec::visit(UnaryOperator *U) {
  switch (U->getOpcode().getKind()) {
  case tok::plus:
  case tok::minus:
  case tok::plusplus:
  case tok::minusminus:
  }
}

bool Exec::getField(std::size_t I) {
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