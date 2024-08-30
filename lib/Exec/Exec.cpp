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
  BeginKeyword::Create({})->setValue(Value(1));
  EndKeyword::Create({})->setValue(Value(0));

  visit(AST);

  if (std::empty(Inputs))
    return;

  BeginKeyword::Create({})->setValue(Value(0));

  for (auto &Input : Inputs) {
    SkipToNextfile = false;
    for (; !Input.isEOF() && !SkipToNextfile;) {
      auto Fields =
          split(Input.getLine(), BuiltinVariables["FS"]->get<StringTy>());
      if (!std::empty(Fields))
        continue;
      for (int I = 1; auto Field : Fields)
        BuiltinVariables["$" + std::to_string(I++)]->setValue(Field);

      BuiltinVariables["NF"]->setValue(Value(std::size(Fields)));
      BuiltinVariables["NF"]->setValue(BuiltinVariables["NF"]->get<NumberTy>() +
                                       1);

      SkipToNext = false;
      visit(AST);

      if (SkipToNextfile)
        break;
    }
  }

  EndKeyword::Create({})->setValue(Value(1));

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
  return true;
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
    if (traverse(S); isEarlyExit())
      break;
  return true;
}

bool Exec::visit(DeleteStmt *D) { return true; }

bool Exec::visit(DoStmt *D) {
  for (;;) {
    traverse(D->getBody());

    if (isEarlyExit())
      break;

    if (traverse(D->getCond());
        D->getCond()->getValue()->is<NumberTy>() &&
            !D->getCond()->getValue()->get<NumberTy>() ||
        D->getCond()->getValue()->is<StringTy>() &&
            !std::empty(D->getCond()->getValue()->get<StringTy>()))
      break;
  }
  ShouldBreak = ShouldContinue = false;
  --NestedLevel;
  return true;
}

bool Exec::visit(ExitStmt *E) {
  if (E->getValue() != nullptr) {
    traverse(E->getValue());
    std::exit(E->getValue()->getValue()->getAs<NumberTy>());
  }
  std::exit(EXIT_SUCCESS);
}

bool Exec::visit(ForStmt *F) {
  if (F->getInit() != nullptr)
    traverse(F->getInit());

  for (;;) {
    if (F->getCond() != nullptr) {
      traverse(F->getCond());
      if (F->getCond()->getValue()->is<NumberTy>() &&
              !F->getCond()->getValue()->get<NumberTy>() ||
          F->getCond()->getValue()->is<StringTy>() &&
              !std::empty(F->getCond()->getValue()->get<StringTy>()))
        break;
    }

    if (F->getBody() != nullptr)
      traverse(F->getBody());

    if (F->getInc() != nullptr)
      traverse(F->getInc());
  }

  return true;
}

bool Exec::visit(ForRangeStmt *F) {
  for (auto &[Key, Val] : F->getRange()->getValue()->get<ArrayTy>()) {
    F->getLoopVar()->setValue(Value::Scalar(Key));
    traverse(F->getBody());
  }
  return true;
}

bool Exec::visit(IfStmt *I) {
  traverse(I->getCond());
  if (I->getCond()->getValue()->is<NumberTy>() &&
          !I->getCond()->getValue()->get<NumberTy>() ||
      I->getCond()->getValue()->is<StringTy>() &&
          !std::empty(I->getCond()->getValue()->get<StringTy>()))
    traverse(I->getThen());
  else if (I->getElse() != nullptr)
    traverse(I->getElse());
  return true;
}

bool Exec::visit(NextStmt *N) {
  SkipToNext = true;
  return true;
}

bool Exec::visit(NextfileStmt *N) {
  SkipToNextfile = true;
  return true;
}

bool Exec::visit(PrintStmt *P) {
  assert(P->getOpcode().is(tok::unknown) && P->getOutput() == nullptr &&
         "unimplemented");
  if (P->getIden().is(tok::kw_print)) {
    std::puts(std::ranges::fold_left(P->getArgs(), std::string(),
                                     [this](std::string S, Expr *E) {
                                       traverse(E);
                                       return S +
                                              E->getValue()->getAs<StringTy>() +
                                              ' ';
                                     })
                  .c_str());
  } else {
    if (!std::empty(P->getArgs())) {
      auto Args = std::ranges::fold_left(
          P->getArgs() | std::views::drop(1), std::vector<Value>(),
          [this](std::vector<Value> Args, Expr *E) {
            traverse(E);
            Args.push_back(*E->getValue());
            return Args;
          });
      traverse(P->getArgs().front());
      auto S = format(P->getArgs().front()->getValue()->get<StringTy>(), Args);
      std::printf("%s", S.c_str());
    }
  }
  return true;
}

bool Exec::visit(ReturnStmt *R) {
  assert(CallLevel > 0 && "cannot return from non-function");
  traverse(R->getValue());
  ParentFunction->setValue(*R->getValue()->getValue());
  ShouldReturn = true;
  return true;
}

bool Exec::visit(ValueStmt *V) {
  traverse(V->getValue());
  return true;
}

bool Exec::visit(WhileStmt *W) {
  assert(W->getCond() != nullptr && "while loop must have condition");

  ++NestedLevel;
  for (;;) {
    traverse(W->getCond());
    traverse(W->getBody());
    if (isEarlyExit())
      break;
  }
  ShouldBreak = ShouldContinue = false;
  --NestedLevel;
  return true;
}

bool Exec::visit(ArraySubscriptExpr *A) {
  traverse(A->getLHS());
  Value::Scalar *S = A->getValue()->operator[](std::ranges::fold_left(
      A->getRHS(), std::string(), [this](std::string S, Expr * E) {
        traverse(E);
        return S + E->getValue()->getAs<StringTy>();
      }));

  A->setValue(*S);
  return true;
}

bool Exec::visit(BinaryOperator *B) {
  switch (B->getOpcode().getKind()) {
  default:
    cawk_unreachable("Invalid binary operation");
    exit(EXIT_FAILURE);
#define CASE(TOK, OP)                                                          \
  case TOK:                                                                    \
    traverse(B->getLHS());                                                     \
    traverse(B->getRHS());                                                     \
    return B->getLHS()                                                         \
        ->getValue()                                                           \
        ->getAs<NumberTy>() OP B->getRHS()                                     \
        ->getValue()                                                           \
        ->getAs<NumberTy>();
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
    traverse(B->getLHS());
    traverse(B->getRHS());
    B->getLHS()->setValue(*B->getRHS()->getValue());

#undef CASE
  }
  return true;
}

bool Exec::visit(CallExpr *C) {
  auto IdenKind =
      ptr_cast<DeclRefExpr>(C->getCallee())->getIdentifier().getKind();
  if (isBuiltin(IdenKind)) {
    auto Args =
        std::ranges::fold_left(C->getArgs(), std::vector<Value *>(),
                               [this](std::vector<Value *> Args, Expr *E) {
                                 traverse(E);
                                 Args.push_back(E->getValue());
                                 return Args;
                               });
    // execBuiltin(IdenKind, Args);
    return true;
  }

  FunctionDecl *Function = C->getFunction();
  auto Params = Function->getParams();
  auto Args = C->getArgs();

  for (int I = 0; I != std::size(Args); ++I) {
    // if (Args[I]->getValue()->is<ArrayTy>())
    //   Params[I]->setExpr(new (Params[I]->getExpr()) Array(Args[I]));
    // else
    //   Params[I]->setExpr(new (Params[I]->getExpr()) Scalar(Args[I]));
  }

  visit(const_cast<CompoundStmt *>(Function->getBody()));
  return true;
}

bool Exec::visit(DeclRefExpr *D) { return true; }

bool Exec::visit(FloatingLiteral *F) {
  F->setValue(Value(std::stod(std::string(F->getLiteral().getLiteralData()))));
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
  S->setValue(Value(String));
  return true;
}

bool Exec::visit(UnaryOperator *U) {
  switch (U->getOpcode().getKind()) {
  default:
  case tok::plus:
  case tok::minus:
  case tok::plusplus:
  case tok::minusminus:
  }
  return true;
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
  //   switch (Kind) {
  //   default:
  //     return false;
  //   case tok::kw_gsub:
  //   case tok::kw_index:
  //     assert(std::size(Args) == 2 && "invalid call to index");
  //     return Value(index(Args.front().toString(), Args.back().toString()));
  //   case tok::kw_match:
  //   case tok::kw_split:
  //   case tok::kw_sprintf:
  //     assert(!std::empty(Args) && "invalid call to sprintf");
  //     return Value(sprintf(Args.front().toString(),
  //                          std::vector(std::cbegin(Args) + 1,
  //                          std::cend(Args))));
  //   case tok::kw_sub:
  //   case tok::kw_substr:
  //     return true;
  //   }
  return Value();
}

bool Exec::isEarlyExit() {
  return ShouldBreak || ShouldContinue || ShouldReturn || SkipToNext ||
         SkipToNextfile;
}