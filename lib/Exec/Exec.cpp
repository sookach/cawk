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
      auto Fields = split(Input.getLine(),
                          BuiltinVariables["FS"]->getValueAs<StringTy>());
      if (!std::empty(Fields))
        continue;
      for (int I = 1; auto Field : Fields)
        BuiltinVariables["$" + std::to_string(I++)]->setValue(Field);

      BuiltinVariables["NF"]->setValue(Value(std::size(Fields)));
      BuiltinVariables["NF"]->setValue(
          BuiltinVariables["NF"]->getValueAs<NumberTy>() + 1);

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
          P->getArgs() | std::views::drop(1), std::vector<Value *>(),
          [this](std::vector<Value *> Args, Expr *E) {
            traverse(E);
            Args.push_back(E->getValue());
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
  traverse(R->getValue());
  CallStack.back()->setValue(*R->getValue()->getValue());
  ShouldReturn = true;
  return true;
}

bool Exec::visit(ValueStmt *V) {
  traverse(V->getValue());
  return true;
}

bool Exec::visit(WhileStmt *W) {
  for (;;) {
    traverse(W->getCond());
    traverse(W->getBody());
    if (isEarlyExit())
      break;
  }
  ShouldBreak = ShouldContinue = false;
  return true;
}

bool Exec::visit(ArraySubscriptExpr *A) {
  traverse(A->getLHS());
  Value *S = A->getLHS()->getValue()->operator[](Value(std::ranges::fold_left(
      A->getRHS(), std::string(), [this](std::string S, Expr * E) {
        traverse(E);
        return S + E->getValue()->getAs<StringTy>();
      })));

  A->setValue(S);
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
    B->setValue(B->getLHS()                                                    \
                    ->getValueAs<NumberTy>() OP B->getRHS()                    \
                    ->getValueAs<NumberTy>());                                 \
    break
    CASE(tok::plus, +);
    CASE(tok::minus, -);
    CASE(tok::star, *);
    CASE(tok::slash, /);
    CASE(tok::equalequal, ==);
    CASE(tok::exclaimequal, !=);
#undef CASE
  case tok::equal:
    traverse(B->getLHS());
    traverse(B->getRHS());
    B->getLHS()->setValue(*B->getRHS()->getValue());
    break;
#define CASE(TOK, OP)                                                          \
  traverse(B->getLHS());                                                       \
  traverse(B->getRHS());                                                       \
  B->getLHS()->setValue(B->getLHS()                                            \
                            ->getValueAs<NumberTy>() OP B->getRHS()            \
                            ->getValueAs<NumberTy>());                         \
  B->setValue(B->getValueAs<NumberTy>());                                      \
  break
    CASE(tok::plusequal, +);
    CASE(tok::minusequal, -);
    CASE(tok::starequal, *);
    CASE(tok::slashequal, /);
#undef CASE
  case tok::caretequal:
  case tok::starstarequal:
    traverse(B->getLHS());
    traverse(B->getRHS());
    B->getLHS()->setValue(std::pow(B->getLHS()->getValueAs<NumberTy>(),
                                   B->getRHS()->getValueAs<NumberTy>()));
    B->setValue(B->getLHS()->getValueAs<NumberTy>());
    break;
  case tok::space:
    traverse(B->getLHS());
    traverse(B->getRHS());
    B->setValue(B->getLHS()->getValueAs<StringTy>() +
                B->getRHS()->getValueAs<StringTy>());
    break;
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
    CallStack.push_back(C);
    if (!execBuiltin(IdenKind, Args)) {
      CallStack.pop_back();
      return false;
    }
    CallStack.pop_back();
    return true;
  }

  FunctionDecl *Function = C->getFunction();
  auto Params = Function->getParams();
  auto Args = C->getArgs();

  for (int I = 0; I != std::size(Args); ++I) {
    if (isa<DeclRefExpr>(Args[I]) && Args[I]->getValue()->is<ArrayTy>())
      Params[I]->setExpr(static_cast<DeclRefExpr *>(Args[I]));
    else
      Params[I]->getExpr()->setValue(*Args[I]->getValue());
  }

  CallStack.push_back(C);
  visit(const_cast<CompoundStmt *>(Function->getBody()));
  CallStack.pop_back();
  return true;
}

bool Exec::visit(DeclRefExpr *D) { return true; }

bool Exec::visit(FloatingLiteral *F) {
  F->setValue(Value(std::stod(std::string(F->getLiteral().getLiteralData()))));
  return true;
}

bool Exec::visit(RegexLiteral *R) {
  std::string String;
  auto Data = R->getLiteral().getLiteralData();
  auto It = std::cbegin(Data) + 1, End = std::cend(Data) - 1;
  for (; It != End;) {
    switch (*It) {
    default:
      String.push_back(*(It++));
      break;
    case '\\':
      switch (*(++It)) {
      default:
        String.push_back(*(It++));
        break;
      case '\\':
        String.push_back('\\');
        ++It;
        break;
      case '\a':
        String.push_back('\a');
        ++It;
        break;
      case '\b':
        String.push_back('\b');
        ++It;
        break;
      case '\f':
        String.push_back('\f');
        ++It;
        break;
      case '\n':
        String.push_back('\n');
        ++It;
        break;
      case '\r':
        String.push_back('\r');
        ++It;
        break;
      case '\t':
        String.push_back('\t');
        ++It;
        break;
      case '\v':
        String.push_back('\v');
        ++It;
        break;
      }
    }
  }
  
  R->setValue(Value(String));
  return true;
}

bool Exec::visit(StringLiteral *S) {
  std::string String;
  auto Data = S->getLiteral().getLiteralData();
  auto It = std::cbegin(Data) + 1, End = std::cend(Data) - 1;
  for (; It != End;) {
    switch (*It) {
    default:
      String.push_back(*(It++));
      break;
    case '\\':
      switch (*(++It)) {
      default:
        String.push_back(*(It++));
        break;
      case '\\':
        String.push_back('\\');
        ++It;
        break;
      case 'a':
        String.push_back('\a');
        ++It;
        break;
      case 'b':
        String.push_back('\b');
        ++It;
        break;
      case 'f':
        String.push_back('\f');
        ++It;
        break;
      case 'n':
        String.push_back('\n');
        ++It;
        break;
      case 'r':
        String.push_back('\r');
        ++It;
        break;
      case 't':
        String.push_back('\t');
        ++It;
        break;
      case 'v':
        String.push_back('\v');
        ++It;
        break;
      }
    }
  }
  S->setValue(Value(String));
  return true;
}

bool Exec::visit(UnaryOperator *U) {
  switch (U->getOpcode().getKind()) {
  default:
  case tok::plus:
    traverse(U->getSubExpr());
    U->setValue(U->getSubExpr()->getValueAs<NumberTy>());
    break;
  case tok::minus:
    traverse(U->getSubExpr());
    U->setValue(-U->getSubExpr()->getValueAs<NumberTy>());
    break;
  case tok::plusplus:
    traverse(U->getSubExpr());
    if (U->getFix() == UnaryOperator::Prefix)
      U->setValue(U->getSubExpr()->getValueAs<NumberTy>() + 1);
    else
      U->setValue(U->getSubExpr()->getValueAs<NumberTy>());
    U->getSubExpr()->setValue(U->getSubExpr()->getValueAs<NumberTy>() + 1);
    break;
  case tok::minusminus:
    traverse(U->getSubExpr());
    if (U->getFix() == UnaryOperator::Prefix)
      U->setValue(U->getSubExpr()->getValueAs<NumberTy>() - 1);
    else
      U->setValue(U->getSubExpr()->getValueAs<NumberTy>());
    U->getSubExpr()->setValue(U->getSubExpr()->getValueAs<NumberTy>() - 1);
    break;
  case tok::exclaim:
    traverse(U->getSubExpr());
    switch (U->getSubExpr()->getValue()->getType()) {
    case ArrayTy:
      return false;
    case NumberTy:
      U->setValue(!U->getSubExpr()->getValueAs<NumberTy>());
      break;
    case StringTy:
      U->setValue(!std::empty(U->getSubExpr()->getValueAs<StringTy>()));
      break;
    case NullTy:
      U->setValue(true);
    }
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

bool Exec::execBuiltin(tok::TokenKind Kind, std::vector<Value *> Args) {
  switch (Kind) {
  default:
    return false;
  case tok::kw_gsub:
  case tok::kw_index:
    assert(std::size(Args) == 2 && "invalid call to index");
    CallStack.back()->setValue(
        index(Args.front()->getAs<StringTy>(), Args.back()->getAs<StringTy>()));
  case tok::kw_match:
  case tok::kw_split:
  case tok::kw_sprintf:
    assert(!std::empty(Args) && "invalid call to sprintf");
    CallStack.back()->setValue(
        sprintf(Args.front()->getAs<StringTy>(),
                std::vector(std::cbegin(Args) + 1, std::cend(Args))));
  case tok::kw_sub:
  case tok::kw_substr:
    return true;
  }
  return false;
}

bool Exec::isEarlyExit() {
  return ShouldBreak || ShouldContinue || ShouldReturn || SkipToNext ||
         SkipToNextfile;
}