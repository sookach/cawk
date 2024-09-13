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
  EndKeyword::Create({})->setValue(Value(0.0));

  visit(AST);

  if (std::empty(Inputs))
    return;

  BeginKeyword::Create({})->setValue(Value(0.0));

  for (auto &Input : Inputs) {
    SkipToNextfile = false;
    for (; !Input.isEOF() && !SkipToNextfile;) {
      FieldTable = {new Value(Input.getLine())};
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

    if (traverse(D->getCond()); !D->getCond()->isTrue())
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
      if (!F->getCond()->isTrue())
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
  Environments.emplace_back();
  for (auto &[Key, Val] : F->getRange()->getValue()->get<ArrayTy>()) {
    Environments.back()[std::string(F->getLoopVar()->getName())] =
        new Value(Key);
    traverse(F->getBody());
  }
  Environments.pop_back();
  return true;
}

bool Exec::visit(IfStmt *I) {
  traverse(I->getCond());
  if (I->getCond()->isTrue())
    return traverse(I->getThen());
  else if (I->getElse() != nullptr)
    return traverse(I->getElse());
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
    if (!W->getCond()->isTrue())
      break;
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
    B->setValue(Value(B->getLHS()                                              \
                          ->getValueAs<NumberTy>() OP B->getRHS()              \
                          ->getValueAs<NumberTy>()));                          \
    break
    CASE(tok::plus, +);
    CASE(tok::minus, -);
    CASE(tok::star, *);
    CASE(tok::slash, /);
    CASE(tok::equalequal, ==);
    CASE(tok::exclaimequal, !=);
#undef CASE
  case tok::space:
    traverse(B->getLHS());
    traverse(B->getRHS());
    B->setValue(Value(B->getLHS()->getValueAs<StringTy>() +
                      B->getRHS()->getValueAs<StringTy>()));
    break;
  case tok::equal:
    traverse(B->getLHS());
    traverse(B->getRHS());
    B->getLHS()->setValue(*B->getRHS()->getValue());
    B->setValue(B->getLHS()->getValue());
    B->getLHS()->executeOnAssignment();
    break;
#define CASE(TOK, OP)                                                          \
  case TOK:                                                                    \
    traverse(B->getLHS());                                                     \
    traverse(B->getRHS());                                                     \
    B->getLHS()->setValue(Value(B->getLHS()                                    \
                                    ->getValueAs<NumberTy>() OP B->getRHS()    \
                                    ->getValueAs<NumberTy>()));                \
    B->setValue(B->getValue());                                                \
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
    B->getLHS()->setValue(Value(std::pow(B->getLHS()->getValueAs<NumberTy>(),
                                         B->getRHS()->getValueAs<NumberTy>())));
    B->setValue(B->getLHS()->getValue());
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

  if (!traverse(C->getCallee()))
    return false;
  FunctionDecl *Function = C->getCallee()->getValueAs<FunctionTy>();
  auto Params = Function->getParams();
  auto Args = C->getArgs();

  Environments.emplace_back();

  for (Expr *E : Args) {
    if (!traverse(E))
      return false;
  }

  for (int I = 0; I != std::min(std::size(Params), std::size(Args)); ++I) {
    if (!traverse(Args[I]))
      return false;

    auto Type = Args[I]->getType();
    if (Type == ArrayTy || Type == FunctionTy)
      Environments.back()[std::string(Params[I]->getName())] =
          Args[I]->getValue();
    else
      Environments.back()[std::string(Params[I]->getName())] =
          new Value(*Args[I]->getValue());
  }

  CallStack.push_back(C);
  visit(const_cast<CompoundStmt *>(Function->getBody()));
  CallStack.pop_back();
  return true;
}

bool Exec::visit(DeclRefExpr *D) {
  for (auto It = std::crbegin(Environments), End = std::crend(Environments);
       It != End; ++It) {
    if (It->contains(std::string(D->getName()))) {
      D->setValue(*It->at(std::string(D->getName())));
      return true;
    }
  }
  Environments.front()[std::string(D->getName())] = new Value;
  D->setValue(Environments.back()[std::string(D->getName())]);
  return true;
}

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
    cawk_unreachable("Invalid unary operation");
  case tok::plus:
    traverse(U->getSubExpr());
    U->setValue(Value(U->getSubExpr()->getValueAs<NumberTy>()));
    break;
  case tok::minus:
    traverse(U->getSubExpr());
    U->setValue(Value(-U->getSubExpr()->getValueAs<NumberTy>()));
    break;
  case tok::plusplus:
    traverse(U->getSubExpr());
    if (U->getFix() == UnaryOperator::Prefix)
      U->setValue(Value(U->getSubExpr()->getValueAs<NumberTy>() + 1));
    else
      U->setValue(Value(U->getSubExpr()->getValueAs<NumberTy>()));
    U->getSubExpr()->setValue(
        Value(U->getSubExpr()->getValueAs<NumberTy>() + 1));
    break;
  case tok::minusminus:
    traverse(U->getSubExpr());
    if (U->getFix() == UnaryOperator::Prefix)
      U->setValue(Value(U->getSubExpr()->getValueAs<NumberTy>() - 1));
    else
      U->setValue(Value(U->getSubExpr()->getValueAs<NumberTy>()));
    U->getSubExpr()->setValue(
        Value(U->getSubExpr()->getValueAs<NumberTy>() - 1));
    break;
  case tok::exclaim:
    traverse(U->getSubExpr());
    switch (U->getSubExpr()->getType()) {
    case ArrayTy:
    case FunctionTy:
      cawk_fatal("Invalid conversion from ",
                 toString(U->getSubExpr()->getType()), " to boolean.");
    case NumberTy:
      U->setValue(Value(!U->getSubExpr()->getValueAs<NumberTy>()));
      break;
    case StringTy:
      U->setValue(Value(!std::empty(U->getSubExpr()->getValueAs<StringTy>())));
      break;
    case NullTy:
      U->setValue(Value(true));
    }
  case tok::dollar: {
    traverse(U->getSubExpr());
    auto SubExpr = U->getSubExpr()->getValueAs<NumberTy>();
    if (SubExpr < 0) {
      U->setValue(Value());
    } else if (SubExpr >= std::size(FieldTable)) {
      FieldTable.resize(SubExpr + 1, new Value);
      U->setValue(FieldTable[SubExpr]);
    } else {
      U->setValue(FieldTable[SubExpr]);
    }
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
    CallStack.back()->setValue(Value(index(Args.front()->getAs<StringTy>(),
                                           Args.back()->getAs<StringTy>())));
  case tok::kw_match:
  case tok::kw_split:
  case tok::kw_sprintf:
    assert(!std::empty(Args) && "invalid call to sprintf");
    CallStack.back()->setValue(
        Value(sprintf(Args.front()->getAs<StringTy>(),
                      std::vector(std::cbegin(Args) + 1, std::cend(Args)))));
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

void Exec::initBuiltinVariables() {
  Environments.back()["FS"] = new Value(" ");
  Environments.back()["OFS"] = new Value(" ");
  Environments.back()["RS"] = new Value("\n");
  Environments.back()["ORS"] = new Value("\n");
  Environments.back()["NF"] = new Value(0.0);
}