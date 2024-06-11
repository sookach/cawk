#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"
#include <algorithm>
#include <bitset>
#include <cstdlib>
#include <initializer_list>
#include <iostream>
#include <ranges>

namespace cawk {
class Parser {
  Lexer &Lex;
  Token Tok;
  bool HasError;

public:
  Parser(Lexer &Lex) : Lex(Lex), HasError(false) { Lex.Next(Tok); }

  TranslationUnitDecl *Parse();

private:
  Token Advance(bool = false);
  Token Peek(std::size_t, bool = false) const;

  template <typename... Ts> bool Consume(tok::TokenKind K, Ts... Ks) {
    if (Tok.Is(K)) {
      Lex.Next(Tok);
      return true;
    }

    if constexpr (sizeof...(Ks) != 0)
      return Consume(Ks...);

    return false;
  }

  template <typename... Ts> void Expect(tok::TokenKind K, Ts... Ks) {
    if (!Consume(K))
      exit(EXIT_FAILURE); // TODO: error handling

    if constexpr (sizeof...(Ks) != 0)
      Expect(Ks...);
  }

  template <typename... Ts> void ExpectOneOf(tok::TokenKind K, Ts... Ks) {
    if (Consume(K))
      return;

    if constexpr (sizeof...(Ks) == 0)
      exit(EXIT_FAILURE); // TODO: error handling
    else
      ExpectOneOf(Ks...);
  }

  template <typename... Ts> void Skip(Ts... Ks) {
    std::bitset<tok::NUM_TOKENS> Filter((0 | ... | Ks));

    for (; Filter.test(Tok.GetKind()); Advance())
      ;
  }

  TranslationUnitDecl *ParseTranslationUnitDecl() {
    Sequence<Decl *> Decls;
    for (Skip(tok::newline); !Consume(tok::eof); Skip(tok::newline))
      Decls.push_back(ParseGlobalDecl());
    return TranslationUnitDecl::Create(Decls);
  }

  Decl *ParseGlobalDecl() {
    if (Tok.Is(tok::kw_function))
      return ParseFunctionDecl();
    return ParseRuleDecl();
  }

  FunctionDecl *ParseFunctionDecl() {
    Expect(tok::kw_function);
    auto Identifier = Tok;
    Advance();
    Expect(tok::l_paren);
    auto Params = [this] -> Sequence<ParamVarDecl *> {
      if (Tok.Is(tok::r_paren))
        return {};
      return ParseParamList();
    }();
    Expect(tok::r_paren);
    return FunctionDecl::Create(Identifier, Params, ParseCompoundStmt());
  }

  RuleDecl *ParseRuleDecl() {
    auto Pattern = [this] -> Expr * {
      switch (Tok.GetKind()) {
      default:
        return ParseExpr();
      case tok::l_brace:
        return nullptr;
      case tok::kw_BEGIN:
      case tok::kw_END:
        return DeclRefExpr::Create(Advance());
      }
    }();
    auto Action = Tok.Is(tok::l_brace) ? ParseCompoundStmt() : nullptr;
    return RuleDecl::Create(Pattern, Action);
  }

  Sequence<ParamVarDecl *> ParseParamList() {
    Sequence Params = {ParamVarDecl::Create(Tok)};

    for (Expect(tok::identifier); Consume(tok::comma); Expect(tok::identifier))
      Params.push_back(ParamVarDecl::Create(Tok));

    return Params;
  }

  CompoundStmt *ParseCompoundStmt() {
    Expect(tok::l_brace);
    Sequence<Stmt *> Stmts;
    for (Skip(tok::newline, tok::semi); !Tok.Is(tok::r_brace);
         Skip(tok::newline, tok::semi))
      Stmts.push_back(ParseStmt());

    Expect(tok::r_brace);
    return nullptr;
  }

  Stmt *ParseStmt() {
    switch (Tok.GetKind()) {
    default:
      return ParseSimpleStmt();
    case tok::kw_break:
      return ParseBreakStmt();
    case tok::kw_continue:
      return ParseContinueStmt();
    case tok::kw_do:
      return ParseDoStmt();
    case tok::kw_exit:
      return ParseExitStmt();
    case tok::kw_for:
      return ParseForStmt();
    case tok::kw_if:
      return ParseIfStmt();
    case tok::l_brace:
      return ParseCompoundStmt();
    case tok::kw_next:
      return ParseNextStmt();
    case tok::kw_nextfile:
      return ParseNextfileStmt();
    case tok::kw_return:
      return ParseReturnStmt();
    case tok::kw_while:
      return ParseWhileStmt();
    case tok::semi:
      Expect(tok::semi);
      Skip(tok::newline);
    }
    return nullptr;
  }

  BreakStmt *ParseBreakStmt() {
    Expect(tok::kw_break);
    St();
    return BreakStmt::Create();
  }

  ContinueStmt *ParseContinueStmt() {
    Expect(tok::kw_continue);
    St();
    return ContinueStmt::Create();
  }

  DoStmt *ParseDoStmt() {
    Expect(tok::kw_do);
    auto Body = ParseStmt();
    Expect(tok::kw_while, tok::l_paren);
    auto Cond = ParseExpr();
    Expect(tok::r_paren);
    St();
    return DoStmt::Create(Cond, Body);
  }

  ExitStmt *ParseExitStmt() {
    Expect(tok::kw_exit);
    auto ExitCode = !Tok.Is(tok::semi, tok::newline) ? ParseExpr() : nullptr;
    St();
    return ExitStmt::Create(ExitCode);
  }

  Stmt *ParseForStmt() {
    Expect(tok::kw_for, tok::r_paren);
    if (Peek(1).Is(tok::kw_in)) {
      auto LoopVar = DeclRefExpr::Create(Tok);
      Expect(tok::identifier, tok::kw_in);
      auto Range = DeclRefExpr::Create(Tok);
      Expect(tok::identifier, tok::r_paren);
      Skip(tok::newline);
      auto Body = ParseStmt();
      return ForRangeStmt::Create(LoopVar, Range, Body);
    } else {
      auto Init = ParseSimpleStmt();
      Expect(tok::semi);
      auto Cond = [this] -> Expr * {
        if (Tok.Is(tok::semi))
          return nullptr;
        auto Cond = ParseExpr();
        assert(Cond);
        return Cond;
      }();
      Expect(tok::semi);
      Skip(tok::newline);
      auto Inc = ParseSimpleStmt();
      Expect(tok::r_paren);
      Skip(tok::newline);
      auto Body = ParseStmt();
      return ForStmt::Create(Init, Cond, Inc, Body);
    }
  }

  IfStmt *ParseIfStmt() {
    Expect(tok::kw_if, tok::l_paren);
    auto Cond = ParseExpr();
    Expect(tok::r_paren);
    auto Then = ParseStmt();
    auto Else =
        Consume(tok::kw_else) ? (Skip(tok::newline), ParseStmt()) : nullptr;
    return IfStmt::Create(Cond, Then, Else);
  }

  NextStmt *ParseNextStmt() {
    Expect(tok::kw_next);
    St();
    return NextStmt::Create();
  }

  NextfileStmt *ParseNextfileStmt() {
    Expect(tok::kw_next);
    St();
    return NextfileStmt::Create();
  }

  ReturnStmt *ParseReturnStmt() {
    Expect(tok::kw_return);
    auto Value = ParseExpr();
    St();
    return ReturnStmt::Create(Value);
  }

  WhileStmt *ParseWhileStmt() {
    Expect(tok::kw_while, tok::l_paren);
    auto Cond = ParseExpr();
    Expect(tok::r_paren);
    Skip(tok::newline);
    auto Body = ParseStmt();
    return WhileStmt::Create(Cond, Body);
  }

  Stmt *ParseSimpleStmt() {
    switch (Tok.GetKind()) {
    default:
      return ValueStmt::Create(ParseExpr());
    case tok::kw_print:
    case tok::kw_printf:
      return ParsePrintStmt();
    }

    return nullptr;
  }

  PrintStmt *ParsePrintStmt() {
    auto Kind = Tok;
    ExpectOneOf(tok::kw_print, tok::kw_printf);
    auto Args = ParsePrintArgs();

    return nullptr;
  }

  Sequence<Expr *> ParsePrintArgs() {
    if (Consume(tok::l_paren)) {
      auto Args = ParseExprList();
      Expect(tok::r_paren);
      return Args;
    }
    return ParseExprList();
  }

  template <bool IsPrint = false> Sequence<Expr *> ParseExprList() {
    Sequence Exprs = {ParseExpr()};

    for (; Consume(tok::comma);) {
      Skip(tok::newline);
      Exprs.push_back(ParseExpr());
      assert(Exprs.back() != nullptr);
    }

    return Exprs;
  }

  template <bool IsPrintExpr = false> Expr *ParseExpr(int RBP = 0) {
    auto BindingPower = [](tok::TokenKind K) {
      switch (K) {
      default:
        return 0;
      case tok::equal:
      case tok::plusequal:
      case tok::minusequal:
      case tok::starequal:
      case tok::slashequal:
      case tok::percentequal:
      case tok::caretequal:
        return 1;
      case tok::question:
        return 2;
      case tok::pipepipe:
        return 3;
      case tok::ampamp:
        return 4;
      case tok::kw_in:
        return 5;
      case tok::less:
      case tok::lessequal:
      case tok::equalequal:
      case tok::exclaimequal:
      case tok::greaterequal:
      case tok::greater:
      case tok::pipe:
        if constexpr (IsPrintExpr)
          return 0;
        return 6;
      case tok::identifier:
      case tok::string_literal:
        return 7;
      case tok::plus:
      case tok::minus:
        return 8;
      case tok::star:
      case tok::slash:
      case tok::percent:
        return 9;
      case tok::caret:
        return 10;
      }
    };

    auto LHS = [this] -> Expr * {
      switch (Tok.GetKind()) {
      default:
        assert("undefined nud");
        return nullptr;
      case tok::slash: {
        Lex.Undo();
        Lex.Next(Tok, true);
        auto T = Tok;
        Expect(tok::regex_literal);
        return RegexLiteral::Create(T);
      }
      case tok::numeric_constant: {
        auto T = Tok;
        Expect(tok::numeric_constant);
        return FloatingLiteral::Create(T);
      }
      case tok::string_literal: {
        auto T = Tok;
        Expect(tok::string_literal);
        return StringLiteral::Create(T);
      }
      case tok::identifier: {
        auto T = Tok;
        Expect(tok::identifier);
        return DeclRefExpr::Create(T);
      }
      case tok::plus:
      case tok::minus:
      case tok::plusplus:
      case tok::minusminus:
      case tok::exclaim:
      case tok::dollar: {
        auto OpCode = Tok;
        ExpectOneOf(tok::plus, tok::minus, tok::plusplus, tok::minusminus,
                    tok::exclaim, tok::dollar);
        return UnaryOperator::Create(OpCode,
                                     ParseExpr(std::numeric_limits<int>::max()),
                                     UnaryOperator::FixKind::Prefix);
      }
      case tok::l_paren: {
        Expect(tok::l_paren);
        auto E = ParseExpr();
        Expect(tok::r_paren);
        return E;
      }
      }
    }();

    switch (Tok.GetKind()) {
    default:
      break;
    case tok::l_square: {
      Expect(tok::l_square);
      LHS = std::ranges::fold_left(
          ParseExprList(), LHS, [](Expr *LHS, Expr *Idx) {
            return static_cast<Expr *>(ArraySubscriptExpr::Create(LHS, Idx));
          });
      Expect(tok::r_square);
      break;
    }
    case tok::l_paren: {
      Expect(tok::l_paren);
      LHS = CallExpr::Create(LHS, ParseExprList());
      Expect(tok::r_paren);
      break;
    }
    case tok::plusplus:
    case tok::minusminus: {
      auto Opcode = Tok;
      ExpectOneOf(tok::plusplus, tok::minusminus);
      LHS = UnaryOperator::Create(Tok, LHS, UnaryOperator::FixKind::Postfix);
    }
    }

    for (; BindingPower(Tok.GetKind()) > RBP;) {
      auto OpCode = Advance();
      auto BP = BindingPower(OpCode.GetKind()) - [](tok::TokenKind Kind) {
        switch (Kind) {
        default:
          return 0;
        case tok::caret:
        case tok::equal:
        case tok::plusequal:
        case tok::minusequal:
        case tok::starequal:
        case tok::slashequal:
        case tok::percentequal:
        case tok::caretequal:
          return 1;
        }
      }(OpCode.GetKind());
      LHS = BinaryOperator::Create(LHS, ParseExpr(BP), OpCode);
    }

    return LHS;
  }

  void St() {
    ExpectOneOf(tok::newline, tok::semi);
    Skip(tok::newline);
  }
};
} // namespace cawk
