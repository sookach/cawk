#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"
#include <algorithm>
#include <bitset>
#include <cstdlib>

namespace cawk {
class Parser {
  Lexer &Lex;
  Token Tok;
  bool HasError;

public:
  Parser(Lexer &Lex) : Lex(Lex), HasError(false) {}

  TranslationUnitDecl *Parse();

private:
  void Advance(bool = false);
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
    std::vector<Decl *> Decls;
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
    auto Params = [this] -> std::vector<ParamVarDecl *> {
      if (Tok.Is(tok::r_paren))
        return {};
      return ParseParamList();
    }();
    Expect(tok::r_paren);
    return FunctionDecl::Create(Identifier, Params, ParseCompoundStmt());
  }

  RuleDecl *ParseRuleDecl() {
    auto Pattern = [this] -> Expr * {
      if (Tok.Is(tok::l_brace))
        return nullptr;
      return ParseExpr();
    }();
    auto Action = Tok.Is(tok::l_brace) ? ParseCompoundStmt() : nullptr;
    return RuleDecl::Create(Pattern, Action);
  }

  std::vector<ParamVarDecl *> ParseParamList() {
    std::vector Params = {ParamVarDecl::Create(Tok)};

    for (Expect(tok::identifier); Consume(tok::comma); Expect(tok::identifier))
      Params.push_back(ParamVarDecl::Create(Tok));

    return Params;
  }

  CompoundStmt *ParseCompoundStmt() {
    Expect(tok::l_brace);
    std::vector<Stmt *> Stmts;
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
    Expect(tok::kw_print, tok::kw_printf);
    auto Args = ParsePrintArgs();

    return nullptr;
  }

  std::vector<Expr *> ParsePrintArgs() {
    if (Consume(tok::l_paren)) {
      auto Args = ParseExprList();
      Expect(tok::r_paren);
      return Args;
    }
    return ParsePrintExprList();
  }

  std::vector<Expr *> ParseExprList() {
    std::vector Exprs = {ParseExpr()};

    for (; Consume(tok::comma);) {
      Skip(tok::newline);
      Exprs.push_back(ParseExpr());
      assert(Exprs.back() != nullptr);
    }

    return Exprs;
  }

  std::vector<Expr *> ParsePrintExprList() {
    std::vector Exprs = {ParsePrintExpr()};

    for (; Consume(tok::comma);) {
      Skip(tok::newline);
      Exprs.push_back(ParsePrintExpr());
      assert(Exprs.back() != nullptr);
    }

    return Exprs;
  }

  Expr *ParseExpr() { return nullptr; }

  Expr *ParsePrintExpr() { return nullptr; }

  void St() {
    ExpectOneOf(tok::newline, tok::semi);
    Skip(tok::newline);
  }
};
} // namespace cawk
