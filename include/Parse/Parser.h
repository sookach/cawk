#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"

#include <algorithm>
#include <bitset>
#include <cstdlib>
#include <initializer_list>

namespace cawk {
class Parser {
  Lexer &Lex;
  Token Tok;
  bool HasError;

public:
  Parser(Lexer &Lex) : Lex(Lex), HasError(false) {
    Lex.Next<false, false>(Tok);
  }

  TranslationUnitDecl *Parse();

private:
  template <bool = false, bool = false> Token Advance();
  template <bool = false, bool = false> Token Peek(std::size_t) const;

  template <bool NL = false, bool RE = false, typename... Ts>
  bool Consume(tok::TokenKind K, Ts... Ks) {
    if (Tok.Is(K)) {
      Lex.Next<NL, RE>(Tok);
      return true;
    }

    if constexpr (sizeof...(Ks) != 0)
      return Consume<NL, RE>(Ks...);

    return false;
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  bool ConsumeOneOf(tok::TokenKind K, Ts... Ks) {
    if (Consume<NL, RE>(K))
      return true;

    if constexpr (sizeof...(Ks) != 0)
      return Consume<NL, RE>(Ks...);

    return false;
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  void Expect(tok::TokenKind K, Ts... Ks) {
    if (!Consume<NL, RE>(K))
      exit(EXIT_FAILURE); // TODO: error handling

    if constexpr (sizeof...(Ks) != 0)
      Expect<NL, RE>(Ks...);
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  void ExpectOneOf(tok::TokenKind K, Ts... Ks) {
    if (Consume<NL, RE>(K))
      return;

    if constexpr (sizeof...(Ks) == 0)
      exit(EXIT_FAILURE); // TODO: error handling
    else
      ExpectOneOf<NL, RE>(Ks...);
  }

  template <tok::TokenKind... Ks> void Skip() {
    std::bitset<tok::NUM_TOKENS> Filter((0 | ... | Ks));

    for (; Filter.test(Tok.GetKind()); Advance<false, false>())
      ;
  }

  TranslationUnitDecl *ParseTranslationUnit() {
    std::vector<Decl *> Decls;
    for (; (Skip<tok::newline, tok::semi>(), !Tok.Is(tok::eof));) {
      Decls.push_back(ParseDecl());
    }
    return TranslationUnitDecl::Create(Decls);
  }

  Decl *ParseDecl() {
    if (Tok.Is(tok::kw_func))
      return ParseFunctionDecl();
    return ParseRuleDecl();
  }

  FunctionDecl *ParseFunctionDecl() {
    Expect(tok::kw_function);
    auto Identifier = Tok;
    Expect(tok::identifier);
    Expect(tok::l_paren);
    auto Params = [this] {
      std::vector<ParamVarDecl *> Params;

      if (Tok.Is(tok::identifier))
        Params.push_back(ParamVarDecl::Create(Advance()));

      for (; Consume(tok::comma);) {
        Params.push_back(ParamVarDecl::Create(Tok));
        Expect(tok::identifier);
      }

      return Params;
    }();
    auto Body = ParseCompoundStmt();
    return FunctionDecl::Create(Identifier, Params, Body);
  }

  RuleDecl *ParseRuleDecl() {
    auto Pattern = [this] -> Expr * {
      switch (Tok.GetKind()) {
      default:
        return ParseExpr();
      case tok::kw_BEGIN:
      case tok::kw_END:
        return DeclRefExpr::Create(Advance<false>());
      }
    }();

    auto Action =
        ConsumeOneOf(tok::semi, tok::newline) ? nullptr : ParseCompoundStmt();

    return RuleDecl::Create(Pattern, Action);
  }

  CompoundStmt *ParseCompoundStmt() {
    Expect(tok::l_brace);
    std::vector<Stmt *> Stmts;
    for (; (Skip<tok::newline, tok::semi>(), !Tok.Is(tok::r_brace, tok::eof));)
      Stmts.push_back(ParseStmt());

    Expect(tok::r_brace);
    return CompoundStmt::Create(Stmts);
  }

  Stmt *ParseStmt() {
    switch (Tok.GetKind()) {
    default:
      return ParseSimpleStmt();
    case tok::l_brace:
      return ParseCompoundStmt();
    case tok::kw_if:
      return ParseIfStmt();
    case tok::kw_for:
      return ParseForStmt();
    }
  }

  Stmt *ParseSimpleStmt() {
    switch (Tok.GetKind()) {
    default:
      return ParseValueStmt();
    case tok::kw_print:
    case tok::kw_printf:
      return ParsePrintStmt();
    }
  }

  ValueStmt *ParseValueStmt() {
    Expr *Value = ParseExpr();
    ExpectOneOf(tok::semi, tok::newline);
    return ValueStmt::Create(Value);
  }

  IfStmt *ParseIfStmt() {
    Expect(tok::kw_if);
    Expect(tok::l_paren);
    Expr *Cond = ParseExpr();
    Expect(tok::r_paren);
    Stmt *Then = ParseStmt();
    Stmt *Else = Consume(tok::kw_else) ? ParseStmt() : nullptr;
    return IfStmt::Create(Cond, Then, Else);
  }

  Stmt *ParseForStmt() {
    Expect(tok::kw_for);
    Expect(tok::l_paren);

    if (Peek(1).Is(tok::kw_in)) {
      DeclRefExpr *LoopVar = DeclRefExpr::Create(Advance());
      Expect(tok::kw_in);
      DeclRefExpr *Range = DeclRefExpr::Create(Advance());
      Expect(tok::l_paren);
      return ForRangeStmt::Create(LoopVar, Range, ParseStmt());
    }

    Stmt *Init = Tok.Is(tok::semi) ? nullptr : ParseSimpleStmt();
    Expect(tok::semi);
    Expr *Cond = Tok.Is(tok::semi) ? nullptr : ParseExpr();
    Expect(tok::semi);
    Stmt *Inc = Tok.Is(tok::r_paren) ? nullptr : ParseSimpleStmt();
    Expect(tok::r_paren);
    return ForStmt::Create(Init, Cond, Inc, ParseStmt());
  }

  PrintStmt *ParsePrintStmt() {
    auto OpCode = Tok;
    ExpectOneOf<true>(tok::kw_print, tok::kw_printf);
    
  }

  Expr *ParseExpr() { return nullptr; }
};
} // namespace cawk
