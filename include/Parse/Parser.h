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

  template <typename... Ts> bool ConsumeOneOf(tok::TokenKind K, Ts... Ks) {
    if (Consume(K))
      return true;

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

  TranslationUnitDecl *ParseTranslationUnit() {
    std::vector<Decl *> Decls;
    for (; (Skip(tok::semi, tok::newline), !Tok.Is(tok::eof));) {
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
        return DeclRefExpr::Create(Advance());
      }
    }();

    auto Action =
        ConsumeOneOf(tok::semi, tok::newline) ? nullptr : ParseCompoundStmt();

    return RuleDecl::Create(Pattern, Action);
  }

  CompoundStmt *ParseCompoundStmt() { return nullptr; }

  Expr *ParseExpr() { return nullptr; }
};
} // namespace cawk
