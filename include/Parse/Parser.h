#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"
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

  TranslationUnitDecl *ParseProgram();
  Decl *ParseItem();
  std::vector<ParamVarDecl *> ParseParamList();
  Expr *ParsePattern();
  Expr *ParseNormalPattern();
  DeclRefExpr *ParseSpecialPattern();
  CompoundStmt *ParseAction();
  bool ParseTerminator();
};
} // namespace cawk
