#pragma once

#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"
#include <bitset>

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
  void Expect(tok::TokenKind);
  bool Consume(tok::TokenKind);
  Token Peek(std::size_t, bool = false) const;

  template <typename... Ts> void Skip(Ts... Ks) {
    std::bitset<tok::NUM_TOKENS> Filter((0 | ... | Ks));

    for (; Filter.test(Tok.GetKind()); Advance())
      ;
  }

  PatternActionDecl *ParsePatternAction();
};
} // namespace cawk
