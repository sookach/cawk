#include "Parse/Parser.h"
#include "Basic/TokenKinds.h"

namespace cawk {
void Parser::Advance(bool Regex) { Lex.Next(Tok, Regex); }

Token Parser::Peek(std::size_t N, bool Regex) const {
  Token T;

  for (; N != 0; --N)
    Lex.Next(T, Regex);

  return T;
}

bool Parser::Consume(tok::TokenKind Kind) {
  if (!Tok.Is(Kind))
    return false;
  Advance();
  return true;
}

void Parser::Expect(tok::TokenKind Kind) {
  if (!Consume(Kind))
    exit(EXIT_FAILURE);
}



PatternActionDecl *Parser::ParsePatternAction() {
  Skip(tok::newline);
  return nullptr;
}

} // namespace cawk
