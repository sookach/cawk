#include "Parse/Parser.h"
#include "Basic/TokenKinds.h"

namespace cawk {
void Parser::Advance(bool Regex) { Lex.Next(Tok, Regex); }

Token Parser::Peek(std::size_t N, bool Regex) const {
  Token T;
  auto BufferPtr = Lex.GetBufferPtr();

  for (; N != 0; --N)
    Lex.Next(T, Regex);

  Lex.SetBufferPtr(BufferPtr);
  return T;
}

Decl *Parser::ParseGlobalDecl() {
  Skip(tok::newline);

  return nullptr;
}

RuleDecl *Parser::ParseRuleDecl() { return nullptr; }

FunctionDecl *Parser::ParseFunctionDecl() {
  ExpectOneOf(tok::kw_func, tok::kw_function);

  auto Iden = Tok;
  Expect(tok::identifier, tok::l_paren);

  std::vector<std::string> Params;

  if (Tok.Is(tok::identifier)) {
    Params.emplace_back(Tok.GetLiteralData());
    Lex.Next(Tok);

    for (; Consume(tok::comma);) {
      Params.emplace_back(Tok.GetLiteralData());
      Expect(tok::identifier);
    }
  }

  Expect(tok::r_paren);

  return nullptr;
}

CompoundStmt *Parser::ParseCompoundStmt() {
  Expect(tok::l_brace);

  Expect(tok::r_brace);

  return nullptr;
}

} // namespace cawk
