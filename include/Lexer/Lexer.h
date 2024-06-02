#pragma once

#include "Basic/TokenKinds.h"
#include "Token.h"

#include <string_view>
#include <unordered_map>

namespace cawk {
class Lexer {
  std::string_view::const_iterator BufferStart;
  std::string_view::const_iterator BufferEnd;
  std::string_view::const_iterator BufferPtr;

  std::unordered_map<std::string_view, tok::TokenKind> Keywords;

public:
  Lexer(std::string_view Buffer)
      : BufferStart(std::cbegin(Buffer)), BufferEnd(std::cend(Buffer)),
        BufferPtr(BufferStart) {
#define KEYWORD(ID, KEY) Keywords.emplace(#ID, tok::kw_##ID);
#include "Basic/TokenKinds.def"
#undef KEYWORD
  }

  std::string_view::const_iterator GetBufferPtr() const;
  void SetBufferPtr(std::string_view::const_iterator);
  void Next(Token &, bool = false);

private:
  void FormToken(Token &, std::string_view::const_iterator, tok::TokenKind);
  void Identifier(Token &);
  void Number(Token &);
  void String(Token &);
};
} // namespace cawk
