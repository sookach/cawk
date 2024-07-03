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
  std::string_view::const_iterator BufferPrev;

  std::unordered_map<std::string_view, tok::TokenKind> Keywords;

public:
  Lexer(std::string_view Buffer)
      : BufferStart(std::cbegin(Buffer)), BufferEnd(std::cend(Buffer)),
        BufferPtr(BufferStart) {
#define KEYWORD(ID, KEY) Keywords.emplace(#ID, tok::kw_##ID);
#include "Basic/TokenKinds.def"
#undef KEYWORD
  }

  std::string_view::const_iterator getBufferPtr() const;
  void setBufferPtr(std::string_view::const_iterator);
  template <bool, bool> void next(Token &);
  void undo();

private:
  void formToken(Token &, std::string_view::const_iterator, tok::TokenKind);
  void identifier(Token &);
  void number(Token &);
  void string(Token &);
};
} // namespace cawk
