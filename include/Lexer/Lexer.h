#pragma once

#include "Basic/TokenKinds.h"
#include "Exec/IO.h"
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

  std::size_t Line = 1;

public:
  Lexer(std::string_view Buffer)
      : BufferStart(std::cbegin(Buffer)), BufferEnd(std::cend(Buffer)),
        BufferPtr(BufferStart) {
#define KEYWORD(ID, KEY) Keywords.emplace(#ID, tok::kw_##ID);
#include "Basic/TokenKinds.def"
#undef KEYWORD
  }

  std::string_view::const_iterator getBufferPtr() const;
  void setBufferPtr(std::string_view::const_iterator Ptr);
  template <bool, bool> void next(Token &T);
  void undo();

private:
  void formToken(Token &T, std::string_view::const_iterator End,
                 tok::TokenKind Kind);
};
} // namespace cawk
