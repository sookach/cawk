#pragma once

#include "Basic/Diagnostic.h"
#include "Basic/TokenKinds.h"
#include "Exec/IO.h"
#include "Token.h"

#include <string_view>
#include <unordered_map>

namespace cawk {
class Lexer {
  Diagnostic &Diags;
  std::string_view::const_iterator BufferStart;
  std::string_view::const_iterator BufferEnd;
  std::string_view::const_iterator BufferPtr;
  std::string_view::const_iterator BufferPrev;

  std::unordered_map<std::string_view, tok::TokenKind> Keywords;

  std::size_t Line = 1;

public:
  Lexer(std::string_view Buffer, Diagnostic &Diags)
      : Diags(Diags), BufferStart(std::cbegin(Buffer)),
        BufferEnd(std::cend(Buffer)), BufferPtr(BufferStart) {
#define KEYWORD(ID, KEY) Keywords.emplace(#ID, tok::kw_##ID);
#include "Basic/TokenKinds.def"
#undef KEYWORD
  }

  std::string_view::const_iterator getBufferPtr() const;
  void setBufferPtr(std::string_view::const_iterator Ptr);
  template <bool NL, bool RE> void next(Token &T);
  void undo();

  void formSpaceToken(Token &T, std::string_view::const_iterator It) {
    T.Kind = tok::space;
    T.Ptr = It;
    T.Length = 1;
  }

private:
  void formToken(Token &T, std::string_view::const_iterator End,
                 tok::TokenKind Kind);
  void lexIdentifier(Token &T);
  void lexNumericConstant(Token &T);
  void lexStringLiteral(Token &T);
  void lexRegexLiteral(Token &T);

  std::size_t getLine() const { return Line; }
};
} // namespace cawk
