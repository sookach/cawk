#pragma once

#include "Basic/Diagnostic.h"
#include "Basic/TokenKinds.h"
#include "Exec/IO.h"
#include "Token.h"

#include <string_view>
#include <unordered_map>

namespace cawk {
class Lexer {
  using Pointer = std::string_view::const_iterator;

  Diagnostic &Diags;
  Pointer BufferStart;
  Pointer BufferEnd;
  Pointer BufferPtr;
  Pointer BufferPrev;

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

  Pointer getBufferPtr() const;
  void setBufferPtr(Pointer Ptr);
  void next(Token &Result);
  void lexRegexLiteral(Token &Result);
  Token peek(std::size_t N = 1);
  void undo();

  void formSpaceToken(Token &Result, Pointer It);

private:
  void formTokenWithChars(Token &Result, Pointer TokEnd, tok::TokenKind Kind);
  void lexIdentifier(Token &Result);
  void lexNumericConstant(Token &Result);
  void lexStringLiteral(Token &Result);

  std::size_t getLine() const { return Line; }
};
} // namespace cawk
