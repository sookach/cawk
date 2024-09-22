//===- Lexer.cpp - CAWK Language Lexer --------------------------*- C++ -*-===//
//
//  This file implements the Lexer interface.
//
//===----------------------------------------------------------------------===//

#include "Lexer/Lexer.h"
#include "Basic/TokenKinds.h"

#include <algorithm>

namespace charinfo {
inline bool isWhitespace(char c) {
  switch (c) {
  default:
    return false;
  case ' ':
  case '\t':
  case '\f':
  case '\v':
  case '\r':
    return true;
  }
}

inline bool isDigit(char c) {
  switch (c) {
  default:
    return false;
  case '0' ... '9':
    return true;
  }
}

inline bool isLetter(char c) {
  switch (c) {
  default:
    return false;
  case 'A' ... 'Z':
  case 'a' ... 'z':
    return true;
  }
}
} // namespace charinfo

using namespace cawk;
using Pointer = std::string_view::const_iterator;

/// getBufferPtr - Returns the current position in the input buffer.
Pointer Lexer::getBufferPtr() const { return BufferPtr; }

/// setBufferPtr - Sets the current position in the input buffer.
void Lexer::setBufferPtr(Pointer Ptr) { BufferPtr = Ptr; }

/// undo - Restores the previous position in the input buffer.
void Lexer::undo() { BufferPtr = BufferPrev; }

/// peek - Returns the Nth token ahead.
Token Lexer::peek(std::size_t N) {
  Token T;
  auto Ptr = BufferPtr;
  for (; N != 0; --N)
    next(T);
  BufferPtr = Ptr;
  return T;
}

/// formSpaceToken - Marks Result as a space token, with a source range spanning
/// [Ptr, Ptr + 1).
void Lexer::formSpaceToken(Token &Result, Pointer Ptr) {
  Result.Kind = tok::space;
  Result.Ptr = Ptr;
  Result.Length = 1;
}

/// formTokenWithChars - Marks Result as a token of type Kind, with a source
/// range spanning [BufferPtr, TokEnd), and updates BufferPtr to TokEnd.
void Lexer::formTokenWithChars(Token &Result, Pointer TokEnd,
                               tok::TokenKind Kind) {
  Result.Kind = Kind;
  Result.Ptr = BufferPtr;
  Result.Length = TokEnd - BufferPtr;
  Result.Line = Line;
  BufferPtr = TokEnd;
}

/// lexIdentifier - [_a-zA-Z]([_a-zA-Z0-9])*
void Lexer::lexIdentifier(Token &Result) {
  assert(charinfo::isLetter(*BufferPtr));
  auto End = BufferPtr + 1;
  for (; charinfo::isLetter(*End); ++End)
    ;
  std::string_view Name(BufferPtr, End);
  tok::TokenKind Kind =
      Keywords.contains(Name) ? Keywords.at(Name) : tok::identifier;
  formTokenWithChars(Result, End, Kind);
}

/// lexNumericConstant - [0-9]+(\.[0-9]+)?
void Lexer::lexNumericConstant(Token &Result) {
  assert(charinfo::isDigit(*BufferPtr));
  auto End = BufferPtr + 1;
  for (; charinfo::isDigit(*End); ++End)
    ;
  if (*End == '.')
    for (++End; charinfo::isDigit(*End); ++End)
      ;
  formTokenWithChars(Result, End, tok::numeric_constant);
  return;
}

/// lexStringLiteral - "([^"]|\\")*"
void Lexer::lexStringLiteral(Token &Result) {
  auto BeginLoc = BufferPtr;
  auto End = BufferPtr + 1;
  for (; End != BufferEnd && *End != '"'; ++End)
    if (*End == '\\')
      ++End;
  if (End == BufferEnd) {
    Diags.addError(SourceRange(BeginLoc, End), diag::lex_unterminated_string);
    formTokenWithChars(Result, End, tok::unknown);
    return;
  }
  formTokenWithChars(Result, End + 1, tok::string_literal);
}

/// lexRegexLiteral - /([^/|\\]|\\.)*/
void Lexer::lexRegexLiteral(Token &Result) {
  auto BeginLoc = BufferPtr;
  auto End = BufferPtr + 1;
  for (; End != BufferEnd && *End != '/'; ++End)
    if (*End == '\\')
      ++End;
  if (End == BufferEnd) {
    Diags.addError(SourceRange(BeginLoc, End), diag::lex_unterminated_regex);
    formTokenWithChars(Result, End, tok::unknown);
    return;
  }
  formTokenWithChars(Result, End + 1, tok::regex_literal);
}

/// next - Reads the next token from the input buffer and assigns it to Result.
void Lexer::next(Token &Result) {
  /// Peek N characters ahead. For now we treat the end of input as 0, but it
  /// might be worth changing it to EOF(-1) in the future.
  auto Peek = [this](std::size_t N = 1) -> char {
    auto Ptr = BufferPtr;
    for (; N != 0 && Ptr != BufferEnd; --N)
      Ptr = std::find_if_not(Ptr, BufferEnd, charinfo::isWhitespace);
    return Ptr == BufferEnd ? 0 : *Ptr;
  };
  BufferPrev = BufferPtr;

  BufferPtr = std::find_if_not(BufferPtr, BufferEnd, [this](char C) {
    Line += static_cast<std::size_t>(C == '\n');
    return charinfo::isWhitespace(C);
  });

  if (BufferPtr == BufferEnd || *BufferPtr == EOF) {
    Result.Kind = tok::eof;
    return;
  }

  switch (*BufferPtr) {
#define CASE(CH, TOK)                                                          \
  case CH:                                                                     \
    return formTokenWithChars(Result, BufferPtr + 1, TOK)
    CASE('[', tok::l_square);
    CASE(']', tok::r_square);
    CASE('(', tok::l_paren);
    CASE(')', tok::r_paren);
    CASE('{', tok::l_brace);
    CASE('}', tok::r_brace);
    CASE('?', tok::question);
    CASE(':', tok::colon);
    CASE(';', tok::semi);
    CASE(',', tok::comma);
    CASE('$', tok::dollar);
    CASE('~', tok::tilde);
#undef CASE
  case '#':
    BufferPtr = std::find(BufferPtr, BufferEnd, '\n');
    if (BufferPtr != BufferEnd)
      ++BufferPtr;
    return next(Result);
  case '0' ... '9':
    return lexNumericConstant(Result);
  case '"':
    return lexStringLiteral(Result);
  case 'A' ... 'Z':
  case 'a' ... 'z':
  case '_':
    return lexIdentifier(Result);
  case '&':
    return Peek() == '&'
               ? formTokenWithChars(Result, BufferPtr + 2, tok::ampamp)
               : formTokenWithChars(Result, BufferPtr + 1, tok::amp);
  case '|':
    switch (Peek()) {
    case '|':
      return formTokenWithChars(Result, BufferPtr + 2, tok::pipepipe);
    case '&':
      return formTokenWithChars(Result, BufferPtr + 2, tok::pipeamp);
    default:
      return formTokenWithChars(Result, BufferPtr + 1, tok::pipe);
    }
  case '=':
    return Peek() == '='
               ? formTokenWithChars(Result, BufferPtr + 2, tok::equalequal)
               : formTokenWithChars(Result, BufferPtr + 1, tok::equal);
  case '!':
    switch (Peek()) {
    case '=':
      return formTokenWithChars(Result, BufferPtr + 2, tok::exclaimequal);
    case '~':
      return formTokenWithChars(Result, BufferPtr + 2, tok::exclaimtilde);
    default:
      return formTokenWithChars(Result, BufferPtr + 1, tok::exclaim);
    }
  case '<':
    return Peek() == '='
               ? formTokenWithChars(Result, BufferPtr + 2, tok::lessequal)
               : formTokenWithChars(Result, BufferPtr + 1, tok::less);
  case '>':
    switch (Peek()) {
    case '>':
      return formTokenWithChars(Result, BufferPtr + 2, tok::greatergreater);
    case '=':
      return formTokenWithChars(Result, BufferPtr + 2, tok::greaterequal);
    default:
      return formTokenWithChars(Result, BufferPtr + 1, tok::greater);
    }
  case '+':
    switch (Peek()) {
    case '+':
      return formTokenWithChars(Result, BufferPtr + 2, tok::plusplus);
    case '=':
      return formTokenWithChars(Result, BufferPtr + 2, tok::plusequal);
    default:
      return formTokenWithChars(Result, BufferPtr + 1, tok::plus);
    }
  case '-':
    switch (Peek()) {
    case '-':
      return formTokenWithChars(Result, BufferPtr + 2, tok::minusminus);
    case '=':
      return formTokenWithChars(Result, BufferPtr + 2, tok::minusequal);
    default:
      return formTokenWithChars(Result, BufferPtr + 1, tok::minus);
    }
  case '*':
    switch (Peek()) {
    case '*':
      return Peek(2) == '='
                 ? formTokenWithChars(Result, BufferPtr + 3, tok::starstarequal)
                 : formTokenWithChars(Result, BufferPtr + 2, tok::starstar);
    case '=':
      return formTokenWithChars(Result, BufferPtr + 2, tok::starequal);
    default:
      return formTokenWithChars(Result, BufferPtr + 1, tok::star);
    }
  case '^':
    return Peek() == '='
               ? formTokenWithChars(Result, BufferPtr + 2, tok::caretequal)
               : formTokenWithChars(Result, BufferPtr + 1, tok::caret);
  case '%':
    return Peek() == '='
               ? formTokenWithChars(Result, BufferPtr + 2, tok::percentequal)
               : formTokenWithChars(Result, BufferPtr + 1, tok::percent);
  case '\n':
    return formTokenWithChars(Result, BufferPtr + 1, tok::newline);
  case '/':
    return formTokenWithChars(Result, BufferPtr + 1, tok::slash);
  default:
    formTokenWithChars(Result, BufferPtr + 1, tok::unknown);
  }
}