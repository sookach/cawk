#include "Lexer/Lexer.h"
#include "Basic/TokenKinds.h"

#include <algorithm>

namespace charinfo {
template <bool Newline = false> inline bool isWhitespace(char c) {
  switch (c) {
  default:
    return false;
  case ' ':
  case '\t':
  case '\f':
  case '\v':
  case '\r':
    return true;
  case '\n': // Newline has syntactic importance in AWK
    if constexpr (Newline)
      return false;
    return true;
  }
}

template bool isWhitespace<true>(char);
template bool isWhitespace<false>(char);

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

namespace cawk {

std::string_view::const_iterator Lexer::getBufferPtr() const {
  return BufferPtr;
}

void Lexer::setBufferPtr(std::string_view::const_iterator Ptr) {
  BufferPtr = Ptr;
}

void Lexer::undo() { BufferPtr = BufferPrev; }

void Lexer::formToken(Token &T, std::string_view::const_iterator End,
                      tok::TokenKind Kind) {
  T.Kind = Kind;
  T.Ptr = BufferPtr;
  T.Length = End - BufferPtr;
  T.Line = Line;
  BufferPtr = End;
}

void Lexer::lexIdentifier(Token &T) {
  auto End = BufferPtr + 1;
  for (; charinfo::isLetter(*End); ++End)
    ;
  std::string_view Name(BufferPtr, End);
  tok::TokenKind Kind =
      Keywords.contains(Name) ? Keywords.at(Name) : tok::identifier;
  formToken(T, End, Kind);
}

void Lexer::lexNumericConstant(Token &T) {
  auto End = BufferPtr + 1;
  for (; charinfo::isDigit(*End); ++End)
    ;
  if (*BufferPtr == '.')
    for (++BufferPtr; charinfo::isDigit(*BufferPtr); ++BufferPtr)
      ;
  formToken(T, End, tok::numeric_constant);
  return;
}

void Lexer::lexStringLiteral(Token &T) {
  auto BeginLoc = BufferPtr;
  auto End = BufferPtr + 1;
  for (; End != BufferEnd && *End != '\''; ++End)
    if (*End == '\\')
      ++End;
  if (End == BufferEnd) {
    Diags.addError(SourceRange(BeginLoc, End), diag::lex_unterminated_string);
    formToken(T, End, tok::unknown);
    return;
  }
  formToken(T, End + 1, tok::string_literal);
}

void Lexer::lexRegexLiteral(Token &T) {
  auto BeginLoc = BufferPtr;
  auto End = BufferPtr + 1;
  for (; End != BufferEnd && *End != '/'; ++End)
    if (*End == '\\')
      ++End;
  if (End == BufferEnd) {
    Diags.addError(SourceRange(BeginLoc, End), diag::lex_unterminated_regex);
    formToken(T, End, tok::unknown);
    return;
  }
  formToken(T, End + 1, tok::regex_literal);
}

template <bool Newline, bool Regex> void Lexer::next(Token &T) {
  BufferPrev = BufferPtr;

  BufferPtr = std::find_if_not(BufferPtr, BufferEnd, [this](char C) {
    Line += static_cast<std::size_t>(C == '\n');
    return charinfo::isWhitespace<Newline>(C);
  });

  if (BufferPtr == BufferEnd || *BufferPtr == EOF) {
    T.Kind = tok::eof;
    return;
  }

  switch (*BufferPtr) {
#define CASE(CH, TOK)                                                          \
  case CH:                                                                     \
    formToken(T, BufferPtr + 1, TOK);                                          \
    break
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
  case '0' ... '9':
    lexNumericConstant(T);
    break;
  case '"':
    lexStringLiteral(T);
    break;
  case 'A' ... 'Z':
  case 'a' ... 'z':
  case '_':
    lexIdentifier(T);
    break;
  case '&':
    if (*(BufferPtr + 1) == '&')
      formToken(T, BufferPtr + 2, tok::ampamp);
    else
      formToken(T, BufferPtr + 1, tok::amp);
    break;
  case '|':
    if (*(BufferPtr + 1) == '|')
      formToken(T, BufferPtr + 2, tok::pipepipe);
    else if (*(BufferPtr + 1) == '&')
      formToken(T, BufferPtr + 2, tok::pipeamp);
    else
      formToken(T, BufferPtr + 1, tok::pipe);
    break;
  case '=':
    if (*(BufferPtr + 1) == '=')
      formToken(T, BufferPtr + 2, tok::equalequal);
    else
      formToken(T, BufferPtr + 1, tok::equal);
    break;
  case '!':
    if (*(BufferPtr + 1) == '=')
      formToken(T, BufferPtr + 2, tok::exclaimequal);
    else if (*(BufferPtr + 1) == '~')
      formToken(T, BufferPtr + 2, tok::exclaimtilde);
    else
      formToken(T, BufferPtr + 1, tok::exclaim);
    break;
  case '<':
    if (*(BufferPtr + 1) == '=')
      formToken(T, BufferPtr + 2, tok::lessequal);
    else
      formToken(T, BufferPtr + 1, tok::less);
    break;
  case '>':
    if (*(BufferPtr + 1) == '>')
      formToken(T, BufferPtr + 2, tok::greatergreater);
    else if (*(BufferPtr + 1) == '=')
      formToken(T, BufferPtr + 2, tok::greaterequal);
    else
      formToken(T, BufferPtr + 1, tok::greater);
    break;
  case '+':
    if (*(BufferPtr + 1) == '+')
      formToken(T, BufferPtr + 2, tok::plusplus);
    else if (*(BufferPtr + 1) == '=')
      formToken(T, BufferPtr + 2, tok::plusequal);
    else
      formToken(T, BufferPtr + 1, tok::plus);
    break;
  case '-':
    if (*(BufferPtr + 1) == '-')
      formToken(T, BufferPtr + 2, tok::minusminus);
    else if (*(BufferPtr + 1) == '=')
      formToken(T, BufferPtr + 2, tok::minusequal);
    else
      formToken(T, BufferPtr + 1, tok::minus);
    break;
  case '*':
    if (*(BufferPtr + 1) == '*') {
      if (*(BufferPtr + 2) == '=')
        formToken(T, BufferPtr + 3, tok::starstarequal);
      else
        formToken(T, BufferPtr + 2, tok::starstar);
    } else if (*(BufferPtr + 1) == '=') {
      formToken(T, BufferPtr + 2, tok::starequal);
    } else {
      formToken(T, BufferPtr + 1, tok::star);
    }
    break;
  case '^':
    if (*(BufferPtr + 1) == '=')
      formToken(T, BufferPtr + 2, tok::caretequal);
    else
      formToken(T, BufferPtr + 1, tok::caret);
    break;
  case '%':
    if (*(BufferPtr + 1) == '=')
      formToken(T, BufferPtr + 2, tok::percentequal);
    else
      formToken(T, BufferPtr + 1, tok::percent);
    break;
  case '\n':
    formToken(T, BufferPtr + 1, tok::newline);
    break;
  case '/':
    if constexpr (Regex) {
      lexRegexLiteral(T);
    } else {
      return formToken(T, BufferPtr + 1, tok::slash);
    }
  default:
    formToken(T, BufferPtr + 1, tok::unknown);
  }
}

template void Lexer::next<false, false>(Token &);
template void Lexer::next<false, true>(Token &);
template void Lexer::next<true, false>(Token &);
template void Lexer::next<true, true>(Token &);

} // namespace cawk
