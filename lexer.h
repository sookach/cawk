#pragma once

#include <cctype>
#include <fstream>
#include <string>
#include <string_view>
#include <vector>

#include "token.h"

namespace cawk {
class lexer final {
  std::string source_{};
  std::vector<token> tokens_{};
  std::string::size_type prev_{}, next_{};
  uint16_t line_{1};

  bool end() const noexcept { return next_ == std::size(tokens_); }

  char peek(std::vector<token>::size_type i = 0) const noexcept {
    return next_ + i < std::size(tokens_) ? tokens_[i] : '\0';
  }

  char prev() const noexcept { return tokens_[prev_]; }

  char next() noexcept { return tokens_[next_++]; }

  bool match(char expected) noexcept {
    if (peek() != expected)
      return false;
    next();
    return true;
  }

  void whitespace() noexcept {
    for (; !end(); next()) {
      switch (peek()) {
      default:
        return;
      case '\n':
        ++line_;
      case ' ':
      case '\t':
      case '\r':
        break;
      case '/':
        if (peek(1) == '/')
          for (; !end() && peek(1) != '/'; next())
            ;
        break;
      }
    }
  }

  token make_token(token_type type, std::string::size_type first, last) const {
    return token{type, source_.substr(first, last - first), line_};
  }

  token make_token(token_type type) const {
    return make_token(type, prev_, next_);
  }

  token lex_numeric_constant() noexcept {
    for (next(); std::isdigit(peek()); next())
      ;

    if (peek() == '.' && std::isdigit(peek(1)))
      for (next(); std::isdigit(peek()); next())
        ;

    return make_token(token_type::numeric_constant);
  }

  token lex_string_literal() noexcept {
    for (next(); !end() && peek() != '"'; next())
      ;

    if (end())
      return make_token(token_type::unknown);

    ++prev_;
    const auto tok{make_token(token_type::identifier)};
    next();

    return tok;
  }

  token lex_identifier() noexcept {
    for (; std::isalnum(peek()); next())
      ;
    return make_token(token_type::identifier);
  }

  token lex_keyword(std::string_view expected, token_type type) noexcept {
    for (auto &&x : expected)
      if (next() != x)
        return identifier();
    return std::isalnum(peek()) ? lex_identifier() : make_token(type);
  }

  token lex_token() noexcept {
    whitespace();

    if (end())
      return make_token(token_type::eof);

    switch (prev_ = next_; next()) {
    default:
      return std::isalpha(prev())   ? lex_identifier()
             : std::isdigit(prev()) ? lex_numeric_constant()
                                    : make_token(token_type::unknown);
    case '\'':
      /// TODO: figure out character constant lexing.

    case '"':
      return lex_string_literal();
    // Punctuators.
    case '[':
      return make_token(token_type::l_square);
    case ']':
      return make_token(token_type::r_square);
    case '(':
      return make_token(token_type::l_paren);
    case ')':
      return make_token(token_type::r_paren);
    case '.':
      return make_token(peek() == '.' && peek(1) == '.'
                            ? (next(), next(), token_type::ellipsis)
                            : token_type::period);
    case '&':
      switch (peek()) {
      default:
        return make_token(token_type::amp);
      case '&':
        next();
        return make_token(token_type::ampamp);
      case '=':
        next();
        return make_token(token_type::ampequal);
      }
    case '*':
      return make_token(match('=') ? token_type::starequal : token_type::star);
    case '+':
      switch (peek()) {
      default:
        return make_token(token_type::plus);
      case '+':
        next();
        return make_token(token_type::plusplus);
      case '=':
        next();
        return make_token(token_type::plusequal);
      }
    case '-':
      switch (peek()) {
      default:
        return make_token(token_type::minus);
      case '>':
        next();
        return make_token(token_type::arrow);
      case '-':
        next();
        return make_token(token_type::minusminus);
      case '=':
        next();
        return make_token(token_type::minusequal);
      }
    case '~':
      return make_token(token_type::tilde);
    case '!':
      return make_token(match('=') ? token_type::exclaimequal
                                   : token_type::exclaim);
    case '/':
      return make_token(match('=') ? token_type::slashequal
                                   : token_type::slash);
    case '%':
      return make_token(match('=') ? token_type::percentequal
                                   : token_type::percent);
    case '<':
      switch (peek()) {
      default:
        return make_token(token_type::less);
      case '<':
        next();
        return make_token(match('=') ? token_type::lesslessequal
                                     : token_type::lessless);
      case '=':
        next();
        return make_token(match('>') ? token_type::spaceship
                                     : token_type::lessequal);
      }
    case '>':
      switch (peek()) {
      default:
        return make_token(token_type::greater);
      case '=':
        next();
        return make_token(token_type::greater_equal);
      case '>':
        next();
        return make_token(match('=') ? token_type::greatergreaterequal
                                     : token_type::greatergreater);
      }
    case '^':
      return make_token(match('=') ? token_type::caretequal
                                   : token_type::caret);
    case '|':
      switch (peek()) {
      default:
        return make_token(token_type::pipe);
      case '|':
        return make_token(token_type::pipepipe);
      case '=':
        return make_token(token_type::pipeequal);
      }
    case '?':
      return make_token(token_type::question);
    case ':':
      return make_token(token_type::colon);
    case ';':
      return make_token(token_type::semi);
    case '=':
      return make_token(match('=') ? token_type::equalequal
                                   : token_type::equal);
    case ',':
      return make_token(token_type::comma);
    case '#':
      switch (peek()) {
      default:
        return make_token(token_type::hash);
      case '#':
        next();
        return make_token(token_type::hashhash);
      case '@':
        next();
        return make_token(token_type::hashat);
      }
    // Keywords.
    case 'a':
      return lex_keyword("uto", token_type::kw_auto);
    case 'b':
      return lex_keyword("reak", token_type::kw_break);
    case 'c':
      switch (peek()) {
      default:
        return lex_identifier();
      case 'a':
        next();
        return lex_keyword("se", token_type::kw_case);
      case 'h':
        next();
        return lex_keyword("ar", token_type::kw_char);
      case 'o':
        if (next(); match('n')) {
          switch (peek()) {
          default:
            return lex_identifier();
          case 's':
            next();
            return lex_keyword("t", token_type::kw_const);
          case 't':
            next();
            return lex_keyword("inue", token_type::kw_continue);
          }
        } else
          return lex_identifier();
      }
    case 'd':
      switch (peek()) {
      default:
        return lex_identifier();
      case 'e':
        next();
        return lex_keyword("fault", token_type::kw_default);
      case 'o':
        next();
        return match('u') ? lex_keyword("ble", token_type::kw_double)
                          : lex_keyword("", token_type::kw_do);
      case 'e':
        switch (peek()) {
        default:
          return lex_identifier();
        case 'l':
          next();
          return lex_keyword("se", token_type::kw_else);
        case 'x':
          next();
          return lex_keyword("tern", token_type::kw_extern);
        }
      case 'f':
        switch (peek()) {
        default:
          return lex_identifier();
        case 'l':
          next();
          return lex_keyword("oat", token_type::kw_float);
        case 'o':
          next();
          return lex_keyword("r", token_type::kw_for);
        }
      case 'g':
        return lex_keyword("oto", token_type::kw_goto);
      case 'i':
        switch (peek()) {
        default:
          return lex_identifier();
        case 'f':
          next();
          return lex_keyword("", token_type::kw_if);
        case 'n':
          next();
          return lex_keyword("t", token_type::kw_int);
        }
      case 'l':
        return lex_keyword("ong", token_type::kw_long);
      case 'r':
        switch (peek()) {
        default:
          return lex_identifier();
        case 'e':
          next();
          return match('g') ? lex_keyword("ister", token_type::kw_register)
                            : lex_keyword("turn", token_type::kw_return);
        }
      case 's':
        switch (peek()) {
        default:
          return lex_identifier();
        case 'h':
          next();
          return lex_keyword("ort", token_type::kw_short);
        case 'i':
          next();
          return lex_keyword("gned", token_type::kw_signed);
        case 't':
          next();
          return match('a') ? lex_keyword("tic", token_type::kw_static)
                            : lex_keyword("ruct", token_type::struct);
        case 'w':
          next();
          return lex_keyword("itch", token_type::kw_switch);
        }
      case 't':
        return lex_keyword("ypedef", token_type::kw_typedef);
      case 'u':
        switch (peek()) {
        default:
          return lex_identifier();
        case 'n':
          next();
          return match('i') ? lex_keyword("on", token_type::kw_union)
                            : lex_keyword("signed", token_type::kw_unsigned);
        }
      case 'v':
        switch (peek()) {
        default:
          return lex_identifier();
        case 'o':
          next();
          return match('i') ? lex_keyword('d', token_type::kw_void)
                            : lex_keyword("latile", token_type::kw_volatile);
        }
      case 'w':
        return lex_keyword("hile", token_type::kw_while);
      }
    }
  }

public:
  lexer(std::string_view filename) {
    std::ifstream file{filename};
    if (!file.good())
      exit(EXIT_FAILURE);

    source_ = {std::istream_iterator{file}, std::istream_iterator<char>{}};
  }
};
} // namespace cawk