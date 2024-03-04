//===- lexer.h - cawk language lexer --------------------------------------===//
//
//  This file defines and implements the lexer interface.
//
//===----------------------------------------------------------------------===//

#pragma once

#include <cctype>
#include <fstream>
#include <string>
#include <string_view>
#include <vector>

#include "token.h"

namespace cawk {

/// Lexer - A very basic interface for reading a source file and converting it
/// into a stream of tokens.
class lexer final {
  /// The contents of the source file.
  std::string source_{};

  /// Start of the lexeme.
  std::string::size_type prev_{};

  /// End of the lexeme.
  std::string::size_type next_{};

  /// Current line the lexer is handling.
  uint16_t line_{1};

  inline static constexpr struct {
    [[nodiscard]] __attribute__((const)) inline constexpr bool
    operator()(char x) const {
      return std::isalpha(x) || x == '_';
    }
  } isalpha{};

  inline static constexpr struct {
    [[nodiscard]] __attribute__((const)) inline constexpr bool
    operator()(char x) const {
      return isalpha(x) || std::isdigit(x);
    }
  } isalnum{};

  /// @brief end - Check if entire source was lexed.
  /// @return true if end of input, false otherwise.
  [[nodiscard]] __attribute__((const)) constexpr bool end() const noexcept {
    return next_ == std::size(source_);
  }

  /// @brief peek - Peek an arbitrary number of characters ahead in the input
  /// stream.
  /// @param i The lookahed amount (default is 0).
  /// @return The character at source_[next_ + i];
  [[nodiscard]] __attribute__((const)) constexpr char
  peek(std::string::size_type i = 0) const noexcept {
    return next_ + i < std::size(source_) ? source_[next_ + i] : '\0';
  }

  /// @brief prev - Get starting character of lexeme.
  /// @return value at source_[prev_].
  [[nodiscard]] __attribute__((const)) constexpr char prev() const noexcept {
    return source_[prev_];
  }

  /// @brief next - Advances the next_ pointer.
  /// @return value at source_[next_].
  constexpr char next() noexcept { return source_[next_++]; }

  /// @brief match - If the current character matches the expected, advance.
  /// @param expected The expected character.
  /// @return true if current character matches expected, false otherwise.
  [[nodiscard]] constexpr bool match(char expected) noexcept {
    if (peek() != expected)
      return false;
    next();
    return true;
  }

  /// @brief whitespace - Skips all whitespace including comments.
  constexpr void whitespace() noexcept {
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
        return;
      }
    }
  }

  /// @brief make_token - Helper function for constructing a token. Constructs a
  /// token with the given type, the current lexeme, and current line.
  /// @param type token_type.
  /// @return A token with values set as described above.
  [[nodiscard]] __attribute__((const)) constexpr token
  make_token(token_type type) const {
    return token{type, source_.substr(prev_, next_ - prev_), line_};
  }

  /// @brief lex_numeric_constant - Lexes a numeric constant. Handles both
  /// integers and real numbers.
  /// @return A token with the lexeme as the string literal of the numeric.
  [[nodiscard]] constexpr token lex_numeric_constant() noexcept {
    for (; std::isdigit(peek()); next())
      ;

    if (peek() == '.' && std::isdigit(peek(1)))
      for (next(); std::isdigit(peek()); next())
        ;

    return make_token(token_type::numeric_constant);
  }

  /// @brief lex_string_literal - Lexes a string literal. Allows multi-line
  /// strings.
  /// @return The lexed string literal.
  [[nodiscard]] constexpr token lex_string_literal() noexcept {
    for (; !end() && peek() != '"'; next())
      ;

    if (end()) [[unlikely]]
      return make_token(token_type::unknown);

    ++prev_;
    const auto tok{make_token(token_type::string_literal)};
    next();

    return tok;
  }

  /// @brief lex_string_literal - Lexes a character constant. Uses the same
  /// process as string literal lexing so it will allow completely invalid
  /// characters, which will get caught in the final build.
  /// TODO: should catch invalid characters here instead of pasing the buck.
  /// @return The lexed string literal.
  [[nodiscard]] constexpr token lex_char_constant() noexcept {
    for (; !end() && peek() != '\''; next())
      ;

    if (end()) [[unlikely]]
      return make_token(token_type::unknown);

    ++prev_;
    const auto tok{make_token(token_type::char_constant)};
    next();

    return tok;
  }

  /// @brief lex_identifier - Lexes an identifier.
  /// @return The lexed identifier.
  [[nodiscard]] constexpr token lex_identifier() noexcept {
    for (; isalnum(peek()); next())
      ;
    auto tok{make_token(token_type::identifier)};
    tok.lexeme_.push_back('_');
    return tok;
  }

  /// @brief lex_keyword - Checks if the identifier matches a suffix of a
  /// keyword.
  /// @param expected The suffix to check.
  /// @param type The type of the keyword.
  /// @return A keyword token if matched, result of handle_identifier()
  /// otherwise.
  [[nodiscard]] constexpr token lex_keyword(std::string_view expected,
                                            token_type type) noexcept {
    for (auto &&x : expected) {
      if (peek() != x)
        return lex_identifier();
      next();
    }
    return isalnum(peek()) ? lex_identifier() : make_token(type);
  }

  /// @brief lex_token - Lexes the next token from the input.
  /// @return The next token from the input.
  [[nodiscard]] constexpr token lex_token() noexcept {
    whitespace();

    if (end()) [[unlikely]]
      return make_token(token_type::eof);

    switch (prev_ = next_; next()) {
    default:
      return isalpha(prev())        ? lex_identifier()
             : std::isdigit(prev()) ? lex_numeric_constant()
                                    : make_token(token_type::unknown);
    case '\'':
      /// TODO: figure out character constant lexing.
      return lex_char_constant();
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
    case '{':
      return make_token(token_type::l_brace);
    case '}':
      return make_token(token_type::r_brace);
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
      switch (peek()) {
      default:
        return make_token(token_type::exclaim);
      case '=':
        next();
        return make_token(token_type::exclaimequal);
      case '[':
        next();
        return make_token(token_type::exclaiml_square);
      }
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
        return make_token(token_type::greaterequal);
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
        next();
        return make_token(token_type::pipepipe);
      case '=':
        next();
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
    case '$':
      return make_token(token_type::dollar);
    // Keywords.
    case 'a':
      return lex_keyword("uto", token_type::kw_auto);
    case 'b':
      switch (peek()) {
      default:
        return lex_keyword("ool", token_type::kw_bool);
      case 'r':
        next();
        return lex_keyword("eak", token_type::kw_break);
      }
    case 'B':
      return lex_keyword("EGIN", token_type::kw_begin);
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
      return lex_keyword("efault", token_type::kw_default);
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
    case 'E':
      return lex_keyword("ND", token_type::kw_end);
    case 'f':
      switch (peek()) {
      default:
        return lex_keyword("alse", token_type::kw_false);
      case '3':
        next();
        return lex_keyword("2", token_type::kw_f32);
      case '6':
        next();
        return lex_keyword("4", token_type::kw_f64);
      case 'o':
        next();
        return lex_keyword("r", token_type::kw_for);
      case 'n':
        next();
        return lex_keyword("", token_type::kw_fn);
      }
    case 'g':
      return lex_keyword("oto", token_type::kw_goto);
    case 'i':
      switch (peek()) {
      default:
        return lex_identifier();
      case '8':
        next();
        return lex_keyword("", token_type::kw_i8);
      case '1':
        next();
        return match('6') ? lex_keyword("", token_type::kw_i16)
                          : lex_keyword("28", token_type::kw_i128);
      case '3':
        next();
        return lex_keyword("2", token_type::kw_i32);
      case '6':
        next();
        return lex_keyword("4", token_type::kw_i64);
      case 'f':
        next();
        return lex_keyword("", token_type::kw_if);
      case 'n':
        next();
        return lex_keyword("", token_type::kw_in);
      }
    case 'p':
      return lex_keyword("rint", token_type::kw_print);
    case 'r':
      switch (peek()) {
      default:
        return lex_identifier();
      case 'e':
        next();
        return lex_keyword("turn", token_type::kw_return);
      }
    case 's':
      switch (peek()) {
      default:
        return lex_identifier();
      case 'l':
        return lex_keyword("lice", token_type::kw_slice);
      case 't':
        switch (next(); peek()) {
        case 'a':
          next();
          return lex_keyword("tic", token_type::kw_static);
        case 'r':
          next();
          return match('i') ? lex_keyword("ng", token_type::kw_string)
                            : lex_keyword("uct", token_type::kw_struct);
        }
      case 'w':
        next();
        return lex_keyword("itch", token_type::kw_switch);
      }
    case 't':
      return lex_keyword("rue", token_type::kw_true);
    case 'u':
      switch (peek()) {
      default:
        return lex_identifier();
      case '8':
        next();
        return lex_keyword("", token_type::kw_u8);
      case '1':
        next();
        return match('6') ? lex_keyword("", token_type::kw_u16)
                          : lex_keyword("28", token_type::kw_u128);
      case '3':
        next();
        return lex_keyword("2", token_type::kw_u32);
      case '6':
        next();
        return lex_keyword("4", token_type::kw_u64);
      case 'n':
        next();
        return lex_keyword("ion", token_type::kw_union);
      }
    case 'v':
      switch (peek()) {
      default:
        return lex_identifier();
      case 'o':
        next();
        return lex_keyword("id", token_type::kw_void);
      }
    }
  }

public:
  lexer(std::string_view filename) {
    std::ifstream file{filename};
    if (!file.good())
      exit(EXIT_FAILURE);

    source_ = {std::istreambuf_iterator{file},
               std::istreambuf_iterator<char>{}};
  }

  [[nodiscard]] std::vector<token> operator()() {
    std::vector<token> tokens{};

    for (;;) {
      tokens.push_back(lex_token());
      if (tokens.back().type_ == token_type::eof) [[unlikely]]
        break;
    }

    return tokens;
  }
};
} // namespace cawk