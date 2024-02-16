//===- parser.h - cawk language parser ------------------------------------===//
//
//  This file defines and implements the parser interface. It's a pretty simple
//  handwritten recursive descent parser with a mix of top down operator
//  precedence (Pratt) parsing and precedence climbing for handling expressions.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "ast.h"
#include "token.h"

#include <vector>

namespace cawk {
class parser final {
  std::vector<token> tokens_{};
  std::vector<token>::size_type curr_{};

  token peek(int i = 0) const noexcept {
    return curr_ + i < std::size(tokens_) ? tokens_[curr_ + 1] : token{};
  }

  token next() noexcept { return tokens_[curr_++]; }

  bool match(token_type type) noexcept {
    if (peek().type_ != type)
      return false;
    next();
    return true;
  }

  std::unique_ptr<expr> nud() {
    switch (peek().type_) {
    case token_type::identifier:
    case token_type::numeric_constant:
    case token_type::string_literal:
      return std::make_unique<atom_expr>(next());
    case token_type::l_paren: {
      next();
      auto e{parse_expr()};
      match(token_type::r_paren);
      return std::move(e);
    }
    case token_type::minus: {
      auto op{next()};
      return unary_expr{};
    }
    }
  }

  std::unique_ptr<expr> parse_expr(int rbp = 0) {}

  std::unique_ptr<stmt> parse_var_decl() noexcept {
    const auto type{next()};
    const auto iden{next()};
    const auto init{match(token_type::equal) ? parse_expr() : nullptr};

    return std::make_unique<var_decl>(type, iden, std::move(init));
  }

  std::unique_ptr<stmt> parse_decl() noexcept {
    switch (peek().type_) {
    default:
    //
    case token_type::kw_char:
    case token_type::kw_i8:
    case token_type::kw_i16:
    case token_type::kw_i32:
    case token_type::kw_i64:
    case token_type::kw_u8:
    case token_type::kw_u16:
    case token_type::kw_u32:
    case token_type::kw_u64:
    case token_type::kw_f32:
    case token_type::kw_f64:
      parse_var_decl();
    }
  }

public:
  parser(std::vector<token> tokens) : tokens_{tokens} {}

  std::vector<std::unique_ptr<stmt>> operator()() {
    std::vector<std::unique_ptr<stmt>> ast{};
    for (; !match(token_type::eof);)
      ast.push_back(parse_decl());
  }
};
} // namespace cawk