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

  token peek(std::vector<token>::size_type i = 0) const noexcept {
    return tokens_[curr_ + i];
  }

  token next() noexcept { return tokens_[curr_++]; }

  bool match(token_type type) noexcept {
    if (peek().type_ != type)
      return false;
    next();
    return true;
  }

  void expect(token_type type) noexcept {
    if (!match(type))
      exit(EXIT_FAILURE);
  }

  uint8_t lbp(token_type type) const noexcept {
    switch (type) {
    default:
      return 0;
    case token_type::equal:
    case token_type::plusequal:
    case token_type::minusequal:
    case token_type::starequal:
    case token_type::slashequal:
    case token_type::percentequal:
    case token_type::lesslessequal:
    case token_type::greatergreaterequal:
    case token_type::ampequal:
    case token_type::caretequal:
    case token_type::pipeequal:
      return 1;
    case token_type::pipepipe:
      return 2;
    case token_type::ampamp:
      return 3;
    case token_type::pipe:
      return 4;
    case token_type::caret:
      return 5;
    case token_type::amp:
      return 6;
    case token_type::equalequal:
    case token_type::exclaimequal:
      return 7;
    case token_type::less:
    case token_type::lessequal:
    case token_type::greater:
    case token_type::greaterequal:
      return 8;
    case token_type::lessless:
    case token_type::greatergreater:
      return 9;
    case token_type::plus:
    case token_type::minus:
      return 10;
    case token_type::star:
    case token_type::slash:
    case token_type::percent:
      return 11;
    }
  }

  std::unique_ptr<expr> nud() {
    switch (peek().type_) {
    default:
      std::cerr << "undefined nud" << std::endl;
      exit(EXIT_FAILURE);
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
      return std::make_unique<unary_expr>(op, parse_expr());
    }
    }
  }

  std::unique_ptr<expr> parse_expr(int rbp = 0) {
    auto lhs{nud()};

    for (; lbp(peek().type_) > rbp;) {
      auto op{peek()};
      next();
      lhs = std::make_unique<binary_expr>(op, std::move(lhs),
                                          parse_expr(lbp(op.type_)));
    }

    return std::move(lhs);
  }

  std::unique_ptr<stmt> parse_var_decl() noexcept {
    const auto type{next()};
    const auto iden{next()};
    auto init{match(token_type::equal) ? parse_expr() : nullptr};
    expect(token_type::semi);

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
      return parse_var_decl();
    }
  }

public:
  parser(std::vector<token> tokens) : tokens_{tokens} {}

  std::vector<std::unique_ptr<stmt>> operator()() {
    std::vector<std::unique_ptr<stmt>> ast{};
    for (; !match(token_type::eof);)
      ast.push_back(parse_decl());
    return ast;
  }
};
} // namespace cawk