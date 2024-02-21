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
    case token_type::semi:
      /// TODO: is this the right way to handle empty expressions?
      return nullptr;
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
      return std::make_unique<unary_expr>(op, parse_expr(12));
    }
    case token_type::plusplus:
    case token_type::minusminus: {
      auto op{next()};
      return std::make_unique<unary_expr>(op, parse_expr(12));
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

  std::unique_ptr<stmt> parse_var_decl(bool expect_semi = true) noexcept {
    const auto type{next()};
    const auto iden{next()};
    auto init{match(token_type::equal) ? parse_expr() : nullptr};

    if (expect_semi)
      expect(token_type::semi);

    return std::make_unique<var_decl>(type, iden, std::move(init));
  }

  std::unique_ptr<stmt> parse_expr_stmt() noexcept {
    auto e{parse_expr()};
    expect(token_type::semi);
    return std::make_unique<expr_stmt>(std::move(e));
  }

  std::unique_ptr<stmt> parse_block_stmt() noexcept {
    auto block{std::make_unique<block_stmt>()};
    expect(token_type::l_brace);
    for (; peek().type_ != token_type::eof &&
           peek().type_ != token_type::r_brace;)
      block->body_.push_back(parse_decl());
    expect(token_type::r_brace);
    return std::move(block);
  }

  std::unique_ptr<stmt> parse_if_stmt() noexcept {
    expect(token_type::kw_if);
    expect(token_type::l_paren);
    auto cond{parse_expr()};
    expect(token_type::r_paren);
    auto then_branch{parse_stmt()};
    auto else_branch{match(token_type::kw_else) ? parse_stmt() : nullptr};
    return std::make_unique<if_stmt>(std::move(cond), std::move(then_branch),
                                     std::move(else_branch));
  }

  std::unique_ptr<stmt> parse_for_stmt() noexcept {
    expect(token_type::kw_for);
    expect(token_type::l_paren);
    auto init{peek().type_ == token_type::semi          ? nullptr
              : peek(1).type_ == token_type::identifier ? parse_var_decl(false)
                                                        : parse_stmt()};
    if (next().type_ == token_type::colon) {
      auto range{parse_expr()};
      expect(token_type::r_paren);
      auto body{parse_stmt()};
      return std::make_unique<range_stmt>(std::move(init), std::move(range),
                                          std::move(body));
    }

    auto cond{peek().type_ == token_type::semi ? nullptr : parse_expr()};
    expect(token_type::semi);
    auto incr{peek().type_ == token_type::r_paren ? nullptr : parse_expr()};
    expect(token_type::r_paren);
    auto body{parse_stmt()};

    return std::make_unique<for_stmt>(std::move(init), std::move(cond),
                                      std::move(incr), std::move(body));
  }

  std::unique_ptr<stmt> parse_stmt() noexcept {
    switch (peek().type_) {
    default:
      return parse_expr_stmt();
    case token_type::l_brace:
      return parse_block_stmt();
    case token_type::kw_if:
      return parse_if_stmt();
    case token_type::kw_for:
      return parse_for_stmt();
    }
  }

  std::unique_ptr<stmt> parse_decl() noexcept {
    switch (peek().type_) {
    default:
      return parse_stmt();
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