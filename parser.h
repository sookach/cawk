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

/// Parser - Interface for building an AST from a stream of tokens.
class parser final {
  /// The stream of tokens.
  std::vector<token> tokens_{};

  /// Lookahead token index.
  std::vector<token>::size_type curr_{};

  /// @brief peek - Peek an arbitrary number of tokens ahead in the input
  /// stream.
  /// @param i The lookahed amount (default is 0).
  /// @return The token at tokens_[curr_ + i];
  /// TODO: should probably change this to a referene
  [[nodiscard]] __attribute__((const)) constexpr token
  peek(std::vector<token>::size_type i = 0) const noexcept {
    return tokens_[curr_ + i];
  }

  /// @brief next - Consume and return the current token.
  /// @return The current token.
  constexpr token next() noexcept { return tokens_[curr_++]; }

  /// @brief match - If the current token's type matches the expected, advance.
  /// @param type The expected token_type.
  /// @return true if current token's type matches expected, false otherwise.
  [[nodiscard]] constexpr bool match(token_type type) noexcept {
    if (peek().type_ != type)
      return false;
    next();
    return true;
  }

  /// @brief expect - Strong expectation that the current token's type matches
  /// the expected. Exits with a failure status if they do not match.
  /// @param type The expected token_type.
  constexpr void expect(token_type type) noexcept {
    if (!match(type))
      exit(EXIT_FAILURE);
  }

  /// @brief lbp - The left binding power of an operator.
  /// @param type The type of the token (a.k.a the operator type)
  /// @return an unsigned byte >= 0 representing the binding power.
  [[nodiscard]] __attribute__((const)) constexpr uint8_t
  lbp(token_type type) const noexcept {
    switch (type) {
    default:
      return 0;
    case token_type::equal:
      [[fallthrough]];
    case token_type::plusequal:
      [[fallthrough]];
    case token_type::minusequal:
      [[fallthrough]];
    case token_type::starequal:
      [[fallthrough]];
    case token_type::slashequal:
      [[fallthrough]];
    case token_type::percentequal:
      [[fallthrough]];
    case token_type::lesslessequal:
      [[fallthrough]];
    case token_type::greatergreaterequal:
      [[fallthrough]];
    case token_type::ampequal:
      [[fallthrough]];
    case token_type::caretequal:
      [[fallthrough]];
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
      [[fallthrough]];
    case token_type::exclaimequal:
      return 7;
    case token_type::less:
      [[fallthrough]];
    case token_type::lessequal:
      [[fallthrough]];
    case token_type::greater:
      [[fallthrough]];
    case token_type::greaterequal:
      return 8;
    case token_type::tilde:
      return 9;
    case token_type::lessless:
      [[fallthrough]];
    case token_type::greatergreater:
      return 10;
    case token_type::plus:
      [[fallthrough]];
    case token_type::minus:
      return 11;
    case token_type::star:
      [[fallthrough]];
    case token_type::slash:
      [[fallthrough]];
    case token_type::percent:
      return 12;
    }
  }

  /// @brief nud - Null denomination handler.
  /// @return The resulting expr ast node of the handler call.
  [[nodiscard]] std::unique_ptr<expr> nud() {
    switch (peek().type_) {
    default:
      std::cerr << "undefined nud" << std::endl;
      exit(EXIT_FAILURE);
    case token_type::semi:
      /// TODO: is this the right way to handle empty expressions?
      return nullptr;
    case token_type::identifier:
      [[fallthrough]];
    case token_type::numeric_constant:
      [[fallthrough]];
    case token_type::string_literal:
      [[fallthrough]];
    case token_type::char_constant:
      [[fallthrough]];
    case token_type::kw_true:
      [[fallthrough]];
    case token_type::kw_false:
      [[fallthrough]];
    case token_type::regex_literal:
      return std::make_unique<atom_expr>(next());
    case token_type::l_paren: {
      next();
      auto e{parse_expr()};
      expect(token_type::r_paren);
      return std::make_unique<grouping_expr>(std::move(e));
    }
    case token_type::minus: {
      auto op{next()};
      return std::make_unique<prefix_expr>(op, parse_expr(12));
    }
    case token_type::plusplus:
      [[fallthrough]];
    case token_type::minusminus: {
      auto op{next()};
      return std::make_unique<prefix_expr>(op, parse_expr(12));
    }
    case token_type::dollar:
      next();
      return std::make_unique<field_expr>(parse_expr(12));
    case token_type::exclaiml_square:
      next();
      switch (peek().type_) {
      default:
        std::cerr << "invalid cast target " << peek().lexeme_ << std::endl;
      case token_type::kw_i8:
        [[fallthrough]];
      case token_type::kw_i16:
        [[fallthrough]];
      case token_type::kw_i32:
        [[fallthrough]];
      case token_type::kw_i64:
        [[fallthrough]];
      case token_type::kw_i128:
        [[fallthrough]];
      case token_type::kw_u8:
        [[fallthrough]];
      case token_type::kw_u16:
        [[fallthrough]];
      case token_type::kw_u32:
        [[fallthrough]];
      case token_type::kw_u64:
        [[fallthrough]];
      case token_type::kw_u128:
        [[fallthrough]];
      case token_type::kw_f32:
        [[fallthrough]];
      case token_type::kw_f64:
        [[fallthrough]];
      case token_type::kw_char:
        [[fallthrough]];
      case token_type::kw_bool:
        [[fallthrough]];
      case token_type::kw_string: {
        auto type{next()};
        expect(token_type::r_square);
        return std::make_unique<cast_expr>(type, parse_expr());
      }
      }
    case token_type::kw_getline: {
      auto getline_call{std::make_unique<atom_expr>(next())};
      std::vector<std::unique_ptr<expr>> args{};

      switch (peek().type_) {
      default:
        return std::make_unique<call_expr>(std::move(getline_call));
      case token_type::identifier:
        return std::make_unique<call_expr>(std::move(getline_call),
                                           std::make_unique<atom_expr>(next()));
      case token_type::less:
        break;
      case token_type::pipe:
        getline_call = std::make_unique<atom_expr>(
            token{.lexeme_ = "getline.operator()<false>"});
        break;
      }

      next();
      if (match(token_type::l_paren)) {
        args.push_back(parse_expr());
        expect(token_type::r_paren);
      } else
        args.push_back(std::make_unique<atom_expr>(next()));

      if (match(token_type::greater)) {
        auto var{next()};
        if (var.type_ != token_type::identifier) [[unlikely]]
          exit(EXIT_FAILURE);

        args.push_back(std::make_unique<atom_expr>(var));
      }

      return std::make_unique<call_expr>(std::move(getline_call),
                                         std::move(args));
    }
    }
  }

  /// @brief parse_expr - Parse an expression using Pratt Parsing / precedence
  /// climbing.
  /// @param rbp The right binding power of the most recent operator.
  /// @return an expr ast node.
  [[nodiscard]] std::unique_ptr<expr> parse_expr(int rbp = 0) {
    auto lhs{nud()};

    switch (peek().type_) {
    default:
      break;
    case token_type::plusplus:
      [[fallthrough]];
    case token_type::minusminus: {
      auto op{next()};
      lhs = std::make_unique<postfix_expr>(op, std::move(lhs));
      break;
    }
    case token_type::l_paren: {
      next();
      std::vector<std::unique_ptr<expr>> args{};
      for (; peek().type_ != token_type::eof &&
             peek().type_ != token_type::r_paren;)
        args.push_back(parse_expr());
      expect(token_type::r_paren);
      lhs = std::make_unique<call_expr>(std::move(lhs), std::move(args));
      break;
    }
    case token_type::l_square: {
      next();
      lhs = std::make_unique<index_expr>(std::move(lhs), parse_expr());
      expect(token_type::r_square);
    }
    }

    for (; lbp(peek().type_) > rbp;) {
      auto op{peek()};
      next();
      lhs = std::make_unique<binary_expr>(op, std::move(lhs),
                                          parse_expr(lbp(op.type_)));
    }

    return std::move(lhs);
  }

  /// @brief parse_var_decl - Parse a variable declaration.
  /// @param expect_semi Whether to expect a semicolon at the end (defaults to
  /// true).
  /// @return A var_decl ast node representing the declaration.
  [[nodiscard]] std::unique_ptr<stmt>
  parse_var_decl(bool expect_semi = true) noexcept {
    const auto is_static{match(token_type::kw_static)};
    const auto type{next()};
    std::vector<token> templ{};

    switch (type.type_) {
    default:
      break;
    case token_type::kw_set:
      [[fallthrough]];
    case token_type::kw_slice:
      expect(token_type::exclaiml_square);
      templ = {next()};
      for (; match(token_type::comma);) {
        next();
        templ.push_back(next());
      }
      expect(token_type::r_square);
    }

    const auto iden{next()};
    auto init{match(token_type::equal) ? parse_expr() : nullptr};

    if (expect_semi)
      expect(token_type::semi);

    return std::make_unique<var_decl>(is_static, type, templ, iden,
                                      std::move(init));
  }

  /// @brief parse_fn_decl - Parse a function declaration (really definition
  /// since cawk does not have function declarations).
  /// @return A fn_decl ast node representing the function definition.
  [[nodiscard]] std::unique_ptr<stmt> parse_fn_decl() noexcept {
    expect(token_type::kw_fn);
    const auto iden{next().lexeme_};
    expect(token_type::l_paren);
    std::vector<std::string> params{};

    for (; peek().type_ != token_type::eof && !match(token_type::r_paren);)
      params.push_back(next().lexeme_);

    if (peek().type_ == token_type::eof)
      exit(EXIT_FAILURE);

    bool ret{match(token_type::arrow)};
    auto body{parse_block_stmt()};

    return std::make_unique<fn_decl>(iden, std::move(body), params, ret);
  }

  /// @brief parse_pattern_action - Parse a pattern action rule
  /// @return A pattern_action_decl ast node represetning the pattern action
  /// rule.
  [[nodiscard]] std::unique_ptr<stmt> parse_pattern_action() noexcept {
    std::unique_ptr<expr> pattern{};
    pattern_action_decl::type pos{pattern_action_decl::type::mid};

    switch (peek().type_) {
    default:
      pattern = parse_expr();
      break;
    case token_type::kw_begin:
      next();
      pos = pattern_action_decl::type::begin;
      break;
    case token_type::kw_end:
      next();
      pos = pattern_action_decl::type::end;
      break;
    case token_type::regex_literal:
      pattern = std::make_unique<binary_expr>(
          token{.type_ = token_type::tilde},
          std::make_unique<atom_expr>(token{.lexeme_ = "fields__.front()"}),
          std::make_unique<atom_expr>(next()));
      [[fallthrough]];
    case token_type::l_brace:;
    }

    auto action{peek().type_ == token_type::l_brace ? parse_block_stmt()
                                                    : nullptr};

    return std::make_unique<pattern_action_decl>(std::move(pattern),
                                                 std::move(action), pos);
  }

  /// @brief parse_expr_stmt - Parse an expression statement.
  /// @return An expr_stmt ast node representing the expression statement.
  [[nodiscard]] std::unique_ptr<stmt> parse_expr_stmt() noexcept {
    auto e{parse_expr()};
    expect(token_type::semi);
    return std::make_unique<expr_stmt>(std::move(e));
  }

  /// @brief parse_block_stmt - Parse a code block.
  /// @return A block_stmt ast node representing the block.
  [[nodiscard]] std::unique_ptr<stmt> parse_block_stmt() noexcept {
    std::vector<std::unique_ptr<stmt>> block{};
    expect(token_type::l_brace);
    for (; peek().type_ != token_type::eof &&
           peek().type_ != token_type::r_brace;)
      block.push_back(parse_inner_decl());
    expect(token_type::r_brace);
    return std::make_unique<block_stmt>(std::move(block));
  }

  /// @brief parse_exit_stmt - Parse an exit statement.
  /// @return An expr_stmt with a call to the exit() function.
  /// NB: I could create a seperate ast node for exit statements, but it is
  /// simple enough to just reuse code.
  [[nodiscard]] std::unique_ptr<stmt> parse_exit_stmt() noexcept {
    expect(token_type::kw_exit);
    auto exit_code{
        peek().type_ == token_type::semi
            ? std::make_unique<atom_expr>(token{.lexeme_ = "EXIT_SUCCESS"})
            : parse_expr()};
    expect(token_type::semi);
    return std::make_unique<expr_stmt>(std::make_unique<call_expr>(
        std::make_unique<atom_expr>(token{.lexeme_ = "exit"}),
        std::move(exit_code)));
  }

  /// @brief parse_if_stmt - Parse an if statement.
  /// @return An if_stmt ast node representing the if statement.
  [[nodiscard]] std::unique_ptr<stmt> parse_if_stmt() noexcept {
    expect(token_type::kw_if);
    expect(token_type::l_paren);
    auto cond{parse_expr()};
    expect(token_type::r_paren);
    auto then_branch{parse_stmt()};
    auto else_branch{match(token_type::kw_else) ? parse_stmt() : nullptr};
    return std::make_unique<if_stmt>(std::move(cond), std::move(then_branch),
                                     std::move(else_branch));
  }

  /// @brief parse_for_stmt - Parse a for loop (tradition & range-based).
  /// @return A for_stmt/ range_stmt ast node representing the for loop.
  [[nodiscard]] std::unique_ptr<stmt> parse_for_stmt() noexcept {
    expect(token_type::kw_for);
    expect(token_type::l_paren);

    if (peek(1).type_ == token_type::kw_in) {
      auto var{next()};
      next();
      auto range{parse_expr()};
      expect(token_type::r_paren);
      auto body{parse_stmt()};

      return std::make_unique<range_stmt>(std::move(var), std::move(range),
                                          std::move(body));
    }

    std::unique_ptr<stmt> init{};
    switch (peek().type_) {
    default:
      init = parse_expr_stmt();
      break;
    case token_type::semi:
      next();
      break;
    case token_type::kw_static:
      [[fallthrough]];
    case token_type::kw_auto:
      [[fallthrough]];
    case token_type::kw_i8:
      [[fallthrough]];
    case token_type::kw_i16:
      [[fallthrough]];
    case token_type::kw_i32:
      [[fallthrough]];
    case token_type::kw_i64:
      [[fallthrough]];
    case token_type::kw_i128:
      [[fallthrough]];
    case token_type::kw_u8:
      [[fallthrough]];
    case token_type::kw_u16:
      [[fallthrough]];
    case token_type::kw_u32:
      [[fallthrough]];
    case token_type::kw_u64:
      [[fallthrough]];
    case token_type::kw_u128:
      [[fallthrough]];
    case token_type::kw_f32:
      [[fallthrough]];
    case token_type::kw_f64:
      [[fallthrough]];
    case token_type::kw_char:
      [[fallthrough]];
    case token_type::kw_bool:
      [[fallthrough]];
    case token_type::kw_set:
      [[fallthrough]];
    case token_type::kw_slice:
      [[fallthrough]];
    case token_type::kw_string:
      init = parse_var_decl();
    }

    auto cond{peek().type_ == token_type::semi ? nullptr : parse_expr()};
    expect(token_type::semi);
    auto incr{peek().type_ == token_type::r_paren ? nullptr : parse_expr()};
    expect(token_type::r_paren);
    auto body{parse_stmt()};

    return std::make_unique<for_stmt>(std::move(init), std::move(cond),
                                      std::move(incr), std::move(body));
  }

  /// @brief parse_print_stmt - Parse a print statement.
  /// @return A print_stmt AST node.
  [[nodiscard]] std::unique_ptr<stmt> parse_print_stmt() noexcept {
    expect(token_type::kw_print);
    std::vector<std::unique_ptr<expr>> args{};

    for (; peek().type_ != token_type::eof && peek().type_ != token_type::semi;)
      args.push_back(
          match(token_type::comma)
              ? std::make_unique<atom_expr>(
                    token{.type_ = token_type::string_literal, .lexeme_ = " "})
              : parse_expr());

    expect(token_type::semi);

    return std::make_unique<print_stmt>(std::move(args));
  }

  /// @brief parse_return_stmt - Parse a return statement.
  /// @return A return_stmt AST node.
  [[nodiscard]] std::unique_ptr<stmt> parse_return_stmt() noexcept {
    expect(token_type::kw_return);
    auto value{parse_expr()};
    expect(token_type::semi);
    return std::make_unique<return_stmt>(std::move(value));
  }

  /// @brief parse_stmt - Parse a statement. Switches on the lookahead token to
  /// determine the subtype.
  /// @return A stmt AST node.
  [[nodiscard]] std::unique_ptr<stmt> parse_stmt() noexcept {
    switch (peek().type_) {
    default:
      return parse_expr_stmt();
    case token_type::l_brace:
      return parse_block_stmt();
    case token_type::kw_if:
      return parse_if_stmt();
    case token_type::kw_exit:
      return parse_exit_stmt();
    case token_type::kw_for:
      return parse_for_stmt();
    case token_type::kw_print:
      return parse_print_stmt();
    case token_type::kw_return:
      return parse_return_stmt();
    }
  }

  /// @brief parse_inner_decl - Parse a declaration that can only appear in a
  /// local scope.
  /// @return A stmt AST node.
  [[nodiscard]] std::unique_ptr<stmt> parse_inner_decl() noexcept {
    switch (peek().type_) {
    default:
      return parse_stmt();
    case token_type::kw_static:
      [[fallthrough]];
    case token_type::kw_auto:
      [[fallthrough]];
    case token_type::kw_i8:
      [[fallthrough]];
    case token_type::kw_i16:
      [[fallthrough]];
    case token_type::kw_i32:
      [[fallthrough]];
    case token_type::kw_i64:
      [[fallthrough]];
    case token_type::kw_i128:
      [[fallthrough]];
    case token_type::kw_u8:
      [[fallthrough]];
    case token_type::kw_u16:
      [[fallthrough]];
    case token_type::kw_u32:
      [[fallthrough]];
    case token_type::kw_u64:
      [[fallthrough]];
    case token_type::kw_u128:
      [[fallthrough]];
    case token_type::kw_f32:
      [[fallthrough]];
    case token_type::kw_f64:
      [[fallthrough]];
    case token_type::kw_char:
      [[fallthrough]];
    case token_type::kw_bool:
      [[fallthrough]];
    case token_type::kw_set:
      [[fallthrough]];
    case token_type::kw_slice:
      [[fallthrough]];
    case token_type::kw_string:
      return parse_var_decl();
    }
  }

  /// @brief parse_outer_decl - Parse a declaration that can appear in the
  /// global scope.
  /// @return A stmt AST node.
  [[nodiscard]] std::unique_ptr<stmt> parse_outer_decl() noexcept {
    switch (peek().type_) {
    default:
      return parse_pattern_action();
    case token_type::kw_auto:
      [[fallthrough]];
    case token_type::kw_i8:
      [[fallthrough]];
    case token_type::kw_i16:
      [[fallthrough]];
    case token_type::kw_i32:
      [[fallthrough]];
    case token_type::kw_i64:
      [[fallthrough]];
    case token_type::kw_i128:
      [[fallthrough]];
    case token_type::kw_u8:
      [[fallthrough]];
    case token_type::kw_u16:
      [[fallthrough]];
    case token_type::kw_u32:
      [[fallthrough]];
    case token_type::kw_u64:
      [[fallthrough]];
    case token_type::kw_u128:
      [[fallthrough]];
    case token_type::kw_f32:
      [[fallthrough]];
    case token_type::kw_f64:
      [[fallthrough]];
    case token_type::kw_char:
      [[fallthrough]];
    case token_type::kw_bool:
      [[fallthrough]];
    case token_type::kw_set:
      [[fallthrough]];
    case token_type::kw_slice:
      [[fallthrough]];
    case token_type::kw_string:
      return parse_var_decl();
    case token_type::kw_fn:
      return parse_fn_decl();
    }
  }

public:
  parser(std::vector<token> tokens) : tokens_{tokens} {}

  [[nodiscard]] std::vector<std::unique_ptr<stmt>> operator()() {
    std::vector<std::unique_ptr<stmt>> ast{};
    for (; !match(token_type::eof);)
      ast.push_back(parse_outer_decl());
    return ast;
  }
};
} // namespace cawk