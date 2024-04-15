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

  /// Indicates whether parser is in panic mode.
  bool panic_{};

  /// Indicates whether parser encountered an error.
  bool error_{};

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
    if (!match(type) && !panic_) [[unlikely]] {
      error_ = panic_ = true;
      std::cerr << "error on line " << peek().line_ << ": expected " << type
                << ", got " << peek().type_ << std::endl;
    }
  }

  /// @brief panic - Performs panic mode error recovery.
  constexpr void panic() noexcept {
    for (panic_ = false;;) {
      switch (peek().type_) {
      default:
        next();
        continue;
      case token_type::r_brace:
        [[fallthrough]];
      case token_type::semi:
        next();
        return;
      case token_type::eof:
        //     [[fallthrough]];
        //   case token_type::l_brace:
        //     [[fallthrough]];
        //   case token_type::kw_for:
        //     [[fallthrough]];
        //   case token_type::kw_begin:
        //     [[fallthrough]];
        //   case token_type::kw_end:
        //     [[fallthrough]];
        //   case token_type::kw_static:
        //     [[fallthrough]];
        //   case token_type::kw_auto:
        //     [[fallthrough]];
        //   case token_type::kw_i8:
        //     [[fallthrough]];
        //   case token_type::kw_i16:
        //     [[fallthrough]];
        //   case token_type::kw_i32:
        //     [[fallthrough]];
        //   case token_type::kw_i64:
        //     [[fallthrough]];
        //   case token_type::kw_i128:
        //     [[fallthrough]];
        //   case token_type::kw_u8:
        //     [[fallthrough]];
        //   case token_type::kw_u16:
        //     [[fallthrough]];
        //   case token_type::kw_u32:
        //     [[fallthrough]];
        //   case token_type::kw_u64:
        //     [[fallthrough]];
        //   case token_type::kw_u128:
        //     [[fallthrough]];
        //   case token_type::kw_f32:
        //     [[fallthrough]];
        //   case token_type::kw_f64:
        //     [[fallthrough]];
        //   case token_type::kw_char:
        //     [[fallthrough]];
        //   case token_type::kw_bool:
        //     [[fallthrough]];
        //   case token_type::kw_hmap:
        //     [[fallthrough]];
        //   case token_type::kw_hset:
        //     [[fallthrough]];
        //   case token_type::kw_map:
        //     [[fallthrough]];
        //   case token_type::kw_set:
        //     [[fallthrough]];
        //   case token_type::kw_slice:
        //     [[fallthrough]];
        //   case token_type::kw_string:
        return;
      }
    }
  }

  /// @brief lbp - The left binding power of an operator.
  /// @param type The type of the token (a.k.a the operator type)
  /// @return an unsigned byte >= 0 representing the binding power.
  [[nodiscard]] __attribute__((const)) static constexpr uint8_t
  lbp(token_type type) noexcept {
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
    case token_type::starstar:
      [[fallthrough]];
    case token_type::slashslash:
      return 13;
    }
  }

  /// @brief nud - Null denomination handler.
  /// @return The resulting expr ast node of the handler call.
  [[nodiscard]] constexpr std::unique_ptr<expr> nud() noexcept {
    switch (peek().type_) {
    default:
      if (!panic_) {
        std::cerr << "line " << peek().line_ << ": expected expression."
                  << std::endl;
        std::cerr << "\tnote: undefined nud for " << peek().type_ << std::endl;
        error_ = panic_ = true;
      }
      return nullptr;
    // case token_type::semi:
    /// TODO: is this the right way to handle empty expressions?
    /// Edit from the future: Nah yeah nah, consider this code
    /// var![i32] = {};
    /// It will try to parse this as a rule, panic (rightfully so), but will
    /// stay in an infinite loop since a rule never consumes semis (panic is
    /// stopped upon reaching the '}').
    //   return nullptr;
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
    case token_type::l_brace: {
      next();

      if (match(token_type::r_brace))
        return std::make_unique<init_list_expr>();

      auto init_list{std::make_unique<init_list_expr>()};
      init_list->init_list_.push_back(parse_expr());

      for (; match(token_type::comma);)
        init_list->init_list_.push_back(parse_expr());

      expect(token_type::r_brace);

      return std::move(init_list);
    }
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
  [[nodiscard]] constexpr std::unique_ptr<expr> parse_expr(int rbp = 0) {
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
      if (peek().type_ != token_type::r_paren) {
        for (args.push_back(parse_expr()); match(token_type::comma);)
          args.push_back(parse_expr());
      }
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
      auto bp{[](token_type type) constexpr noexcept -> uint8_t {
        // need to discriminate between left/right associativity.
        switch (type) {
        default:
          return lbp(type);
        case token_type::starstar:
          [[fallthrough]];
        case token_type::slashslash:
          return lbp(type) - 1;
        }
      }(op.type_)};
      next();
      lhs = std::make_unique<binary_expr>(op, std::move(lhs), parse_expr(bp));
    }

    return std::move(lhs);
  }

  [[nodiscard]] constexpr std::unique_ptr<templ> parse_template() noexcept {
    expect(token_type::exclaiml_square);
    auto t{std::make_unique<templ>()};

    auto parse_type{[this]() noexcept -> std::unique_ptr<templ_type> {
      switch (auto type{next()}; type.type_) {
      default:
        if (!panic_)
          std::cerr << "error on line " << type.line_ << ": "
                    << "invalid template argument " << type.lexeme_
                    << std::endl;
        panic_ = true;
        return nullptr;
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
      case token_type::kw_string:
        return std::make_unique<templ_type>(type);
      case token_type::kw_hset:
        [[fallthrough]];
      case token_type::kw_hmap:
        [[fallthrough]];
      case token_type::kw_map:
        [[fallthrough]];
      case token_type::kw_set:
        [[fallthrough]];
      case token_type::kw_slice:
        return std::make_unique<templ_type>(type, parse_template());
      }
    }};

    for (t->types_.push_back(parse_type()); match(token_type::comma);)
      t->types_.push_back(parse_type());

    expect(token_type::r_square);

    return std::move(t);
  }

  /// @brief parse_var_decl - Parse a variable declaration.
  /// @param expect_semi Whether to expect a semicolon at the end (defaults to
  /// true).
  /// @return A var_decl ast node representing the declaration.
  [[nodiscard]] constexpr std::unique_ptr<var_decl>
  parse_var_decl(bool expect_semi = true) noexcept {
    const auto is_static{match(token_type::kw_static)};
    const auto type{next()};

    /// TODO: need to figure out nested template parsing.
    std::unique_ptr<templ> temp{};
    switch (type.type_) {
    default:
      break;
    case token_type::kw_hmap:
      [[fallthrough]];
    case token_type::kw_hset:
      [[fallthrough]];
    case token_type::kw_map:
      [[fallthrough]];
    case token_type::kw_set:
      [[fallthrough]];
    case token_type::kw_slice:
      temp = parse_template();
    }

    const auto iden{next()};
    auto init{match(token_type::equal) ? parse_expr() : nullptr};

    if (expect_semi)
      expect(token_type::semi);

    return std::make_unique<var_decl>(is_static, type, std::move(temp), iden,
                                      std::move(init));
  }

  /// @brief parse_fn_decl - Parse a function declaration (really definition
  /// since cawk does not have function declarations).
  /// @return A fn_decl ast node representing the function definition.
  [[nodiscard]] constexpr std::unique_ptr<fn_decl> parse_fn_decl() noexcept {
    expect(token_type::kw_function);
    const auto iden{next().lexeme_};
    expect(token_type::l_paren);
    std::vector<std::pair<bool, std::string>> params{};

    if (!match(token_type::r_paren)) {
      params.emplace_back(match(token_type::amp), next().lexeme_);
      for (; match(token_type::comma);)
        params.emplace_back(match(token_type::amp), next().lexeme_);
      expect(token_type::r_paren);
    }

    if (peek().type_ == token_type::eof)
      exit(EXIT_FAILURE);

    bool ret{match(token_type::arrow)};
    auto body{parse_block_decl()};

    return std::make_unique<fn_decl>(iden, std::move(body), params, ret);
  }

  /// @brief rule_decl - Parse a rule declaration.
  /// @return A rule_decl ast node representing the rule declaration.
  /// rule.
  [[nodiscard]] constexpr std::unique_ptr<rule_decl> parse_rule() noexcept {
    std::unique_ptr<expr> pattern{};
    rule_decl::type pos{rule_decl::type::mid};

    switch (peek().type_) {
    default:
      pattern = parse_expr();
      // probably not the cleanest way to handle regexes but it suffices.
      if (dynamic_cast<atom_expr *>(pattern.get()) != nullptr &&
          dynamic_cast<atom_expr *>(pattern.get())->atom_.type_ ==
              token_type::string_literal)
        pattern = std::make_unique<binary_expr>(
            token{.type_ = token_type::tilde},
            std::make_unique<atom_expr>(token{.lexeme_ = "fields__.front()"}),
            std::move(pattern));
      break;
    case token_type::kw_begin:
      next();
      pos = rule_decl::type::begin;
      break;
    case token_type::kw_end:
      next();
      pos = rule_decl::type::end;
      [[fallthrough]];
    case token_type::l_brace:;
    }

    return std::make_unique<rule_decl>(
        std::move(pattern),
        peek().type_ == token_type::l_brace ? parse_block_decl() : nullptr,
        pos);
  }

  /// @brief parse_expr_stmt - Parse an expression statement.
  /// @return An expr_stmt ast node representing the expression statement.
  [[nodiscard]] constexpr std::unique_ptr<expr_stmt>
  parse_expr_stmt() noexcept {
    if (match(token_type::semi))
      return nullptr;
    auto e{parse_expr()};
    expect(token_type::semi);
    return std::make_unique<expr_stmt>(std::move(e));
  }

  /// @brief parse_block_decl - Parse a code block.
  /// @return A block_decl ast node representing the block.
  [[nodiscard]] std::unique_ptr<block_decl> parse_block_decl() noexcept {
    std::vector<std::unique_ptr<decl>> block{};
    expect(token_type::l_brace);
    for (; peek().type_ != token_type::eof &&
           peek().type_ != token_type::r_brace;)
      block.push_back(parse_inner_decl());
    expect(token_type::r_brace);
    return std::make_unique<block_decl>(std::move(block));
  }

  /// @brief parse_exit_stmt - Parse an exit statement.
  /// @return An expr_stmt with a call to the exit() function.
  [[nodiscard]] constexpr std::unique_ptr<stmt> parse_exit_stmt() noexcept {
    expect(token_type::kw_exit);
    auto exit_code{
        peek().type_ == token_type::semi
            ? std::make_unique<atom_expr>(token{.lexeme_ = "EXIT_SUCCESS"})
            : parse_expr()};
    expect(token_type::semi);
    return std::make_unique<exit_stmt>(std::move(exit_code));
  }

  /// @brief parse_if_stmt - Parse an if statement.
  /// @return An if_stmt ast node representing the if statement.
  [[nodiscard]] constexpr std::unique_ptr<if_stmt> parse_if_stmt() noexcept {
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
  [[nodiscard]] constexpr std::unique_ptr<stmt> parse_for_stmt() noexcept {
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

    std::unique_ptr<decl> init{};
    switch (peek().type_) {
    default:
      init = std::make_unique<decl_stmt>(parse_expr_stmt());
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
    case token_type::kw_hset:
      [[fallthrough]];
    case token_type::kw_hmap:
      [[fallthrough]];
    case token_type::kw_map:
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
  [[nodiscard]] constexpr std::unique_ptr<print_stmt>
  parse_print_stmt() noexcept {
    expect(token_type::kw_print);

    if (match(token_type::semi))
      return std::make_unique<print_stmt>();

    std::vector<std::unique_ptr<expr>> args{};
    args.push_back(parse_expr());

    for (; match(token_type::comma);)
      args.push_back(parse_expr());

    expect(token_type::semi);

    return std::make_unique<print_stmt>(std::move(args));
  }

  /// @brief parse_return_stmt - Parse a return statement.
  /// @return A return_stmt AST node.
  [[nodiscard]] constexpr std::unique_ptr<return_stmt>
  parse_return_stmt() noexcept {
    expect(token_type::kw_return);
    auto value{parse_expr()};
    expect(token_type::semi);
    return std::make_unique<return_stmt>(std::move(value));
  }

  /// @brief parse_break_stmt - Parse a break statement.
  /// @return A break_stmt AST node.
  [[nodiscard]] constexpr std::unique_ptr<break_stmt>
  parse_break_stmt() noexcept {
    expect(token_type::kw_break);
    expect(token_type::semi);
    return std::make_unique<break_stmt>();
  }

  /// @brief parse_switch_stmt - Parse a switch statement.
  /// @return A switch_stmt AST node.
  [[nodiscard]] constexpr std::unique_ptr<switch_stmt>
  parse_switch_stmt() noexcept {
    expect(token_type::kw_switch);
    expect(token_type::l_paren);
    auto e{parse_expr()};
    expect(token_type::r_paren);
    expect(token_type::l_brace);

    std::vector<std::pair<token, std::unique_ptr<block_decl>>> cases{};

    for (bool done{}; !done;) {
      switch (peek().type_) {
      default:
        done = true;
        continue;
      case token_type::kw_case:
        next();
        [[fallthrough]];
      case token_type::kw_default:
        break;
      }

      cases.push_back(
          [this]() constexpr noexcept -> decltype(cases)::value_type {
            std::vector<std::unique_ptr<decl>> v{};
            auto label{next()};
            expect(token_type::colon);
            for (;;) {
              switch (peek().type_) {
              default:
                v.push_back(parse_inner_decl());
                break;
              case token_type::kw_break:
                next();
                expect(token_type::semi);
                v.push_back(std::make_unique<decl_stmt>(
                    std::make_unique<break_stmt>()));
                break;
              case token_type::eof:
                [[fallthrough]];
              case token_type::kw_case:
                [[fallthrough]];
              case token_type::r_brace:
                return std::make_pair(
                    label, std::make_unique<block_decl>(std::move(v)));
              }
            }
          }());
    }

    expect(token_type::r_brace);

    return std::make_unique<switch_stmt>(std::move(e), std::move(cases));
  }

  /// @brief parse_stmt - Parse a statement. Switches on the lookahead token to
  /// determine the subtype.
  /// @return A stmt AST node.
  [[nodiscard]] constexpr std::unique_ptr<stmt> parse_stmt() noexcept {
    switch (peek().type_) {
    default:
      return parse_expr_stmt();
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
    case token_type::kw_break:
      return parse_break_stmt();
    case token_type::kw_switch:
      return parse_switch_stmt();
    }
  }

  /// @brief parse_stmt_block - Parses both statemetns and block declarations.
  /// @return A decl AST node.
  [[nodiscard]] constexpr std::unique_ptr<decl> parse_stmt_block() noexcept {
    switch (peek().type_) {
    default:
      return std::make_unique<decl_stmt>(parse_stmt());
    case token_type::l_brace:
      return parse_block_decl();
    }
  }

  /// @brief parse_inner_decl - Parse a declaration that can only appear
  /// in a local scope.
  /// @return A stmt AST node.
  [[nodiscard]] constexpr std::unique_ptr<decl> parse_inner_decl() noexcept {
    std::unique_ptr<decl> d{};
    switch (peek().type_) {
    default:
      d = std::make_unique<decl_stmt>(parse_stmt());
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
    case token_type::kw_hmap:
      [[fallthrough]];
    case token_type::kw_hset:
      [[fallthrough]];
    case token_type::kw_map:
      [[fallthrough]];
    case token_type::kw_set:
      [[fallthrough]];
    case token_type::kw_slice:
      [[fallthrough]];
    case token_type::kw_string:
      d = parse_var_decl();
    }

    if (panic_) [[unlikely]]
      panic();

    return std::move(d);
  }

  /// @brief parse_outer_decl - Parse a declaration that can appear in the
  /// global scope.
  /// @return A stmt AST node.
  [[nodiscard]] constexpr std::unique_ptr<decl> parse_outer_decl() noexcept {
    std::unique_ptr<decl> d{};

    switch (peek().type_) {
    default:
      d = parse_rule();
      break;
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
    case token_type::kw_hmap:
      [[fallthrough]];
    case token_type::kw_hset:
      [[fallthrough]];
    case token_type::kw_map:
      [[fallthrough]];
    case token_type::kw_set:
      [[fallthrough]];
    case token_type::kw_slice:
      [[fallthrough]];
    case token_type::kw_string:
      d = parse_var_decl();
      break;
    case token_type::kw_function:
      d = parse_fn_decl();
      break;
    }

    if (panic_) [[unlikely]] {
      panic();
    }

    return std::move(d);
  }

public:
  constexpr parser(std::vector<token> tokens) : tokens_{tokens} {}

  [[nodiscard]] constexpr std::vector<std::unique_ptr<decl>> operator()() {
    std::vector<std::unique_ptr<decl>> ast{};

    for (; !match(token_type::eof);)
      ast.push_back(parse_outer_decl());

    if (error_) [[unlikely]]
      exit(EXIT_FAILURE);

    return ast;
  }
};
} // namespace cawk