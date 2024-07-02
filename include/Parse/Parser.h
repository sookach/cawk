#pragma once

#include "AST/AST.h"
#include "Basic/OperatorPrecedence.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"

#include <bitset>
#include <cstdlib>
#include <initializer_list>

namespace cawk {
class Parser {
  Lexer &Lex;
  Token Tok;
  bool HasError;

public:
  Parser(Lexer &Lex) : Lex(Lex), HasError(false) {
    Lex.next<false, false>(Tok);
  }

  TranslationUnitDecl *parse();

private:
  template <bool = false, bool = false> Token advance();
  template <bool = false, bool = false> Token peek(std::size_t) const;

  template <bool NL = false, bool RE = false, typename... Ts>
  bool consume(tok::TokenKind K, Ts... Ks) {
    if (Tok.is(K)) {
      Lex.next<NL, RE>(Tok);
      return true;
    }

    if constexpr (sizeof...(Ks) != 0)
      return consume<NL, RE>(Ks...);

    return false;
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  bool consumeOneOf(tok::TokenKind K, Ts... Ks) {
    if (consume<NL, RE>(K))
      return true;

    if constexpr (sizeof...(Ks) != 0)
      return consume<NL, RE>(Ks...);

    return false;
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  void expect(tok::TokenKind K, Ts... Ks) {
    if (!consume<NL, RE>(K))
      exit(EXIT_FAILURE); // TODO: error handling

    if constexpr (sizeof...(Ks) != 0)
      expect<NL, RE>(Ks...);
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  void expectOneOf(tok::TokenKind K, Ts... Ks) {
    if (consume<NL, RE>(K))
      return;

    if constexpr (sizeof...(Ks) == 0)
      exit(EXIT_FAILURE); // TODO: error handling
    else
      expectOneOf<NL, RE>(Ks...);
  }

  template <tok::TokenKind... Ks> void skip() {
    std::bitset<tok::NUM_TOKENS> Filter((0 | ... | Ks));

    for (; Filter.test(Tok.getKind()); advance<false, false>())
      ;
  }

  TranslationUnitDecl *parseTranslationUnit() {
    std::vector<Decl *> Decls;
    for (; (skip<tok::newline, tok::semi>(), !Tok.is(tok::eof));) {
      Decls.push_back(parseDecl());
    }
    return TranslationUnitDecl::Create(Decls);
  }

  Decl *parseDecl() {
    if (Tok.is(tok::kw_func))
      return parseFunctionDecl();
    return parseRuleDecl();
  }

  FunctionDecl *parseFunctionDecl() {
    expect(tok::kw_function);
    auto Identifier = Tok;
    expect(tok::identifier);
    expect(tok::l_paren);
    auto Params = [this] {
      std::vector<ParamVarDecl *> Params;

      if (Tok.is(tok::identifier))
        Params.push_back(ParamVarDecl::Create(advance()));

      for (; consume(tok::comma);) {
        Params.push_back(ParamVarDecl::Create(Tok));
        expect(tok::identifier);
      }

      return Params;
    }();
    auto Body = parseCompoundStmt();
    return FunctionDecl::Create(Identifier, Params, Body);
  }

  RuleDecl *parseRuleDecl() {
    auto Pattern = [this] -> Expr * {
      switch (Tok.getKind()) {
      default:
        return parseExpr();
      case tok::kw_BEGIN:
      case tok::kw_END:
        return DeclRefExpr::Create(advance<false>());
      }
    }();

    auto Action =
        consumeOneOf(tok::semi, tok::newline) ? nullptr : parseCompoundStmt();

    return RuleDecl::Create(Pattern, Action);
  }

  CompoundStmt *parseCompoundStmt() {
    expect(tok::l_brace);
    std::vector<Stmt *> Stmts;
    for (; (skip<tok::newline, tok::semi>(), !Tok.is(tok::r_brace, tok::eof));)
      Stmts.push_back(parseStmt());

    expect(tok::r_brace);
    return CompoundStmt::Create(Stmts);
  }

  Stmt *parseStmt() {
    switch (Tok.getKind()) {
    default:
      return parseSimpleStmt();
    case tok::l_brace:
      return parseCompoundStmt();
    case tok::kw_if:
      return parseIfStmt();
    case tok::kw_for:
      return parseForStmt();
    }
  }

  Stmt *parseSimpleStmt() {
    switch (Tok.getKind()) {
    default:
      return parseValueStmt();
    case tok::kw_print:
    case tok::kw_printf:
      return parsePrintStmt();
    }
  }

  ValueStmt *parseValueStmt() {
    Expr *Value = parseExpr();
    expectOneOf(tok::semi, tok::newline);
    return ValueStmt::Create(Value);
  }

  IfStmt *parseIfStmt() {
    expect(tok::kw_if);
    expect(tok::l_paren);
    Expr *Cond = parseExpr();
    expect(tok::r_paren);
    Stmt *Then = parseStmt();
    Stmt *Else = consume(tok::kw_else) ? parseStmt() : nullptr;
    return IfStmt::Create(Cond, Then, Else);
  }

  Stmt *parseForStmt() {
    expect(tok::kw_for);
    expect(tok::l_paren);

    if (peek(1).is(tok::kw_in)) {
      DeclRefExpr *LoopVar = DeclRefExpr::Create(advance());
      expect(tok::kw_in);
      DeclRefExpr *Range = DeclRefExpr::Create(advance());
      expect(tok::l_paren);
      return ForRangeStmt::Create(LoopVar, Range, parseStmt());
    }

    Stmt *Init = Tok.is(tok::semi) ? nullptr : parseSimpleStmt();
    expect(tok::semi);
    Expr *Cond = Tok.is(tok::semi) ? nullptr : parseExpr();
    expect(tok::semi);
    Stmt *Inc = Tok.is(tok::r_paren) ? nullptr : parseSimpleStmt();
    expect(tok::r_paren);
    return ForStmt::Create(Init, Cond, Inc, parseStmt());
  }

  PrintStmt *parsePrintStmt() {
    Token Iden = Tok;
    expectOneOf<true>(tok::kw_print, tok::kw_printf);

    std::vector<Expr *> Args = [this] -> std::vector<Expr *> {
      if (consumeOneOf(tok::newline, tok::semi))
        return {};

      std::vector Args = {parseExpr()};

      for (; consume(tok::comma);)
        Args.push_back(parseExpr());

      return Args;
    }();

    auto [OpCode, Output] = [this] -> std::pair<Token, Expr *> {
      switch (Tok.getKind()) {
      default:
        return {};
      case tok::greater:
      case tok::greatergreater:
      case tok::pipe:
        break;
      }

      Token OpCode = Tok;
      expectOneOf(tok::greater, tok::greatergreater, tok::pipe);
      return {Tok, parseExpr()};
    }();

    return PrintStmt::Create(Iden, Args, OpCode, Output);
  }

  Expr *parseExpr(prec::Level MinPrec = prec::Unknown) {
    auto NUD = [this] -> Expr * {
      switch (Tok.getKind()) {
      default:
        // TODO: handle error
        return nullptr;
      case tok::identifier:
        return DeclRefExpr::Create(advance());
      case tok::numeric_constant:
        return FloatingLiteral::Create(advance());
      case tok::string_literal:
        return StringLiteral::Create(advance());
      case tok::plusplus:
      case tok::minusminus:
      case tok::exclaim:
      case tok::plus:
      case tok::minus:
      case tok::dollar: {
        auto OpCode = advance();
        return UnaryOperator::Create(OpCode, parseExpr(prec::Maximum),
                                     UnaryOperator::Prefix);
      }
      }
    };

    auto LHS = [this](Expr *LHS) -> Expr * {
      for (;;) {
        switch (Tok.getKind()) {
        default:
          return LHS;
        case tok::plusplus:
        case tok::minusminus: {
          auto OpCode = advance();
          return UnaryOperator::Create(OpCode, LHS, UnaryOperator::Prefix);
        }
        case tok::l_paren: {
          std::vector<Expr *> Args;

          if (!Tok.is(tok::r_paren))
            Args.push_back(parseExpr());

          for (; consume(tok::comma);)
            Args.push_back(parseExpr());

          expect(tok::r_paren);

          LHS = CallExpr::Create(LHS, Args);
        }
        }
      }
    }(NUD());

    for (; getBinOpPrecedence(Tok.getKind()) > MinPrec;) {
      auto OpCode = advance();
      switch (OpCode.getKind()) {
      default:
        LHS = BinaryOperator::Create(
            LHS, parseExpr(getBinOpPrecedence(OpCode.getKind())), OpCode);
        break;
      case tok::equal:
      case tok::plusequal:
      case tok::minusequal:
      case tok::starequal:
      case tok::slashequal:
      case tok::caretequal:
      case tok::starstarequal:
        LHS = BinaryOperator::Create(
            LHS,
            parseExpr(prec::Level(getBinOpPrecedence(OpCode.getKind()) - 1)),
            OpCode);
      }
    }

    return LHS;
  }
};
} // namespace cawk
