#pragma once

#include "AST/AST.h"
#include "Basic/OperatorPrecedence.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"

#include <algorithm>
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
    Lex.Next<false, false>(Tok);
  }

  TranslationUnitDecl *Parse();

private:
  template <bool = false, bool = false> Token Advance();
  template <bool = false, bool = false> Token Peek(std::size_t) const;

  template <bool NL = false, bool RE = false, typename... Ts>
  bool Consume(tok::TokenKind K, Ts... Ks) {
    if (Tok.is(K)) {
      Lex.Next<NL, RE>(Tok);
      return true;
    }

    if constexpr (sizeof...(Ks) != 0)
      return Consume<NL, RE>(Ks...);

    return false;
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  bool ConsumeOneOf(tok::TokenKind K, Ts... Ks) {
    if (Consume<NL, RE>(K))
      return true;

    if constexpr (sizeof...(Ks) != 0)
      return Consume<NL, RE>(Ks...);

    return false;
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  void Expect(tok::TokenKind K, Ts... Ks) {
    if (!Consume<NL, RE>(K))
      exit(EXIT_FAILURE); // TODO: error handling

    if constexpr (sizeof...(Ks) != 0)
      Expect<NL, RE>(Ks...);
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  void ExpectOneOf(tok::TokenKind K, Ts... Ks) {
    if (Consume<NL, RE>(K))
      return;

    if constexpr (sizeof...(Ks) == 0)
      exit(EXIT_FAILURE); // TODO: error handling
    else
      ExpectOneOf<NL, RE>(Ks...);
  }

  template <tok::TokenKind... Ks> void Skip() {
    std::bitset<tok::NUM_TOKENS> Filter((0 | ... | Ks));

    for (; Filter.test(Tok.getKind()); Advance<false, false>())
      ;
  }

  TranslationUnitDecl *ParseTranslationUnit() {
    std::vector<Decl *> Decls;
    for (; (Skip<tok::newline, tok::semi>(), !Tok.is(tok::eof));) {
      Decls.push_back(ParseDecl());
    }
    return TranslationUnitDecl::Create(Decls);
  }

  Decl *ParseDecl() {
    if (Tok.is(tok::kw_func))
      return ParseFunctionDecl();
    return ParseRuleDecl();
  }

  FunctionDecl *ParseFunctionDecl() {
    Expect(tok::kw_function);
    auto Identifier = Tok;
    Expect(tok::identifier);
    Expect(tok::l_paren);
    auto Params = [this] {
      std::vector<ParamVarDecl *> Params;

      if (Tok.is(tok::identifier))
        Params.push_back(ParamVarDecl::Create(Advance()));

      for (; Consume(tok::comma);) {
        Params.push_back(ParamVarDecl::Create(Tok));
        Expect(tok::identifier);
      }

      return Params;
    }();
    auto Body = ParseCompoundStmt();
    return FunctionDecl::Create(Identifier, Params, Body);
  }

  RuleDecl *ParseRuleDecl() {
    auto Pattern = [this] -> Expr * {
      switch (Tok.getKind()) {
      default:
        return ParseExpr();
      case tok::kw_BEGIN:
      case tok::kw_END:
        return DeclRefExpr::Create(Advance<false>());
      }
    }();

    auto Action =
        ConsumeOneOf(tok::semi, tok::newline) ? nullptr : ParseCompoundStmt();

    return RuleDecl::Create(Pattern, Action);
  }

  CompoundStmt *ParseCompoundStmt() {
    Expect(tok::l_brace);
    std::vector<Stmt *> Stmts;
    for (; (Skip<tok::newline, tok::semi>(), !Tok.is(tok::r_brace, tok::eof));)
      Stmts.push_back(ParseStmt());

    Expect(tok::r_brace);
    return CompoundStmt::Create(Stmts);
  }

  Stmt *ParseStmt() {
    switch (Tok.getKind()) {
    default:
      return ParseSimpleStmt();
    case tok::l_brace:
      return ParseCompoundStmt();
    case tok::kw_if:
      return ParseIfStmt();
    case tok::kw_for:
      return ParseForStmt();
    }
  }

  Stmt *ParseSimpleStmt() {
    switch (Tok.getKind()) {
    default:
      return ParseValueStmt();
    case tok::kw_print:
    case tok::kw_printf:
      return ParsePrintStmt();
    }
  }

  ValueStmt *ParseValueStmt() {
    Expr *Value = ParseExpr();
    ExpectOneOf(tok::semi, tok::newline);
    return ValueStmt::Create(Value);
  }

  IfStmt *ParseIfStmt() {
    Expect(tok::kw_if);
    Expect(tok::l_paren);
    Expr *Cond = ParseExpr();
    Expect(tok::r_paren);
    Stmt *Then = ParseStmt();
    Stmt *Else = Consume(tok::kw_else) ? ParseStmt() : nullptr;
    return IfStmt::Create(Cond, Then, Else);
  }

  Stmt *ParseForStmt() {
    Expect(tok::kw_for);
    Expect(tok::l_paren);

    if (Peek(1).is(tok::kw_in)) {
      DeclRefExpr *LoopVar = DeclRefExpr::Create(Advance());
      Expect(tok::kw_in);
      DeclRefExpr *Range = DeclRefExpr::Create(Advance());
      Expect(tok::l_paren);
      return ForRangeStmt::Create(LoopVar, Range, ParseStmt());
    }

    Stmt *Init = Tok.is(tok::semi) ? nullptr : ParseSimpleStmt();
    Expect(tok::semi);
    Expr *Cond = Tok.is(tok::semi) ? nullptr : ParseExpr();
    Expect(tok::semi);
    Stmt *Inc = Tok.is(tok::r_paren) ? nullptr : ParseSimpleStmt();
    Expect(tok::r_paren);
    return ForStmt::Create(Init, Cond, Inc, ParseStmt());
  }

  PrintStmt *ParsePrintStmt() {
    Token Iden = Tok;
    ExpectOneOf<true>(tok::kw_print, tok::kw_printf);

    std::vector<Expr *> Args = [this] -> std::vector<Expr *> {
      if (ConsumeOneOf(tok::newline, tok::semi))
        return {};

      std::vector Args = {ParseExpr()};

      for (; Consume(tok::comma);)
        Args.push_back(ParseExpr());

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
      ExpectOneOf(tok::greater, tok::greatergreater, tok::pipe);
      return {Tok, ParseExpr()};
    }();

    return PrintStmt::Create(Iden, Args, OpCode, Output);
  }

  Expr *ParseExpr(prec::Level MinPrec = prec::Unknown) {
    auto NUD = [this] -> Expr * {
      switch (Tok.getKind()) {
      default:
        // TODO: handle error
        return nullptr;
      case tok::identifier:
        return DeclRefExpr::Create(Advance());
      case tok::numeric_constant:
        return FloatingLiteral::Create(Advance());
      case tok::string_literal:
        return StringLiteral::Create(Advance());
      case tok::plusplus:
      case tok::minusminus:
      case tok::exclaim:
      case tok::plus:
      case tok::minus:
      case tok::dollar: {
        auto OpCode = Advance();
        return UnaryOperator::Create(OpCode, ParseExpr(prec::Maximum),
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
          auto OpCode = Advance();
          return UnaryOperator::Create(OpCode, LHS, UnaryOperator::Prefix);
        }
        case tok::l_paren: {
          std::vector<Expr *> Args;

          if (!Tok.is(tok::r_paren))
            Args.push_back(ParseExpr());

          for (; Consume(tok::comma);)
            Args.push_back(ParseExpr());

          Expect(tok::r_paren);

          LHS = CallExpr::Create(LHS, Args);
        }
        }
      }
    }(NUD());

    for (; getBinOpPrecedence(Tok.getKind()) > MinPrec;) {
      auto OpCode = Advance();
      switch (OpCode.getKind()) {
      default:
        LHS = BinaryOperator::Create(
            LHS, ParseExpr(getBinOpPrecedence(OpCode.getKind())), OpCode);
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
            ParseExpr(prec::Level(getBinOpPrecedence(OpCode.getKind()) - 1)),
            OpCode);
      }
    }

    return LHS;
  }
};
} // namespace cawk
