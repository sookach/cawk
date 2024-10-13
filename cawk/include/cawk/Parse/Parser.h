//===--- Parser.h - CAWK Language Parser ------------------------*- C++ -*-===//
//
//  This file defines the Parser interface.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "cawk/AST/AST.h"
#include "cawk/Basic/Diagnostic.h"
#include "cawk/Basic/OperatorPrecedence.h"
#include "cawk/Basic/TokenKinds.h"
#include "cawk/Lexer/Lexer.h"
#include "cawk/Sema/Sema.h"

#include <bitset>
#include <cstdlib>
#include <initializer_list>
#include <utility>

namespace cawk {

/// @brief The Parser class is responsible for parsing the input source code.
class Parser {
public:
  /// @brief Lex - The lexer instance.
  Lexer &Lex;

  /// @brief Actions - The semantic analysis instance.
  Sema Actions;

  /// @brief Tok - The current lookahead token.
  Token Tok;

  /// @brief Indicates whether an error occurred during parsing.
  bool HasError = false;

  /// @brief Indicates whether the parser is in panic mode.
  bool PanicMode = false;
  std::string ExpectedTypes = "one of";

  /// @brief ParenCount - The nested level of parentheses.
  int ParenCount = 0;

  /// @brief Diags - The diagnostic engine.
  Diagnostic &Diags;

public:
  Parser(Lexer &Lex, Diagnostic &Diags)
      : Lex(Lex), Actions(Diags), Diags(Diags) {
    Lex.next(Tok);
  }

  DeclResult parse() { return parseTranslationUnit(); }

  auto getFunctions() { return Actions.getFunctions(); }

private:
  Token advance();
  Token peek(std::size_t) const;

  template <typename... Ts> bool consume(tok::TokenKind K, Ts... Ks) {
    if (!Tok.is(K))
      return false;

    advance();

    if constexpr (sizeof...(Ks) != 0)
      return consume(Ks...);

    return true;
  }

  template <typename... Ts> bool consumeOneOf(tok::TokenKind K, Ts... Ks) {
    if (consume(K))
      return true;

    if constexpr (sizeof...(Ks) != 0)
      return consume(Ks...);

    return false;
  }

  template <typename... Ts> bool expect(tok::TokenKind K, Ts... Ks) {
    if (!consume(K)) {
      if (!std::exchange(PanicMode, true))
        Diags.addError(SourceRange(Tok.getRawData()),
                       diag::parse_unexpected_token, tok::getTokenName(K),
                       tok::getTokenName(Tok.getKind()));
      return false;
    }

    if constexpr (sizeof...(Ks) != 0)
      return expect(Ks...);

    return true;
  }

  template <typename... Ts> bool expectOneOf(tok::TokenKind K, Ts... Ks) {
    if (consume(K)) {
      ExpectedTypes = "one of";
      return true;
    }

    ExpectedTypes += " '";
    ExpectedTypes.append_range(tok::getTokenName(K));
    ExpectedTypes.push_back('\'');

    if constexpr (sizeof...(Ks) == 0) {
      if (!std::exchange(PanicMode, true))
        Diags.addError(SourceRange(Tok.getRawData()),
                       diag::parse_unexpected_token, ExpectedTypes.c_str(),
                       tok::getTokenName(Tok.getKind()));
      ExpectedTypes = "one of";
      return false;
    } else {
      return expectOneOf(Ks...);
    }
  }

  void skip(auto... Kinds) {
    std::bitset<tok::NUM_TOKENS> Filter(
        (std::bitset<tok::NUM_TOKENS>() | ... |
         std::bitset<tok::NUM_TOKENS>().set(Kinds)));

    for (; Filter.test(Tok.getKind()); advance())
      ;
  }

  void skipUntil(auto... Kinds) {
    std::bitset<tok::NUM_TOKENS> Filter(
        (std::bitset<tok::NUM_TOKENS>() | ... |
         std::bitset<tok::NUM_TOKENS>().set(Kinds)));

    for (; !Filter.test(Tok.getKind()); advance())
      ;
  }

  bool consumeTerminator() { return consumeOneOf(tok::newline, tok::semi); }

  DeclResult parseTranslationUnit();

  DeclResult parseDeclaration();
  DeclResult parseFunctionDeclaration();
  DeclResult parseRuleDeclaration();

  StmtResult parseStatement();
  StmtResult parseBreakStatement();
  StmtResult parseCompoundStatement();
  StmtResult parseDoStatement();
  StmtResult parseForStatement();
  StmtResult parseIfStatement();
  StmtResult parseNullStatement();
  StmtResult parsePrintStatement();
  StmtResult parseReturnStatement();
  StmtResult parseSimpleStatement();
  StmtResult parseValueStatement();
  StmtResult parseWhileStatement();

  ExprResult parseExpression(prec::Level MinPrec = prec::Assignment);

  std::pair<std::vector<VarDecl *>, bool> parseParameterDeclarationClause();

  void recover() { skipUntil(tok::newline, tok::semi, tok::eof); }
};
} // namespace cawk
