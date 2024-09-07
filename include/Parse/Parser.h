//===--- Parser.h - CAWK Language Parser ------------------------*- C++ -*-===//
//
//  This file defines the Parser interface.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "AST/AST.h"
#include "Basic/Diagnostic.h"
#include "Basic/OperatorPrecedence.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"
#include <Sema/Sema.h>

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

  /// @brief Diags - The diagnostic engine.
  Diagnostic &Diags;

public:
  Parser(Lexer &Lex, Diagnostic &Diags)
      : Lex(Lex), Actions(Diags), Diags(Diags) {
    Lex.next<false, false>(Tok);
  }

  DeclResult parse() { return parseTranslationUnit(); }

  auto getSymbols() { return Actions.getSymbols(); }

private:
  template <bool NL = true, bool RE = false> Token advance();
  template <bool NL = true, bool RE = false> Token peek(std::size_t) const;

  template <bool NL = true, bool RE = false, typename... Ts>
  bool consume(tok::TokenKind K, Ts... Ks) {
    if (Tok.is(K)) {
      Lex.next<NL, RE>(Tok);
      return true;
    }

    if constexpr (sizeof...(Ks) != 0)
      return consume<NL, RE>(Ks...);

    return false;
  }

  template <bool NL = true, bool RE = false, typename... Ts>
  bool consumeOneOf(tok::TokenKind K, Ts... Ks) {
    if (consume<NL, RE>(K))
      return true;

    if constexpr (sizeof...(Ks) != 0)
      return consume<NL, RE>(Ks...);

    return false;
  }

  template <bool NL = true, bool RE = false, typename... Ts>
  bool expect(tok::TokenKind K, Ts... Ks) {
    if (!consume<NL, RE>(K)) {
      if (!std::exchange(PanicMode, true))
        Diags.addError(SourceRange(std::cbegin(Tok.getRawData()),
                                   std::cend(Tok.getRawData())),
                       diag::parse_unexpected_token, tok::getTokenName(K),
                       tok::getTokenName(Tok.getKind()));
      return false;
    }

    if constexpr (sizeof...(Ks) != 0)
      return expect<NL, RE>(Ks...);

    return true;
  }

  template <bool NL = true, bool RE = false, typename... Ts>
  bool expectOneOf(tok::TokenKind K, Ts... Ks) {
    if (consume<NL, RE>(K)) {
      ExpectedTypes = "one of";
      return true;
    }

    ExpectedTypes += " '";
    ExpectedTypes.append_range(tok::getTokenName(K));
    ExpectedTypes.push_back('\'');

    if constexpr (sizeof...(Ks) == 0) {
      if (!std::exchange(PanicMode, true))
        Diags.addError(SourceRange(std::cbegin(Tok.getRawData()),
                                   std::cend(Tok.getRawData())),
                       diag::parse_unexpected_token, ExpectedTypes.c_str(),
                       tok::getTokenName(Tok.getKind()));
      ExpectedTypes = "one of";
      return false;
    } else {
      return expectOneOf<NL, RE>(Ks...);
    }
  }

  void skip(auto... Kinds) {
    std::bitset<tok::NUM_TOKENS> Filter(
        (std::bitset<tok::NUM_TOKENS>() | ... |
         std::bitset<tok::NUM_TOKENS>().set(Kinds)));

    for (; Filter.test(Tok.getKind()); advance<false, false>())
      ;
  }

  void skipUntil(auto... Kinds) {
    std::bitset<tok::NUM_TOKENS> Filter(
        (std::bitset<tok::NUM_TOKENS>() | ... |
         std::bitset<tok::NUM_TOKENS>().set(Kinds)));

    for (; !Filter.test(Tok.getKind()); advance<false, false>())
      ;
  }

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
  StmtResult parsePrintStatement();
  StmtResult parseReturnStatement();
  StmtResult parseSimpleStatement();
  StmtResult parseValueStatement();
  StmtResult parseWhileStatement();

  template <bool CommaOp = false>
  ExprResult parseExpression(prec::Level MinPrec = prec::Unknown);

  void recover() { skipUntil(tok::newline, tok::semi, tok::eof); }
};
} // namespace cawk
