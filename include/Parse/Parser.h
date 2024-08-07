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

  TranslationUnitDecl *parse() { return parseTranslationUnit(); }

private:
  template <bool LexNewline = false, bool LexRegex = false> Token advance();
  template <bool LexNewline = false, bool LexRegex = false>
  Token peek(std::size_t) const;

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
    std::bitset<tok::NUM_TOKENS> Filter(
        (std::bitset<tok::NUM_TOKENS>() | ... |
         std::bitset<tok::NUM_TOKENS>().set(Ks)));

    for (; Filter.test(Tok.getKind()); advance<false, false>())
      ;
  }

  TranslationUnitDecl *parseTranslationUnit();

  Decl *parseDecl();
  FunctionDecl *parseFunctionDecl();
  RuleDecl *parseRuleDecl();

  Stmt *parseStmt();
  BreakStmt *parseBreakStmt();
  CompoundStmt *parseCompoundStmt();
  DoStmt *parseDoStmt();
  Stmt *parseForStmt();
  IfStmt *parseIfStmt();
  PrintStmt *parsePrintStmt();
  ReturnStmt *parseReturnStmt();
  Stmt *parseSimpleStmt();
  ValueStmt *parseValueStmt();
  WhileStmt *parseWhileStmt();

  Expr *parseExpr(prec::Level MinPrec = prec::Unknown);
};
} // namespace cawk
