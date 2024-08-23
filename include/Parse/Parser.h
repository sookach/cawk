#pragma once

#include "AST/AST.h"
#include "Basic/Diagnostic.h"
#include "Basic/OperatorPrecedence.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"

#include <bitset>
#include <cstdlib>
#include <initializer_list>
#include <utility>

namespace cawk {
template <typename T> struct ParseResult : private std::pair<T *, bool> {
  ParseResult(bool Invalid = false) : std::pair<T *, bool>(nullptr, Invalid) {}
  ParseResult(T *Ptr) : std::pair<T *, bool>(Ptr, true) {}
  T *get() { return this->first; }
  template <typename Ty> Ty *getAs() { return static_cast<Ty *>(get()); }
  bool isValid() { return this->second; }
  ParseResult &operator=(T *RHS) {
    this->first = RHS;
    this->second = true;
    return *this;
  }
};

using DeclResult = ParseResult<Decl>;
using StmtResult = ParseResult<Stmt>;
using ExprResult = ParseResult<Expr>;

class Parser {
public:
  Lexer &Lex;
  Token Tok;
  bool HasError = false;
  bool PanicMode = false;
  std::string ExpectedTypes = "one of";
  Diagnostic &Diags;

public:
  Parser(Lexer &Lex, Diagnostic &Diags) : Lex(Lex), Diags(Diags) {
    Lex.next<false, false>(Tok);
  }

  DeclResult parse() { return parseTranslationUnit(); }

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
  bool expect(tok::TokenKind K, Ts... Ks) {
    if (!consume<NL, RE>(K)) {
      if (!std::exchange(PanicMode, true))
        Diags.addError(Tok.getLine(), diag::parse_unexpected_token,
                       tok::getTokenName(K), tok::getTokenName(Tok.getKind()));
      return false;
    }

    if constexpr (sizeof...(Ks) != 0)
      return expect<NL, RE>(Ks...);

    return true;
  }

  template <bool NL = false, bool RE = false, typename... Ts>
  void expectOneOf(tok::TokenKind K, Ts... Ks) {
    if (consume<NL, RE>(K)) {
      ExpectedTypes = "one of";
      return;
    }

    ExpectedTypes += " '";
    ExpectedTypes.append_range(tok::getTokenName(K));
    ExpectedTypes.push_back('\'');

    if constexpr (sizeof...(Ks) == 0) {
      if (!std::exchange(PanicMode, true))
        Diags.addError(Tok.getLine(), diag::parse_unexpected_token,
                       ExpectedTypes.c_str(), tok::getTokenName(Tok.getKind()));
      ExpectedTypes = "one of";
    } else {
      expectOneOf<NL, RE>(Ks...);
    }
  }

  template <tok::TokenKind... Ks> void skip() {
    std::bitset<tok::NUM_TOKENS> Filter(
        (std::bitset<tok::NUM_TOKENS>() | ... |
         std::bitset<tok::NUM_TOKENS>().set(Ks)));

    for (; Filter.test(Tok.getKind()); advance<false, false>())
      ;
  }

  DeclResult parseTranslationUnit();

  DeclResult parseDecl();
  DeclResult parseFunctionDecl();
  DeclResult parseRuleDecl();

  StmtResult parseStmt();
  StmtResult parseBreakStmt();
  StmtResult parseCompoundStmt();
  StmtResult parseDoStmt();
  StmtResult parseForStmt();
  StmtResult parseIfStmt();
  StmtResult parsePrintStmt();
  StmtResult parseReturnStmt();
  StmtResult parseSimpleStmt();
  StmtResult parseValueStmt();
  StmtResult parseWhileStmt();

  ExprResult parseExpr(prec::Level MinPrec = prec::Unknown);
};
} // namespace cawk
