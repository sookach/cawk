//===--- Parser.h - CAWK Language Parser ------------------------*- C++ -*-===//
//
//  This file implements the Parser interface.
//
//===----------------------------------------------------------------------===//
#include "Parse/Parser.h"

using namespace cawk;

/// \brief Consumes the current lookahead token and advances to the next one.
/// \tparam LexNewline - should the lexer lex newlines?
/// \tparam LexRegex - are we attempting to lex a regex?
/// \return The current lookahead token.
template <bool LexNewline, bool LexRegex> Token Parser::advance() {
  auto Prev = Tok;
  Lex.next<LexNewline, LexRegex>(Tok);
  return Prev;
}

template Token Parser::advance<false, false>();
template Token Parser::advance<false, true>();
template Token Parser::advance<true, false>();
template Token Parser::advance<true, true>();

/// \brief Peeks N tokens ahead.
/// \tparam LexNewLine - should the lexer lex newlines?
/// \tparam LexRegex - are we attempting to lex a regex?
/// \param N - the number of tokens to peek.
/// \return The Nth token ahead.
template <bool LexNewLine, bool LexRegex>
Token Parser::peek(std::size_t N) const {
  Token T;
  auto BufferPtr = Lex.getBufferPtr();

  for (; N != 0; --N)
    Lex.next<LexNewLine, LexRegex>(T);

  Lex.setBufferPtr(BufferPtr);
  return T;
}

template Token Parser::peek<false, false>(std::size_t N) const;
template Token Parser::peek<false, true>(std::size_t N) const;
template Token Parser::peek<true, false>(std::size_t N) const;
template Token Parser::peek<true, true>(std::size_t N) const;

/// \brief Parses a translation unit.
/// \return The parsed translation unit.
DeclResult Parser::parseTranslationUnit() {
  std::vector<Decl *> Decls;
  for (; (skip<tok::newline, tok::semi>(), !consume(tok::eof));) {
    DeclResult Res = parseDecl();
    if (!HasError)
      Decls.push_back(Res.get());
  }
  if (!HasError)
    return TranslationUnitDecl::Create(Decls);
  return false;
}

/// \brief Parses a rule declaration or function definition.
DeclResult Parser::parseDecl() {
  if (Tok.is(tok::kw_function))
    return parseFunctionDecl();
  return parseRuleDecl();
}

/// \brief Parses a function declaration.
DeclResult Parser::parseFunctionDecl() {
  // This test should never be fail, but it's here as a sanity check.
  if (!expect(tok::kw_function))
    return false;
  auto Identifier = Tok;
  if (!expect(tok::identifier, tok::l_paren)) {
    skipUntil<tok::l_brace, tok::r_brace, tok::eof>();
    if (Tok.is(tok::eof))
      return false;
    PanicMode = false;
    parseCompoundStmt();
    return false;
  }
  auto [Valid, Params] = [this] {
    std::vector<ParamVarDecl *> Params;

    if (Tok.is(tok::identifier))
      Params.push_back(ParamVarDecl::Create(advance()));

    for (; consume(tok::comma);) {
      Params.push_back(ParamVarDecl::Create(Tok));
      if (!expect(tok::identifier))
        return pair(false, Params);
    }

    return std::pair(true, Params);
  }();
  if (!Valid)
    return false;
  if (!consume(tok::r_paren))
    return false;
  auto Body = parseCompoundStmt();
  if (Body.isValid())
    return FunctionDecl::Create(Identifier, Params, Body.getAs<CompoundStmt>());
  return false;
}

DeclResult Parser::parseRuleDecl() {
  auto Pattern = [this] -> ExprResult {
    switch (Tok.getKind()) {
    default:
      return parseExpr();
    case tok::kw_BEGIN:
    case tok::kw_END:
      return DeclRefExpr::Create(advance<false>());
    }
  }();

  if (!Pattern.isValid())
    return false;

  auto Action =
      consumeOneOf(tok::semi, tok::newline) ? nullptr : parseCompoundStmt();

  if (!Action.isValid())
    return false;

  return RuleDecl::Create(Pattern.get(), Action.getAs<CompoundStmt>());
}

StmtResult Parser::parseStmt() {
  switch (Tok.getKind()) {
  default: {
    auto S = parseSimpleStmt();
    if (!S.isValid())
      return false;
    skip<tok::semi, tok::newline>();
    return S;
  }
  case tok::l_brace:
    return parseCompoundStmt();
  case tok::kw_break:
    return parseBreakStmt();
  case tok::kw_do:
    return parseDoStmt();
  case tok::kw_for:
    return parseForStmt();
  case tok::kw_if:
    return parseIfStmt();
  case tok::kw_return:
    return parseReturnStmt();
  case tok::kw_while:
    return parseWhileStmt();
  }
}

StmtResult Parser::parseBreakStmt() {
  if (!consume(tok::kw_break))
    return false;
  return BreakStmt::Create();
}

StmtResult Parser::parseCompoundStmt() {
  if (!consume(tok::l_brace))
    return false;

  std::vector<Stmt *> Stmts;
  for (; (skip<tok::newline, tok::semi>(), !Tok.is(tok::r_brace, tok::eof));) {
    StmtResult S = parseStmt();
    if (!S.isValid()) {
      recover();
      return false;
    }
  }

  if (!consume(tok::r_brace))
    return false;

  return CompoundStmt::Create(Stmts);
}

StmtResult Parser::parseDoStmt() {
  if (!consume(tok::kw_do))
    return false;

  auto Body = parseCompoundStmt();
  if (!Body.isValid())
    return false;

  if (!consume(tok::kw_while, tok::l_paren))
    return false;

  auto Cond = parseExpr();
  if (!Cond.isValid())
    return false;

  if (!consume(tok::r_paren))
    return false;

  return DoStmt::Create(Cond.get(), Body.get());
}

StmtResult Parser::parseForStmt() {
  if (!consume(tok::kw_for, tok::l_paren))
    return false;

  if (peek(1).is(tok::kw_in)) {
    if (!consume(tok::identifier))
      return false;
    DeclRefExpr *LoopVar = DeclRefExpr::Create(advance());
    if (!consume(tok::kw_in, tok::identifier))
      return false;
    DeclRefExpr *Range = DeclRefExpr::Create(advance());
    if (!consume(tok::r_paren))
      return false;
    StmtResult Body = parseStmt();
    if (!Body.isValid())
      return false;
    return ForRangeStmt::Create(LoopVar, Range, Body.get());
  }

  StmtResult Init = Tok.is(tok::semi) ? true : parseSimpleStmt();
  if (!Init.isValid())
    return false;
  if (!consume(tok::semi))
    return false;
  ExprResult Cond = Tok.is(tok::semi) ? true : parseExpr();
  if (!Cond.isValid())
    return false;
  if (!consume(tok::semi))
    return false;
  StmtResult Inc = Tok.is(tok::r_paren) ? true : parseSimpleStmt();
  if (!Inc.isValid())
    return false;
  if (!consume(tok::r_paren))
    return false;
  StmtResult Body = parseStmt();
  if (!Body.isValid())
    return false;
  return ForStmt::Create(Init.get(), Cond.get(), Inc.get(), Body.get());
}

StmtResult Parser::parseIfStmt() {
  if (!consume(tok::kw_if, tok::l_paren))
    return false;
  ExprResult Cond = parseExpr();
  if (!Cond.isValid())
    return false;
  if (!consume(tok::r_paren))
    return false;
  StmtResult Then = parseStmt();
  if (!Then.isValid())
    return false;
  StmtResult Else = consume(tok::kw_else) ? parseStmt() : true;
  if (!Else.isValid())
    return false;
  return IfStmt::Create(Cond.get(), Then.get(), Else.get());
}

StmtResult Parser::parsePrintStmt() {
  Token Iden = Tok;
  if (!consumeOneOf<true>(tok::kw_print, tok::kw_printf))
    return false;

  auto [Args, Valid] = [this] -> std::pair<std::vector<Expr *>, bool> {
    if (consumeOneOf(tok::newline, tok::semi))
      return {{}, true};

    ExprResult Arg = parseExpr();
    if (!Arg.isValid())
      return {{}, false};
    std::vector Args = {Arg.get()};

    for (; consume(tok::comma);) {
      Arg = parseExpr();
      if (!Arg.isValid())
        return {{}, false};
      Args.push_back(Arg.get());
    }

    return {Args, true};
  }();

  if (!Valid)
    return false;

  auto [OpCode, Output] = [this] -> std::pair<Token, ExprResult> {
    switch (Tok.getKind()) {
    default:
      return {};
    case tok::greater:
    case tok::greatergreater:
    case tok::pipe:
      break;
    }

    Token OpCode = Tok;
    if (!consumeOneOf(tok::greater, tok::greatergreater, tok::pipe))
      return {{}, false};
    return {Tok, parseExpr()};
  }();

  return PrintStmt::Create(Iden, Args, OpCode, Output.get());
}

StmtResult Parser::parseReturnStmt() {
  if (!consume(tok::kw_return))
    return false;
  ExprResult E = parseExpr();
  if (!E.isValid())
    return false;
  return ReturnStmt::Create(E.get());
}

StmtResult Parser::parseSimpleStmt() {
  switch (Tok.getKind()) {
  default:
    return parseValueStmt();
  case tok::kw_print:
  case tok::kw_printf:
    return parsePrintStmt();
  }
}

StmtResult Parser::parseValueStmt() {
  ExprResult Value = parseExpr();
  if (!Value.isValid())
    return false;
  return ValueStmt::Create(Value.get());
}

StmtResult Parser::parseWhileStmt() {
  if (!consume(tok::kw_while, tok::l_paren))
    return false;

  ExprResult Cond = parseExpr();
  if (!Cond.isValid())
    return false;

  if (!consume(tok::r_paren))
    return false;

  StmtResult Body = parseStmt();
  if (!Body.isValid())
    return false;

  return WhileStmt::Create(Cond.get(), Body.get());
}

ExprResult Parser::parseExpr(prec::Level MinPrec) {
  auto NUD = [this] -> ExprResult {
    switch (Tok.getKind()) {
    default:
      // TODO: handle error
      return false;
    case tok::kw_gsub:
    case tok::kw_index:
    case tok::kw_match:
    case tok::kw_split:
    case tok::kw_sprintf:
    case tok::kw_sub:
    case tok::kw_substr:
    case tok::identifier:
      return DeclRefExpr::Create(advance<true>());
    case tok::numeric_constant:
      return FloatingLiteral::Create(advance<true>());
    case tok::string_literal:
      return StringLiteral::Create(advance<true>());
    case tok::plusplus:
    case tok::minusminus:
    case tok::exclaim:
    case tok::plus:
    case tok::minus:
    case tok::dollar: {
      Token OpCode = advance();
      ExprResult SubExpr = parseExpr(prec::Maximum);
      if (!SubExpr.isValid())
        return false;
      return UnaryOperator::Create(OpCode, SubExpr.get(),
                                   UnaryOperator::Prefix);
    }
    }
  };

  auto LHS = [this](ExprResult LHS) -> ExprResult {
    if (!LHS.isValid())
      return false;

    for (;;) {
      switch (Tok.getKind()) {
      default:
        return LHS;
      case tok::plusplus:
      case tok::minusminus: {
        auto OpCode = advance();
        return UnaryOperator::Create(OpCode, LHS.get(), UnaryOperator::Prefix);
      }
      case tok::l_paren: {
        expect(tok::l_paren);
        std::vector<Expr *> Args;

        if (!Tok.is(tok::r_paren)) {
          ExprResult Arg = parseExpr();
          if (!Arg.isValid())
            return false;
          Args.push_back(Arg.get());
        }

        for (ExprResult Arg; consume(tok::comma);) {
          Arg = parseExpr();
          if (!Arg.isValid())
            return false;
          Args.push_back(Arg.get());
        }

        if (!consume(tok::r_paren))
          return false;

        LHS = CallExpr::Create(LHS.get(), Args);
        break;
      }
      case tok::l_square: {
        expect(tok::l_square);

        ExprResult Arg = parseExpr();
        if (!Arg.isValid())
          return false;
        std::vector Args = {Arg.get()};

        for (; consume(tok::comma);) {
          Arg = parseExpr();
          if (!Arg.isValid())
            return false;
          Args.push_back(Arg.get());
        }

        if (!consume(tok::r_square))
          return false;

        LHS = ArraySubscriptExpr::Create(LHS.get(), Args);
      }
      }
    }
  }(NUD());

  for (;;) {
    auto Prec = [this] {
      if (!Tok.is(tok::newline))
        return getBinOpPrecedence(Tok.getKind());
      auto Peek = peek(1);
      if (Peek.is(tok::identifier, tok::string_literal, tok::numeric_constant))
        return prec::Unknown;
      return getBinOpPrecedence(Peek.getKind());
    }();

    if (Prec <= MinPrec)
      break;

    skip<tok::newline>();

    auto OpCode = [this, Prec] {
      if (Prec == prec::StringConcat) {
        auto SpaceTok = Tok;
        Lex.formSpaceToken(SpaceTok,
                           std::cbegin(SpaceTok.getLiteralData()) - 1);
        return SpaceTok;
      }
      return advance();
    }();

    switch (OpCode.getKind()) {
    default: {
      ExprResult RHS = parseExpr(getBinOpPrecedence(OpCode.getKind()));
      if (!RHS.isValid())
        return false;
      LHS = BinaryOperator::Create(LHS.get(), RHS.get(), OpCode);
      break;
    }
    case tok::equal:
    case tok::plusequal:
    case tok::minusequal:
    case tok::starequal:
    case tok::slashequal:
    case tok::caretequal:
    case tok::starstarequal: {
      ExprResult RHS =
          parseExpr(prec::Level(getBinOpPrecedence(OpCode.getKind()) - 1));
      LHS = BinaryOperator::Create(LHS.get(), RHS.get(), OpCode);
    }
    }
  }

  return LHS;
}