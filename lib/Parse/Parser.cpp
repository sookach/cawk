#include "Parse/Parser.h"

using namespace cawk;

template <bool LexNewline, bool LexRegex> Token Parser::advance() {
  auto Prev = Tok;
  Lex.next<LexNewline, LexRegex>(Tok);
  return Prev;
}

template Token Parser::advance<false, false>();
template Token Parser::advance<false, true>();
template Token Parser::advance<true, false>();
template Token Parser::advance<true, true>();

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

DeclResult Parser::parseDecl() {
  if (Tok.is(tok::kw_function))
    return parseFunctionDecl();
  return parseRuleDecl();
}

DeclResult Parser::parseFunctionDecl() {
  if (!consume(tok::kw_function))
    return false;
  auto Identifier = Tok;
  if (!consume(tok::identifier, tok::l_paren))
    return false;
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
  if (!consume(tok::r_paren))
    return false;
  auto Body = parseCompoundStmt();
  if (Body.isValid())
    return FunctionDecl::Create(Identifier, Params, Body.get());
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
    if (!S.isValid())
      return false;
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
  expect(tok::r_paren);
  return DoStmt::Create(Cond, Body);
}

StmtResult Parser::parseForStmt() {
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

StmtResult Parser::parseIfStmt() {
  expect(tok::kw_if);
  expect(tok::l_paren);
  Expr *Cond = parseExpr();
  expect(tok::r_paren);
  Stmt *Then = parseStmt();
  Stmt *Else = consume(tok::kw_else) ? parseStmt() : nullptr;
  return IfStmt::Create(Cond, Then, Else);
}

StmtResult Parser::parsePrintStmt() {
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

StmtResult Parser::parseReturnStmt() {
  expect(tok::kw_return);
  return ReturnStmt::Create(parseExpr());
}

Stmt *Parser::parseSimpleStmt() {
  switch (Tok.getKind()) {
  default:
    return parseValueStmt();
  case tok::kw_print:
  case tok::kw_printf:
    return parsePrintStmt();
  }
}

ValueStmt *Parser::parseValueStmt() {
  Expr *Value = parseExpr();
  return ValueStmt::Create(Value);
}

WhileStmt *Parser::parseWhileStmt() {
  expect(tok::kw_while);
  expect(tok::l_paren);
  auto Cond = parseExpr();
  expect(tok::r_paren);

  return WhileStmt::Create(Cond, parseStmt());
}

Expr *Parser::parseExpr(prec::Level MinPrec) {
  auto NUD = [this] -> Expr * {
    switch (Tok.getKind()) {
    default:
      // TODO: handle error
      return nullptr;
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
        expect(tok::l_paren);
        std::vector<Expr *> Args;

        if (!Tok.is(tok::r_paren))
          Args.push_back(parseExpr());

        for (; consume(tok::comma);)
          Args.push_back(parseExpr());

        expect(tok::r_paren);

        LHS = CallExpr::Create(LHS, Args);
        break;
      }
      case tok::l_square: {
        expect(tok::l_square);
        std::vector Args = {parseExpr()};

        for (; consume(tok::comma);)
          Args.push_back(parseExpr());

        expect(tok::r_square);

        LHS = ArraySubscriptExpr::Create(LHS, Args);
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
          LHS, parseExpr(prec::Level(getBinOpPrecedence(OpCode.getKind()) - 1)),
          OpCode);
    }
  }

  return LHS;
}