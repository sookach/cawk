//===--- Parser.cpp - CAWK Language Parser ----------------------*- C++ -*-===//
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

/// parseTranslationUnit
///     translation-unit:
///	        declaration-seq
///
///     declaration-seq:
///	        declaration
///         declaration-seq declaration
DeclResult Parser::parseTranslationUnit() {
  std::vector<Decl *> Decls;
  for (; (skip(tok::newline, tok::semi), !consume(tok::eof));) {
    DeclResult Res = parseDeclaration();
    if (!HasError)
      Decls.push_back(Res.get());
  }
  if (!HasError)
    return TranslationUnitDecl::Create(Decls);
  return false;
}

/// parseDeclaration
///     declaration:
///	        rule-declaration
///	        function-declaration
DeclResult Parser::parseDeclaration() {
  if (Tok.is(tok::kw_function))
    return parseFunctionDeclaration();
  return parseRuleDeclaration();
}

/// parseFunctionDeclaration
///     function-declaration
///         'function' identifier '(' param-list? ')' compound-statement
///
///     param-list:
///         identifier
///         param-list ',' identifier
DeclResult Parser::parseFunctionDeclaration() {
  auto BeginLoc = Lex.getBufferPtr();
  // These two tests should never be fail, but they're here as sanity checks.
  if (!Semantics.check<true>(static_cast<FunctionDecl *>(nullptr)))
    return false;
  if (!expect(tok::kw_function))
    return false;
  auto Identifier = Tok;
  if (!expect(tok::identifier, tok::l_paren)) {
    skipUntil(tok::l_brace, tok::r_brace, tok::eof);
    if (Tok.is(tok::eof))
      return false;
    PanicMode = false;
    parseCompoundStatement();
    return false;
  }
  auto [Valid, Params] = [this] {
    std::vector<ParamVarDecl *> Params;

    if (Tok.is(tok::identifier)) {
      auto BeginLoc = Lex.getBufferPtr();
      auto Iden = advance();
      auto EndLoc = Lex.getBufferPtr();
      Params.push_back(
          ParamVarDecl::Create(Iden, SourceRange(BeginLoc, EndLoc)));
    }

    for (; consume(tok::comma);) {
      auto BeginLoc = Lex.getBufferPtr();
      auto EndLoc = BeginLoc + Tok.getLength();
      Params.push_back(
          ParamVarDecl::Create(Tok, SourceRange(BeginLoc, EndLoc)));
      if (!expect(tok::identifier))
        return std::pair(false, Params);
    }

    return std::pair(true, Params);
  }();
  if (!Valid)
    return false;
  if (!consume(tok::r_paren))
    return false;
  auto Body = parseCompoundStatement();
  if (!Body.isValid())
    return false;

  if (Semantics.check<false>(static_cast<FunctionDecl *>(nullptr)))
    return false;
  return FunctionDecl::Create(Identifier, Params, Body.getAs<CompoundStmt>(),
                              SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parseRuleDeclaration
///     rule-declaration:
///         expression compound-statement?
///         compound-statement
DeclResult Parser::parseRuleDeclaration() {
  auto BeginLoc = Lex.getBufferPtr();
  auto Pattern = [this, BeginLoc] -> ExprResult {
    switch (Tok.getKind()) {
    default:
      return parseExpression();
    case tok::kw_BEGIN: {
      advance<false>();
      auto EndLoc = Lex.getBufferPtr();
      return BeginKeyword::Create(SourceRange(BeginLoc, EndLoc));
    }
    case tok::kw_END: {
      advance<false>();
      auto EndLoc = Lex.getBufferPtr();
      return EndKeyword::Create(SourceRange(BeginLoc, EndLoc));
    }
    }
  }();

  if (!Pattern.isValid())
    return false;

  auto Action = consumeOneOf(tok::semi, tok::newline)
                    ? nullptr
                    : parseCompoundStatement();

  if (!Action.isValid())
    return false;

  return RuleDecl::Create(Pattern.get(), Action.getAs<CompoundStmt>(),
                          SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// statement:
///	    block-statement
///	    basic-statement
///	    ';'
StmtResult Parser::parseStatement() {
  switch (Tok.getKind()) {
  default: {
    auto S = parseSimpleStatement();
    if (!S.isValid())
      return false;
    skip(tok::semi, tok::newline);
    return S;
  }
  case tok::l_brace:
    return parseCompoundStatement();
  case tok::kw_break:
    return parseBreakStatement();
  case tok::kw_do:
    return parseDoStatement();
  case tok::kw_for:
    return parseForStatement();
  case tok::kw_if:
    return parseIfStatement();
  case tok::kw_return:
    return parseReturnStatement();
  case tok::kw_while:
    return parseWhileStatement();
  }
}

StmtResult Parser::parseBreakStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  if (!consume(tok::kw_break))
    return false;
  return BreakStmt::Create(SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parseCompoundStatement
///     compound-statement:
///         '{' statement-sequence '}'
///
///     statement-sequence:
///         statement
///         statement-sequence (block-statement | simple-statement eol)
StmtResult Parser::parseCompoundStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  if (!consume(tok::l_brace))
    return false;

  std::vector<Stmt *> Stmts;
  for (; (skip(tok::newline, tok::semi), !Tok.is(tok::r_brace, tok::eof));) {
    StmtResult S = parseStatement();
    if (!S.isValid()) {
      recover();
      return false;
    }
  }

  if (!consume(tok::r_brace))
    return false;

  return CompoundStmt::Create(Stmts, SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parseDoStatement
///     do-statement:
///         'do' statement 'while' '(' expression ')'
StmtResult Parser::parseDoStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  if (!consume(tok::kw_do))
    return false;

  auto Body = parseCompoundStatement();
  if (!Body.isValid())
    return false;

  if (!consume(tok::kw_while, tok::l_paren))
    return false;

  auto Cond = parseExpression();
  if (!Cond.isValid())
    return false;

  if (!consume(tok::r_paren))
    return false;

  return DoStmt::Create(Cond.get(), Body.get(),
                        SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parseForStatement
///     for-statement:
///	        'for' '(' identifier 'in' identifier ')' statement
///	        'for' '(' (print-statement | expression)? ';' expression? ';'
///             (print-statement | expression)? ')' statement
StmtResult Parser::parseForStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  if (!consume(tok::kw_for, tok::l_paren))
    return false;

  if (peek(1).is(tok::kw_in)) {
    DeclRefExpr *LoopVar =
        DeclRefExpr::Create(Tok, SourceRange(Tok.getIdentifier()));
    if (!expect(tok::identifier, tok::kw_in))
      return false;
    DeclRefExpr *Range =
        DeclRefExpr::Create(Tok, SourceRange(Tok.getIdentifier()));
    if (!expect(tok::identifier, tok::r_paren))
      return false;
    StmtResult Body = parseStatement();
    if (!Body.isValid())
      return false;
    return ForRangeStmt::Create(LoopVar, Range, Body.get(),
                                SourceRange(BeginLoc, Lex.getBufferPtr()));
  }

  StmtResult Init = Tok.is(tok::semi) ? true : parseSimpleStatement();
  if (!Init.isValid())
    return false;
  if (!expect(tok::semi))
    return false;
  ExprResult Cond = Tok.is(tok::semi) ? true : parseExpression();
  if (!Cond.isValid())
    return false;
  if (!expect(tok::semi))
    return false;
  StmtResult Inc = Tok.is(tok::r_paren) ? true : parseSimpleStatement();
  if (!Inc.isValid())
    return false;
  if (!expect(tok::r_paren))
    return false;
  StmtResult Body = parseStatement();
  if (!Body.isValid())
    return false;
  return ForStmt::Create(Init.get(), Cond.get(), Inc.get(), Body.get(),
                         SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parseIfStatement
///     if-statement:
///	        'if' '(' expression ')' statement ('else' statement)?
StmtResult Parser::parseIfStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  if (!expect(tok::kw_if, tok::l_paren))
    return false;
  ExprResult Cond = parseExpression();
  if (!Cond.isValid())
    return false;
  if (!expect(tok::r_paren))
    return false;
  StmtResult Then = parseStatement();
  if (!Then.isValid())
    return false;
  StmtResult Else = consume(tok::kw_else) ? parseStatement() : true;
  if (!Else.isValid())
    return false;
  return IfStmt::Create(Cond.get(), Then.get(), Else.get(),
                        SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parsePrintStatement
///     print-statement:
///	        'print' expression-list?
///	        'printf' expression-list?
StmtResult Parser::parsePrintStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  Token Iden = Tok;
  if (!consumeOneOf<true>(tok::kw_print, tok::kw_printf))
    return false;

  auto [Args, Valid] = [this] -> std::pair<std::vector<Expr *>, bool> {
    if (consumeOneOf(tok::newline, tok::semi))
      return {{}, true};

    ExprResult Arg = parseExpression();
    if (!Arg.isValid())
      return {{}, false};
    std::vector Args = {Arg.get()};

    for (; consume(tok::comma);) {
      Arg = parseExpression();
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
    return {Tok, parseExpression()};
  }();

  return PrintStmt::Create(Iden, Args, OpCode, Output.get(),
                           SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parseReturnStatement
///     'return' expression?
StmtResult Parser::parseReturnStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  if (!consume(tok::kw_return))
    return false;
  ExprResult E = parseExpression();
  if (!E.isValid())
    return false;
  if (!Semantics.check(static_cast<ReturnStmt *>(nullptr)))
    return false;
  return ReturnStmt::Create(E.get(), SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parseSimpleStatement
///     expression
///     print-statement
StmtResult Parser::parseSimpleStatement() {
  switch (Tok.getKind()) {
  default:
    return parseValueStatement();
  case tok::kw_print:
  case tok::kw_printf:
    return parsePrintStatement();
  }
}

/// parseValueStatement
///     expression
StmtResult Parser::parseValueStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  ExprResult Value = parseExpression();
  if (!Value.isValid())
    return false;
  return ValueStmt::Create(Value.get(),
                           SourceRange(BeginLoc, Lex.getBufferPtr()));
}

/// parseWhileStatement
///     while-statement:
///            'while' '(' expression ')' statement
StmtResult Parser::parseWhileStatement() {
  auto BeginLoc = Lex.getBufferPtr();
  if (!Semantics.check<true>(static_cast<WhileStmt *>(nullptr)))
    return false;
  if (!consume(tok::kw_while, tok::l_paren))
    return false;

  ExprResult Cond = parseExpression();
  if (!Cond.isValid())
    return false;

  if (!consume(tok::r_paren))
    return false;

  StmtResult Body = parseStatement();
  if (!Body.isValid())
    return false;

  if (!Semantics.check<false>(static_cast<WhileStmt *>(nullptr)))
    return false;
  return WhileStmt::Create(Cond.get(), Body.get(),
                           SourceRange(BeginLoc, Lex.getBufferPtr()));
}

ExprResult Parser::parseExpression(prec::Level MinPrec) {
  auto BeginLoc = Lex.getBufferPtr();
  auto NUD = [this] -> ExprResult {
    switch (Tok.getKind()) {
    default:
      return false;
    case tok::kw_gsub:
    case tok::kw_index:
    case tok::kw_match:
    case tok::kw_split:
    case tok::kw_sprintf:
    case tok::kw_sub:
    case tok::kw_substr:
    case tok::identifier: {
      auto BeginLoc = Lex.getBufferPtr();
      return DeclRefExpr::Create(advance<true>(),
                                 SourceRange(BeginLoc, Lex.getBufferPtr()));
    }
    case tok::numeric_constant: {
      auto BeginLoc = Lex.getBufferPtr();
      return FloatingLiteral::Create(advance<true>(),
                                     SourceRange(BeginLoc, Lex.getBufferPtr()));
    }
    case tok::string_literal: {
      auto BeginLoc = Lex.getBufferPtr();
      return StringLiteral::Create(advance<true>(),
                                   SourceRange(BeginLoc, Lex.getBufferPtr()));
    }
    case tok::plusplus:
    case tok::minusminus:
    case tok::exclaim:
    case tok::plus:
    case tok::minus:
    case tok::dollar: {
      auto BeginLoc = Lex.getBufferPtr();
      Token OpCode = advance();
      ExprResult SubExpr = parseExpression(prec::Maximum);
      if (!SubExpr.isValid())
        return false;
      return UnaryOperator::Create(OpCode, SubExpr.get(), UnaryOperator::Prefix,
                                   SourceRange(BeginLoc, Lex.getBufferPtr()));
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
        auto BeginLoc = Lex.getBufferPtr();
        auto OpCode = advance();
        return UnaryOperator::Create(OpCode, LHS.get(), UnaryOperator::Prefix,
                                     SourceRange(BeginLoc, Lex.getBufferPtr()));
      }
      case tok::l_paren: {
        auto BeginLoc = Lex.getBufferPtr();
        expect(tok::l_paren);
        std::vector<Expr *> Args;

        if (!Tok.is(tok::r_paren)) {
          ExprResult Arg = parseExpression();
          if (!Arg.isValid())
            return false;
          Args.push_back(Arg.get());
        }

        for (ExprResult Arg; consume(tok::comma);) {
          Arg = parseExpression();
          if (!Arg.isValid())
            return false;
          Args.push_back(Arg.get());
        }

        if (!consume(tok::r_paren))
          return false;

        LHS = CallExpr::Create(LHS.get(), Args,
                               SourceRange(BeginLoc, Lex.getBufferPtr()));
        break;
      }
      case tok::l_square: {
        auto BeginLoc = Lex.getBufferPtr();
        expect(tok::l_square);

        ExprResult Arg = parseExpression();
        if (!Arg.isValid())
          return false;
        std::vector Args = {Arg.get()};

        for (; consume(tok::comma);) {
          Arg = parseExpression();
          if (!Arg.isValid())
            return false;
          Args.push_back(Arg.get());
        }

        if (!consume(tok::r_square))
          return false;

        LHS = ArraySubscriptExpr::Create(
            LHS.get(), Args, SourceRange(BeginLoc, Lex.getBufferPtr()));
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

    skip(tok::newline);

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
      ExprResult RHS = parseExpression(getBinOpPrecedence(OpCode.getKind()));
      if (!RHS.isValid())
        return false;
      LHS = BinaryOperator::Create(LHS.get(), RHS.get(), OpCode,
                                   SourceRange(BeginLoc, Lex.getBufferPtr()));
      break;
    }
    case tok::equal:
    case tok::plusequal:
    case tok::minusequal:
    case tok::starequal:
    case tok::slashequal:
    case tok::caretequal:
    case tok::starstarequal: {
      ExprResult RHS = parseExpression(
          prec::Level(getBinOpPrecedence(OpCode.getKind()) - 1));
      LHS = BinaryOperator::Create(LHS.get(), RHS.get(), OpCode,
                                   SourceRange(BeginLoc, Lex.getBufferPtr()));
    }
    }
  }

  return LHS;
}