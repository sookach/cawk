//===--- Parser.cpp - CAWK Language Parser ----------------------*- C++ -*-===//
//
//  This file implements the Parser interface.
//
//===----------------------------------------------------------------------===//
#include "Parse/Parser.h"
#include "Support/ScopeExit.h"
#include "Support/Support.h"

using namespace cawk;

bool isTerminatedStatement(Stmt *S) {
  if (isa<CompoundStmt>(S))
    return true;
  return *std::prev(std::cend(S->getSourceRange())) == ';' ||
         *std::prev(std::cend(S->getSourceRange())) == '\n';
}

/// \brief Consumes the current lookahead token and advances to the next one.
/// \tparam NL - should the lexer lex newlines?
/// \tparam RE - are we attempting to lex a regex?
/// \return The current lookahead token.
template <bool NL, bool RE> Token Parser::advance() {
  auto Prev = Tok;
  Lex.next<NL, RE>(Tok);
  return Prev;
}

template Token Parser::advance<false, false>();
template Token Parser::advance<false, true>();
template Token Parser::advance<true, false>();
template Token Parser::advance<true, true>();

/// \brief Peeks N tokens ahead.
/// \tparam NL - should the lexer lex newlines?
/// \tparam RE - are we attempting to lex a regex?
/// \param N - the number of tokens to peek.
/// \return The Nth token ahead.
template <bool NL, bool RE> Token Parser::peek(std::size_t N) const {
  Token T;
  auto BufferPtr = Lex.getBufferPtr();

  for (; N != 0; --N)
    Lex.next<NL, RE>(T);

  Lex.setBufferPtr(BufferPtr);
  return T;
}

template Token Parser::peek<false, false>(std::size_t N) const;
template Token Parser::peek<false, true>(std::size_t N) const;
template Token Parser::peek<true, false>(std::size_t N) const;
template Token Parser::peek<true, true>(std::size_t N) const;

/// parseTranslationUnit
///     translation-unit:
///	      declaration-seq
///
///     declaration-seq:
///	      declaration
///       declaration-seq declaration
DeclResult Parser::parseTranslationUnit() {
  std::vector<Decl *> Decls;
  for (; (skip(tok::newline, tok::semi), !consume(tok::eof));) {
    DeclResult Res = parseDeclaration();
    if (Res.isValid())
      Decls.push_back(Res.get());
  }
  if (HasError)
    return false;

  return TranslationUnitDecl::Create(Decls);
}

/// parseDeclaration
///     declaration:
///	      rule-declaration
///	      function-declaration
DeclResult Parser::parseDeclaration() {
  if (Tok.is(tok::kw_function))
    return parseFunctionDeclaration();
  return parseRuleDeclaration();
}

/// parseFunctionDeclaration
///     function-declaration
///       'function' identifier parameter-declaration-clause compound-statement
///
///     param-list:
///       identifier
///       param-list ',' identifier
DeclResult Parser::parseFunctionDeclaration() {
  auto BeginLoc = std::cbegin(Tok.getRawData());
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

  auto [Params, Valid] = parseParameterDeclarationClause();

  Actions.actOnStartOfFunctionBody();
  auto Body = parseCompoundStatement();
  Actions.actOnFinishOfFunctionBody();

  if (!Body.isValid())
    return false;

  return Actions.actOnFunctionDeclaration(FunctionDecl::Create(
      Identifier, Params, Body.getAs<CompoundStmt>(),
      SourceRange(BeginLoc, std::cend(Body.get()->getSourceRange()))));
}

/// parseRuleDeclaration
///     rule-declaration:
///       expression compound-statement?
///       compound-statement
DeclResult Parser::parseRuleDeclaration() {
  auto BeginLoc = std::cbegin(Tok.getRawData());
  auto Pattern = [this, BeginLoc] -> ExprResult {
    switch (Tok.getKind()) {
    default:
      return parseExpression();
    case tok::l_brace:
      return nullptr;
    case tok::kw_BEGIN: {
      advance();
      auto EndLoc = Lex.getBufferPtr();
      return BeginKeyword::Create(SourceRange(BeginLoc, EndLoc));
    }
    case tok::kw_END: {
      advance();
      auto EndLoc = Lex.getBufferPtr();
      return EndKeyword::Create(SourceRange(BeginLoc, EndLoc));
    }
    }
  }();

  if (!Pattern.isValid())
    return false;

  auto EndLoc = BeginLoc;
  if (Pattern.get() != nullptr)
    EndLoc = std::cend(Pattern.get()->getSourceRange());

  auto Action = consumeOneOf(tok::semi, tok::newline)
                    ? nullptr
                    : parseCompoundStatement();

  if (!Action.isValid())
    return false;

  if (Action.get() != nullptr)
    EndLoc = std::cend(Action.get()->getSourceRange());

  return RuleDecl::Create(Pattern.get(), Action.getAs<CompoundStmt>(),
                          SourceRange(BeginLoc, EndLoc));
}

/// statement:
///	    terminated-statement:
///      compound-statement
///      terminated-simple-statement
///      terminated-break-statement
///      terminated-do-statement
///      terminated-for-statement
///      terminated-if-statement
///      terminated-return-statement
///      terminated-while-statement
///
///	    unterminated-statement:
///      unterminated-simple-statement
///      unterminated-break-statement
///      unterminated-do-statement
///      unterminated-for-statement
///      unterminated-if-statement
///      unterminated-return-statement
///      unterminated-while-statement
StmtResult Parser::parseStatement() {
  switch (Tok.getKind()) {
  default: {
    auto S = parseSimpleStatement();
    if (!S.isValid())
      return false;
    skip(tok::semi, tok::newline);
    return S;
  }
  case tok::newline:
  case tok::semi:
    return parseNullStatement();
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

/// parseBreakStatement
///     terminated-break-statement:
///       'break' terminator
///
///     unterminated-break-statement:
///       'break'
StmtResult Parser::parseBreakStatement() {
  auto BeginLoc = std::cbegin(Tok.getRawData());
  if (!consume(tok::kw_break))
    return false;
  auto EndLoc = [&, this] {
    if (Tok.is(tok::newline, tok::semi)) {
      auto EndLoc = std::cend(Tok.getRawData());
      advance();
      return EndLoc;
    }
    return BeginLoc + std::size(tok::getKeywordSpelling(tok::kw_break));
  }();
  return Actions.actOnBreakStatement(
      BreakStmt::Create(SourceRange(BeginLoc, EndLoc)));
}

/// parseCompoundStatement
///     compound-statement:
///       '{' statement-sequence '}'
///
///     statement-sequence:
///       statement
///       statement-sequence terminated-statement
StmtResult Parser::parseCompoundStatement() {
  assert(Tok.is(tok::l_brace) && "Not a compound statement");
  auto BeginLoc = std::cbegin(Tok.getRawData());
  consume(tok::l_brace);

  std::vector<Stmt *> Stmts;
  for (; !Tok.is(tok::r_brace, tok::eof);) {
    if (StmtResult S = parseStatement(); S.isValid()) {
      Stmts.push_back(S.get());
    } else {
      recover();
      return false;
    }

    if (!isTerminatedStatement(Stmts.back()))
      break;
  }

  auto EndLoc = std::cend(Tok.getRawData());
  if (!expect(tok::r_brace))
    return false;

  return CompoundStmt::Create(Stmts, SourceRange(BeginLoc, EndLoc));
}

/// parseDoStatement
///     terminated-do-statement:
///       'do' statement 'while' '(' expression ')' terminator
///
///     unterminated-do-statement:
///       'do' statement 'while' '(' expression ')'
StmtResult Parser::parseDoStatement() {
  Actions.actOnStartOfDoStatement();
  auto BeginLoc = std::cbegin(Tok.getRawData());
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

  auto EndLoc = std::cend(Tok.getRawData());

  if (!consume(tok::r_paren))
    return false;

  if (Tok.is(tok::newline, tok::semi)) {
    EndLoc = std::cend(Tok.getRawData());
    advance();
  }

  Actions.actOnFinishOfDoStatement();

  return DoStmt::Create(Cond.get(), Body.get(), SourceRange(BeginLoc, EndLoc));
}

/// parseForStatement
///     terminated-for-statement:
///	      'for' '(' identifier 'in' identifier ')' terminated-statement
///	      'for' '(' (print-statement | expression)? ';' expression? ';'
///             (print-statement | expression)? ')' terminated-statement
///
///     unterminated-for-statement:
///	      'for' '(' identifier 'in' identifier ')' unterminated-statement
///	      'for' '(' (print-statement | expression)? ';' expression? ';'
///             (print-statement | expression)? ')' unterminated-statement
StmtResult Parser::parseForStatement() {
  Actions.actOnStartOfForStatement();
  auto BeginLoc = std::cbegin(Tok.getRawData());
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
    auto EndLoc = std::cend(Tok.getRawData());
    consume(tok::newline);
    StmtResult Body =
        consumeTerminator() ? StmtResult(nullptr) : parseStatement();
    if (!Body.isValid())
      return false;
    if (Body.get() != nullptr)
      EndLoc = std::cend(Body.get()->getSourceRange());
    Actions.actOnFinishOfForStatement();
    return ForRangeStmt::Create(LoopVar, Range, Body.get(),
                                SourceRange(BeginLoc, EndLoc));
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
  auto EndLoc = std::cend(Tok.getRawData());
  StmtResult Body = parseStatement();
  if (!Body.isValid())
    return false;
  if (Body.get() != nullptr)
    EndLoc = std::cend(Body.get()->getSourceRange());
  Actions.actOnFinishOfForStatement();
  return ForStmt::Create(Init.get(), Cond.get(), Inc.get(), Body.get(),
                         SourceRange(BeginLoc, EndLoc));
}

/// parseIfStatement
///   terminated-if-statement:
///	    'if' '(' expression ')' terminated-statement
///	    'if' '(' expression ')' statement 'else' terminated-statement
///
///   unterminated-if-statement:
///	    'if' '(' expression ')' unterminated-statement
///	    'if' '(' expression ')' statement 'else' unterminated-statement
StmtResult Parser::parseIfStatement() {
  auto BeginLoc = std::cbegin(Tok.getRawData());
  if (!expect(tok::kw_if, tok::l_paren))
    return false;
  ExprResult Cond = parseExpression();
  if (!Cond.isValid())
    return false;
  if (!expect(tok::r_paren))
    return false;
  auto EndLoc = std::cend(Tok.getRawData());
  consume(tok::newline);
  StmtResult Then = [this] -> StmtResult {
    if (consumeTerminator())
      return nullptr;
    return parseStatement();
  }();
  if (!Then.isValid())
    return false;
  if (Then.get() != nullptr)
    EndLoc = std::cend(Then.get()->getSourceRange());
  StmtResult Else = consume(tok::kw_else) ? parseStatement() : true;
  if (!Else.isValid())
    return false;
  if (Else.get() != nullptr)
    EndLoc = std::cend(Else.get()->getSourceRange());
  return IfStmt::Create(Cond.get(), Then.get(), Else.get(),
                        SourceRange(BeginLoc, EndLoc));
}

/// parseNullStatement
///     null-statement:
///       terminator
StmtResult Parser::parseNullStatement() {
  assert(Tok.is(tok::newline, tok::semi) && "Not a null statement");
  auto BeginLoc = std::cbegin(Tok.getRawData());
  advance();
  return NullStmt::Create(SourceRange(BeginLoc, BeginLoc + 1));
}

/// parsePrintStatement
///   terminated-print-statement:
///	    'print' expression-list terminator
///	    'printf' expression-list terminator
///     'print' '(' expression-list ')' terminator
///     'printf' '(' expression-list ')' terminator
///
///   unterminated-print-statement:
///	    'print' expression-list
///	    'printf' expression-list
///     'print' '(' expression-list ')'
///     'printf' '(' expression-list ')'
StmtResult Parser::parsePrintStatement() {
  auto BeginLoc = std::cbegin(Tok.getRawData());
  Token Iden = Tok;
  if (!consumeOneOf<true>(tok::kw_print, tok::kw_printf))
    return false;

  std::vector<Expr *> Args;

  if (!Tok.is(tok::newline, tok::semi)) {
    bool Paren = Tok.is(tok::l_paren) &&
                 std::cend(Iden.getRawData()) == std::cbegin(Tok.getRawData());
    if (Paren) {
      advance();
      ++ParenCount;
      auto OnExit = make_scope_exit([this] { --ParenCount; });
    }
    ExprResult Arg = parseExpression();
    if (!Arg.isValid())
      return false;
    Args.push_back(Arg.get());
    for (; consume(tok::comma);) {
      Arg = parseExpression();
      if (!Arg.isValid())
        return false;
      Args.push_back(Arg.get());
    }
    if (Paren && !expect(tok::r_paren))
      return false;
  }

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

  auto EndLoc = [&, this] {
    if (Tok.is(tok::newline, tok::semi)) {
      auto EndLoc = std::cend(Tok.getRawData());
      advance();
      return EndLoc;
    }
    return std::empty(Args) ? std::cend(Iden.getIdentifier())
                            : std::cend(Args.back()->getSourceRange());
  }();

  return PrintStmt::Create(Iden, Args, OpCode, Output.get(),
                           SourceRange(BeginLoc, EndLoc));
}

/// parseReturnStatement
///   terminated-return-statement:
///     'return' expression? terminator
///
///   unterminated-return-statement:
///     'return' expression?
StmtResult Parser::parseReturnStatement() {
  auto BeginLoc = std::cbegin(Tok.getRawData());
  if (!consume(tok::kw_return))
    return false;
  auto EndLoc = std::cend(Tok.getRawData());
  ExprResult E = parseExpression();
  if (!E.isValid())
    return false;

  if (E.get() != nullptr)
    EndLoc = std::cend(E.get()->getSourceRange());

  if (Tok.is(tok::newline, tok::semi)) {
    EndLoc = std::cend(Tok.getRawData());
    advance();
  }

  return Actions.actOnReturnStatement(
      ReturnStmt::Create(E.get(), SourceRange(BeginLoc, EndLoc)));
}

/// parseSimpleStatement
///   terminated-simple-statement:
///     terminated-value-statement
///     terminated-print-statement
///
///   unterminated-simple-statement:
///     unterminated-value-statement
///     unterminated-print-statement
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
///   terminated-value-statement:
///     expression terminator
///
///   unterminated-value-statement:
///     expression
StmtResult Parser::parseValueStatement() {
  ExprResult Value = parseExpression();
  if (!Value.isValid())
    return false;
  auto EndLoc = std::cend(Value.get()->getSourceRange());
  if (Tok.is(tok::newline, tok::semi)) {
    EndLoc = std::cend(Tok.getRawData());
    advance();
  }
  return ValueStmt::Create(
      Value.get(),
      SourceRange(std::cbegin(Value.get()->getSourceRange()), EndLoc));
}

/// parseWhileStatement
///     terminated-while-statement:
///       'while' '(' expression ')' terminated-statement
///
///     unterminated-while-statement:
///       'while' '(' expression ')' unterminated-statement
StmtResult Parser::parseWhileStatement() {
  Actions.actOnStartOfWhileStatement();
  auto BeginLoc = std::cbegin(Tok.getRawData());
  if (!consume(tok::kw_while, tok::l_paren))
    return false;

  ExprResult Cond = parseExpression();
  if (!Cond.isValid())
    return false;

  auto EndLoc = std::cend(Tok.getRawData());
  if (!consume(tok::r_paren))
    return false;

  StmtResult Body = [this] -> StmtResult {
    if (consumeTerminator())
      return nullptr;
    return parseStatement();
  }();

  if (!Body.isValid())
    return false;

  if (Body.get() != nullptr)
    EndLoc = std::cend(Body.get()->getSourceRange());

  Actions.actOnFinishOfWhileStatement();
  return WhileStmt::Create(Cond.get(), Body.get(),
                           SourceRange(BeginLoc, EndLoc));
}

ExprResult Parser::parseExpression(prec::Level MinPrec) {
  auto BeginLoc = std::cbegin(Tok.getRawData());
  auto ParseAtom = [&, this] -> ExprResult {
    switch (Tok.getKind()) {
    default:
      return false;
    case tok::l_paren: {
      advance();
      ++ParenCount;
      auto OnExit = make_scope_exit([this] { --ParenCount; });
      ExprResult SubExpr = parseExpression();
      if (!SubExpr.isValid())
        return false;
      if (!expect(tok::r_paren))
        return false;
      return SubExpr;
    }
    case tok::kw_gsub:
    case tok::kw_index:
    case tok::kw_match:
    case tok::kw_split:
    case tok::kw_sprintf:
    case tok::kw_sub:
    case tok::kw_substr:
    case tok::identifier: {
      auto Identifier = advance();
      DeclRefExpr *D = DeclRefExpr::Create(
          Identifier, SourceRange(BeginLoc, std::end(Identifier.getRawData())));
      return D;
    }
    case tok::numeric_constant: {
      auto Literal = advance();
      return FloatingLiteral::Create(
          Literal, SourceRange(BeginLoc, std::cbegin(Tok.getRawData())));
    }
    case tok::string_literal: {
      auto Literal = advance();
      return StringLiteral::Create(
          Literal, SourceRange(BeginLoc, std::cbegin(Tok.getRawData())));
    }
    case tok::plusplus:
    case tok::minusminus:
    case tok::exclaim:
    case tok::plus:
    case tok::minus:
    case tok::dollar: {
      Token OpCode = advance();
      ExprResult SubExpr = parseExpression(prec::Maximum);
      if (!SubExpr.isValid())
        return false;
      return UnaryOperator::Create(
          OpCode, SubExpr.get(), UnaryOperator::Prefix,
          SourceRange(BeginLoc, SubExpr.get()->getSourceRange().first));
    }
    case tok::kw_function: {
      advance();
      auto [Params, Valid] = parseParameterDeclarationClause();
      if (!Valid)
        return false;
      Actions.actOnStartOfFunctionBody();
      auto Body = parseCompoundStatement();
      Actions.actOnFinishOfFunctionBody();
      if (!Body.isValid())
        return false;
      return LambdaExpr::Create(
          Params, Body.getAs<CompoundStmt>(),
          SourceRange(BeginLoc, std::cend(Body.get()->getSourceRange())));
    }
    }
  };

  auto LHS = [&, this](ExprResult LHS) -> ExprResult {
    if (!LHS.isValid())
      return false;

    for (;;) {
      switch (Tok.getKind()) {
      default:
        return LHS;
      case tok::plusplus:
      case tok::minusminus: {
        auto OpCode = advance();
        return UnaryOperator::Create(
            OpCode, LHS.get(), UnaryOperator::Prefix,
            SourceRange(BeginLoc, std::cbegin(Tok.getRawData())));
      }
      case tok::l_paren: {
        if (std::end(LHS.get()->getSourceRange()) !=
            std::cbegin(Tok.getRawData())) {
          return LHS;
        }

        advance();
        ++ParenCount;
        auto OnExit = make_scope_exit([this] { --ParenCount; });
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

        if (!expect(tok::r_paren))
          return false;

        LHS = CallExpr::Create(
            LHS.get(), Args,
            SourceRange(BeginLoc, std::cbegin(Tok.getRawData())));
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
  }(ParseAtom());

  for (;;) {
    if (ParenCount != 0)
      skip(tok::newline);

    auto NextTokPrec = getBinOpPrecedence(Tok.getKind());

    if (NextTokPrec < MinPrec)
      break;

    skip(tok::newline);

    auto OpCode = [this, NextTokPrec] {
      if (NextTokPrec == prec::StringConcat) {
        auto SpaceTok = Tok;
        Lex.formSpaceToken(SpaceTok, std::cbegin(SpaceTok.getRawData()) - 1);
        return SpaceTok;
      }
      return advance();
    }();

    bool IsRightAssoc = NextTokPrec == prec::Assignment;
    ExprResult RHS =
        parseExpression(static_cast<prec::Level>(NextTokPrec + !IsRightAssoc));
    if (!RHS.isValid())
      return false;
    LHS = BinaryOperator::Create(LHS.get(), RHS.get(), OpCode,
                                 SourceRange(BeginLoc, Lex.getBufferPtr()));
  }

  return LHS;
}

/// parseParameterDeclarationClause
///     parameter-declaration-clause:
///       ( parameter-declaration-list? )
///
///     parameter-declaration-list:
///       parameter-declaration
///       parameter-declaration-list, parameter-declaration
std::pair<std::vector<VarDecl *>, bool>
Parser::parseParameterDeclarationClause() {
  std::vector<VarDecl *> Params;
  if (!expect(tok::l_paren))
    return std::pair(Params, false);

  ++ParenCount;
  auto OnExit = make_scope_exit([this] { --ParenCount; });

  if (Tok.is(tok::identifier)) {
    std::string_view SrcRange = Tok.getIdentifier();
    auto Iden = advance();
    Params.push_back(VarDecl::Create(
        DeclRefExpr::Create(Iden, SourceRange(SrcRange)), SrcRange));
  }

  for (; consume(tok::comma);) {
    std::string_view SrcRange = Tok.getIdentifier();
    Params.push_back(VarDecl::Create(
        DeclRefExpr::Create(Tok, SourceRange(SrcRange)), SrcRange));
    if (!expect(tok::identifier))
      return std::pair(Params, false);
  }

  if (!Actions.actOnParamList(Params))
    return std::pair(Params, false);

  if (!expect(tok::r_paren))
    return std::pair(Params, false);

  return std::pair(Params, true);
}