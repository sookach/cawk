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

TranslationUnitDecl *Parser::parseTranslationUnit() {
  std::vector<Decl *> Decls;
  for (; (skip<tok::newline, tok::semi>(), !consume(tok::eof));) {
    Decls.push_back(parseDecl());
  }
  return TranslationUnitDecl::Create(Decls);
}

Decl *Parser::parseDecl() {
  if (Tok.is(tok::kw_function))
    return parseFunctionDecl();
  return parseRuleDecl();
}

FunctionDecl *Parser::parseFunctionDecl() {
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
  expect(tok::r_paren);
  auto Body = parseCompoundStmt();
  return FunctionDecl::Create(Identifier, Params, Body);
}

RuleDecl *Parser::parseRuleDecl() {
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

Stmt *Parser::parseStmt() {
  switch (Tok.getKind()) {
  default: {
    auto S = parseSimpleStmt();
    skip<tok::semi, tok::newline>();
    return S;
  }
  case tok::l_brace:
    return parseCompoundStmt();
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

CompoundStmt *Parser::parseCompoundStmt() {
  expect(tok::l_brace);
  std::vector<Stmt *> Stmts;
  for (; (skip<tok::newline, tok::semi>(), !Tok.is(tok::r_brace, tok::eof));)
    Stmts.push_back(parseStmt());

  expect(tok::r_brace);
  return CompoundStmt::Create(Stmts);
}

DoStmt *Parser::parseDoStmt() {
  expect(tok::kw_do);
  auto Body = parseCompoundStmt();
  expect(tok::kw_while);
  expect(tok::l_paren);
  auto Cond = parseExpr();
  expect(tok::r_paren);
  return DoStmt::Create(Cond, Body);
}

Stmt *Parser::parseForStmt() {
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

IfStmt *Parser::parseIfStmt() {
  expect(tok::kw_if);
  expect(tok::l_paren);
  Expr *Cond = parseExpr();
  expect(tok::r_paren);
  Stmt *Then = parseStmt();
  Stmt *Else = consume(tok::kw_else) ? parseStmt() : nullptr;
  return IfStmt::Create(Cond, Then, Else);
}

PrintStmt *Parser::parsePrintStmt() {
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

ReturnStmt *Parser::parseReturnStmt() {
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

  for (; getBinOpPrecedence((Tok.is(tok::newline) ? peek(1) : Tok).getKind()) >
         MinPrec;) {
    skip<tok::newline>();
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
          LHS, parseExpr(prec::Level(getBinOpPrecedence(OpCode.getKind()) - 1)),
          OpCode);
    }
  }

  return LHS;
}

#if 0
program          : item_list
                 | item_list item
                 ;


item_list        : /* empty */
                 | item_list item terminator
                 ;


item             : action
                 | pattern action
                 | normal_pattern
                 | Function NAME      '(' param_list_opt ')'
                       newline_opt action
                 | Function FUNC_NAME '(' param_list_opt ')'
                       newline_opt action
                 ;


param_list_opt   : /* empty */
                 | param_list
                 ;


param_list       : NAME
                 | param_list ',' NAME
                 ;


pattern          : normal_pattern
                 | special_pattern
                 ;


normal_pattern   : expr
                 | expr ',' newline_opt expr
                 ;


special_pattern  : Begin
                 | End
                 ;


action           : '{' newline_opt                             '}'
                 | '{' newline_opt terminated_statement_list   '}'
                 | '{' newline_opt unterminated_statement_list '}'
                 ;


terminator       : terminator NEWLINE
                 |            ';'
                 |            NEWLINE
                 ;


terminated_statement_list : terminated_statement
                 | terminated_statement_list terminated_statement
                 ;


unterminated_statement_list : unterminated_statement
                 | terminated_statement_list unterminated_statement
                 ;


terminated_statement : action newline_opt
                 | If '(' expr ')' newline_opt terminated_statement
                 | If '(' expr ')' newline_opt terminated_statement
                       Else newline_opt terminated_statement
                 | While '(' expr ')' newline_opt terminated_statement
                 | For '(' simple_statement_opt ';'
                      expr_opt ';' simple_statement_opt ')' newline_opt
                      terminated_statement
                 | For '(' NAME In NAME ')' newline_opt
                      terminated_statement
                 | ';' newline_opt
                 | terminatable_statement NEWLINE newline_opt
                 | terminatable_statement ';'     newline_opt
                 ;


unterminated_statement : terminatable_statement
                 | If '(' expr ')' newline_opt unterminated_statement
                 | If '(' expr ')' newline_opt terminated_statement
                      Else newline_opt unterminated_statement
                 | While '(' expr ')' newline_opt unterminated_statement
                 | For '(' simple_statement_opt ';'
                  expr_opt ';' simple_statement_opt ')' newline_opt
                      unterminated_statement
                 | For '(' NAME In NAME ')' newline_opt
                      unterminated_statement
                 ;


terminatable_statement : simple_statement
                 | Break
                 | Continue
                 | Next
                 | Exit expr_opt
                 | Return expr_opt
                 | Do newline_opt terminated_statement While '(' expr ')'
                 ;


simple_statement_opt : /* empty */
                 | simple_statement
                 ;


simple_statement : Delete NAME '[' expr_list ']'
                 | expr
                 | print_statement
                 ;


print_statement  : simple_print_statement
                 | simple_print_statement output_redirection
                 ;


simple_print_statement : Print  print_expr_list_opt
                 | Print  '(' multiple_expr_list ')'
                 | Printf print_expr_list
                 | Printf '(' multiple_expr_list ')'
                 ;


output_redirection : '>'    expr
                 | APPEND expr
                 | '|'    expr
                 ;


expr_list_opt    : /* empty */
                 | expr_list
                 ;


expr_list        : expr
                 | multiple_expr_list
                 ;


multiple_expr_list : expr ',' newline_opt expr
                 | multiple_expr_list ',' newline_opt expr
                 ;


expr_opt         : /* empty */
                 | expr
                 ;


expr             : unary_expr
                 | non_unary_expr
                 ;


unary_expr       : '+' expr
                 | '-' expr
                 | unary_expr '^'      expr
                 | unary_expr '*'      expr
                 | unary_expr '/'      expr
                 | unary_expr '%'      expr
                 | unary_expr '+'      expr
                 | unary_expr '-'      expr
                 | unary_expr          non_unary_expr
                 | unary_expr '<'      expr
                 | unary_expr LE       expr
                 | unary_expr NE       expr
                 | unary_expr EQ       expr
                 | unary_expr '>'      expr
                 | unary_expr GE       expr
                 | unary_expr '˜'      expr
                 | unary_expr NO_MATCH expr
                 | unary_expr In NAME
                 | unary_expr AND newline_opt expr
                 | unary_expr OR  newline_opt expr
                 | unary_expr '?' expr ':' expr
                 | unary_input_function
                 ;


non_unary_expr   : '(' expr ')'
                 | '!' expr
                 | non_unary_expr '^'      expr
                 | non_unary_expr '*'      expr
                 | non_unary_expr '/'      expr
                 | non_unary_expr '%'      expr
                 | non_unary_expr '+'      expr
                 | non_unary_expr '-'      expr
                 | non_unary_expr          non_unary_expr
                 | non_unary_expr '<'      expr
                 | non_unary_expr LE       expr
                 | non_unary_expr NE       expr
                 | non_unary_expr EQ       expr
                 | non_unary_expr '>'      expr
                 | non_unary_expr GE       expr
                 | non_unary_expr '˜'      expr
                 | non_unary_expr NO_MATCH expr
                 | non_unary_expr In NAME
                 | '(' multiple_expr_list ')' In NAME
                 | non_unary_expr AND newline_opt expr
                 | non_unary_expr OR  newline_opt expr
                 | non_unary_expr '?' expr ':' expr
                 | NUMBER
                 | STRING
                 | lvalue
                 | ERE
                 | lvalue INCR
                 | lvalue DECR
                 | INCR lvalue
                 | DECR lvalue
                 | lvalue POW_ASSIGN expr
                 | lvalue MOD_ASSIGN expr
                 | lvalue MUL_ASSIGN expr
                 | lvalue DIV_ASSIGN expr
                 | lvalue ADD_ASSIGN expr
                 | lvalue SUB_ASSIGN expr
                 | lvalue '=' expr
                 | FUNC_NAME '(' expr_list_opt ')'
                      /* no white space allowed before '(' */
                 | BUILTIN_FUNC_NAME '(' expr_list_opt ')'
                 | BUILTIN_FUNC_NAME
                 | non_unary_input_function
                 ;


print_expr_list_opt : /* empty */
                 | print_expr_list
                 ;


print_expr_list  : print_expr
                 | print_expr_list ',' newline_opt print_expr
                 ;


print_expr       : unary_print_expr
                 | non_unary_print_expr
                 ;


unary_print_expr : '+' print_expr
                 | '-' print_expr
                 | unary_print_expr '^'      print_expr
                 | unary_print_expr '*'      print_expr
                 | unary_print_expr '/'      print_expr
                 | unary_print_expr '%'      print_expr
                 | unary_print_expr '+'      print_expr
                 | unary_print_expr '-'      print_expr
                 | unary_print_expr          non_unary_print_expr
                 | unary_print_expr '˜'      print_expr
                 | unary_print_expr NO_MATCH print_expr
                 | unary_print_expr In NAME
                 | unary_print_expr AND newline_opt print_expr
                 | unary_print_expr OR  newline_opt print_expr
                 | unary_print_expr '?' print_expr ':' print_expr
                 ;


non_unary_print_expr : '(' expr ')'
                 | '!' print_expr
                 | non_unary_print_expr '^'      print_expr
                 | non_unary_print_expr '*'      print_expr
                 | non_unary_print_expr '/'      print_expr
                 | non_unary_print_expr '%'      print_expr
                 | non_unary_print_expr '+'      print_expr
                 | non_unary_print_expr '-'      print_expr
                 | non_unary_print_expr          non_unary_print_expr
                 | non_unary_print_expr '˜'      print_expr
                 | non_unary_print_expr NO_MATCH print_expr
                 | non_unary_print_expr In NAME
                 | '(' multiple_expr_list ')' In NAME
                 | non_unary_print_expr AND newline_opt print_expr
                 | non_unary_print_expr OR  newline_opt print_expr
                 | non_unary_print_expr '?' print_expr ':' print_expr
                 | NUMBER
                 | STRING
                 | lvalue
                 | ERE
                 | lvalue INCR
                 | lvalue DECR
                 | INCR lvalue
                 | DECR lvalue
                 | lvalue POW_ASSIGN print_expr
                 | lvalue MOD_ASSIGN print_expr
                 | lvalue MUL_ASSIGN print_expr
                 | lvalue DIV_ASSIGN print_expr
                 | lvalue ADD_ASSIGN print_expr
                 | lvalue SUB_ASSIGN print_expr
                 | lvalue '=' print_expr
                 | FUNC_NAME '(' expr_list_opt ')'
                     /* no white space allowed before '(' */
                 | BUILTIN_FUNC_NAME '(' expr_list_opt ')'
                 | BUILTIN_FUNC_NAME
                 ;


lvalue           : NAME
                 | NAME '[' expr_list ']'
                 | '$' expr
                 ;


non_unary_input_function : simple_get
                 | simple_get '<' expr
                 | non_unary_expr '|' simple_get
                 ;


unary_input_function : unary_expr '|' simple_get
                 ;


simple_get       : GETLINE
                 | GETLINE lvalue
                 ;


newline_opt      : /* empty */
                 | newline_opt NEWLINE
                 ;
#endif
