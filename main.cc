enum struct TokenKind {
  TK_LSquare,
  TK_RSquare,
  TK_LParen,
  TK_RParen,
  TK_LBrace,
  TK_RBrace,
  TK_Dot,
  TK_Comma,
  TK_Bang,
  TK_Tilde,
  TK_Equal,
  TK_EqualEqual,
  TK_BangEqual,
  TK_Less,
  TK_LessEqual,
  TK_Greater,
  TK_GreaterEqual,
  TK_Plus,
  TK_Minus,
  TK_Slash,
  TK_Star,
  TK_Caret,
  TK_Percent,
  TK_Amp,
  TK_AmpAmp,
  TK_Pipe,
  TK_PipePipe,
  TK_Semi,
  TK_Question,
  TK_Colon,
  TK_Dollar,
  TK_NL,

  // keywords
  TK_Begin,
  TK_Break,
  TK_Continue,
  TK_Do,
  TK_Delete,
  TK_End,
  TK_Exit,
  TK_For,
  TK_Function,
  TK_Getline,
  TK_In,
  TK_If,
  TK_Next,
  TK_Nextfile,
  TK_Print,
  TK_Printf,
  TK_Switch,
  TK_Return,
  TK_While,

  // Tokens
  TK_Identifier,
  TK_StringLiteral,
  TK_RegexLiteral,
};

class Parser {
  using enum TokenKind;

  bool Token(TokenKind) { return true; }

  bool Error() { return false; }

  bool Empty() { return true; }

  bool Program() { return Pas() || Error(); }

  bool And() { return AND() || And() && Nl(); }

  bool Bor() { return BOR() || Bor() && Nl(); }

  bool Comma() { return Token(TK_Comma) || Comma() && Nl(); }

  bool Do() { return DO() || Do() && Nl(); }

  bool Else() { return ELSE() || Else() && Nl(); }

  bool For() {
    return FOR() && Token(TK_LParen) && OptSimpleStmt() && Token(TK_Semi) &&
               OptNl() && Pattern() && Token(TK_Semi) && OptNl() &&
               OptSimpleStmt() && RParen() && Stmt() ||
           FOR() && Token(TK_LParen) && OptSimpleStmt() && Token(TK_Semi) &&
               Token(TK_Semi) && OptNl() && OptSimpleStmt() && RParen() &&
               Stmt() ||
           FOR() && Token(TK_LParen) && VarName() && IN() && VarName() &&
               RParen() && Stmt();
  }

  bool FuncName() { return VAR() || CALL(); }

  bool If() { return IF() && Token(TK_LParen) && Pattern() && RParen(); }

  bool LBrace() { return Token(TK_LBrace) || LBrace() && NL(); }

  bool Nl() { return NL() || Nl() && NL(); }

  bool OptNl() { return Empty() || Nl(); }

  bool OptPst() { return Empty() || Pst(); }

  bool OptSimpleStmt() { return Empty() || SimpleStmt() }

  bool Pas() { return OptPst() || OptPst() && PaStats() && OptPst(); }

  bool PaPat() { return Pattern(); }

  bool PaStat() {
    return PaPat() || PaPat() && LBrace() && StmtList() && Token(TK_RBrace) ||
           PaPat() && Token(TK_Comma) && OptNl && PaPat() ||
           PaPat() && Token(TK_Comma) && OptNl() && PaPat() && LBrace() &&
               StmtList() && Token(TK_RBrace) LBrace() && StmtList() &&
               Token(TK_RBrace) ||
           XBEGIN() && LBrace() && StmtList() && RBrace() ||
           XEND() && LBrace() && StmtList() && RBrace() FUNC() && FuncName() &&
               Token(TK_LParen) && VarList() && RParen() && LBrace() &&
               StmtList() && Token(TK_RBrace);
  }

  bool PaStats() { return PaStat() || PaStats() && OptPst() && PaStat(); }

  bool PatList() { return Pattern() || PatList() && Comma() && Pattern(); }

  bool PPattern() {
    return Var() && ASGNOP() && PPattern() ||
           PPattern() && Token(TK_Question) && PPattern() && Token(TK_Colon) &&
               PPattern() /* prec ? */
           || PPattern() && Bor() && PPattern() /* prec BOR */ ||
           PPattern() && And() && PPattern() /* prec AND */ ||
           PPattern() && MATCHOP() && RegExpr() ||
           PPattern() && MATCHOP() && PPattern() ||
           PPattern() && IN() && VarName() ||
           Token(TK_LParen) && PList() && Token(TK_RParen) && IN() &&
               VarName() ||
           PPattern() && Term() /* prec CAT */ || Re() || Term();
  }

  bool Pattern() {
    return Var() && ASGNOP() && Pattern() ||
           Pattern() && Token(TK_Question) && Pattern() && Token(TK_Colon) &&
               Pattern() /* prec ? */
           || Bor() && Pattern() /* prec BOR */ ||
           And() && Pattern() /* prec AND */ ||
           Pattern() && EQ() && Pattern() || Pattern() && GE() && Pattern() ||
           Pattern() && GT() && Pattern() || Pattern() && LE() && Pattern() ||
           Pattern() && LT() && Pattern() || Pattern() && NE() && Pattern() ||
           Pattern() && MATCHOP() && RegExpr() ||
           Pattern() && MATCHOP() && Pattern() Pattern() && IN() && VarName() ||
           Token(TK_LParen) && PList() && Token(TK_RParen) && IN() &&
               VarName() ||
           Pattern() && TokenType(TK_Pipe) && GETLINE() && Var() ||
           Pattern() && TokenType(TK_Pipe) && GETLINE() ||
           Pattern() && Term() /* prec CAT */
           || Re() || Term();
  }

  bool PList() {
    return Pattern() && Comma() && Pattern() || PList() && Comma() && Pattern();
  }

  bool PPList() { return PPattern() || PPList() && Comma() && PPattern(); }

  bool PrArg() {
    return Empty() || PPList() ||
           Token(TK_LParen) && PPList() && Token(TK_RParen);
  }

  bool Print() { return PRINT() || PRINTF(); }

  bool Pst() {
    return NL() || Token(TK_Semi) || Pst() && NL() || Pst() && Token(TK_Semi);
  }

  bool RBrace() { return Token(TK_RBrace) || RBrace() && NL(); }

  bool Re() { return RegExpr() || NOT() && Re(); }

  bool RegExpr() { return Token(TK_Slash) && REGEXPR() && Token(TK_Slash); }

  bool RParen() { return Token(TK_RParen) || RParen() && NL(); }

  bool SimpleStmt() {
    return Print() && PrArg() && Token(TK_Pipe) && Term() ||
           Print() && PrArg() && APPEND() && Term() ||
           Print() && PrArg() && GT() && Term() || Print() && PrArg() ||
           DELETE() && VarName() && Token(TK_LSquare) && PatList() &&
               Token(TK_RSQuare) ||
           Pattern() || Error();
  }

  bool St() { return Nl() || Token(TK_Semi) && OptNl(); }

  bool Stmt() {
    return BREAK() && St() || CONTINUE() && St() ||
           Do() && Stmt() && WHILE() && Token(TK_LParen) && Pattern() &&
               Token(TK_RParen) && St() ||
           EXIT() && Pattern() && St() || EXIT() && St() || For() ||
           If() && Stmt() && Else() && Stmt() ||
           If() && Stmt() LBrace() && StmtList() && RBrace() ||
           NEXT() && St() || NEXTFILE() && St() ||
           RETURN() && Pattern() && St() || SimpleStmt() && St() ||
           While() && Stmt() || Token(TK_Semi) && Stmt();
  }

  bool StmtList() { return Stmt() || StmtList() && Stmt(); }

  bool SubOp() { return SUB() || GSUB(); }

  bool String() { return STRING() || String() && STRING(); }

  bool Term() {
    return Term() && Token(TK_Slash) && ASGNOP() && Term() ||
           Term() && Token(TK_Plus) && Term() ||
           Term() && Token(TK_Minus) && Term() ||
           Term() && Token(TK_Star) && Term() ||
           Term() && Token(TK_Slash) && Term() ||
           Term() && Token(TK_Percent) && Term() ||
           Term() && POWER() && Term() ||
           Token(TK_Minus) && Term() /* prec UMINUS */ ||
           Token(TK_Plus) && Term() /* prec UMINUS */ ||
           NOT() && Term() && /* prec UMINUS */ ||
           BLTIN() && Token(TK_LParen) && Token(TK_RParen) || BLTIN() ||
           CALL() && Token(TK_LParen) && Token(TK_LParen) ||
           CALL() && Token(TK_LParen) && PatList() &&
               Token(TK_LParen) CLOSE() && Term() ||
           DECR() && Term() || DECR() && Var() || INCR() && Var() ||
           Var() && DECR() ||
           Var() && INCR() GETLINE() && Var() && LT() && Term() ||
           GETLINE() && LT() && Term() || GETLINE() && Var() || GETLINE() ||
           INDEX() && Token(TK_LParen) && Pattern() && Comma() && Pattern() &&
               Token(TK_RParen) ||
           INDEX() && Token(TK_LParen) && Pattern() && Comma() && RegExpr() &&
               Token(TK_RParen) ||
           Token(TK_LParen) && Pattern() && Token(TK_RParen) ||
           MATCHFCN() && Token(TK_LParen) && Pattern() && Comma() &&
               RegExpr() && Token(TK_RParen) ||
           MATCHFCN() && Token(TK_LParen) && Pattern() && Comma() &&
               Pattern() && Token(TK_RParen) ||
           NUMBER() ||
           SPLIT() && Token(TK_LParen) && Pattern() && Comma() && VarName() &&
               Comma() && Patter() && Token(TK_RParen) ||

           SPLIT() && Token(TK_LParen) && Pattern() && Comma() && VarName() &&
               Comma() && RegExpr() && Token(TK_RParen) ||
           SPLIT() && Token(TK_LParen) && Pattern() && Comma() && VarName() &&
               Token(TK_RParen) ||
           SPRINTF() && Token(TK_LParen) && PatList() && Token(TK_RParen) ||
           String() ||
           SubOp() && Token(TK_RParen) && RegExpr() && Comma() && Pattern() &&
               Token(TK_RParen) ||
           SubOp() && Token(TK_RParen) && Pattern() && Comma() && Pattern() &&
               Token(TK_RParen) ||
           SubOp() && Token(TK_RParen) && RegExpr() && Comma() && Pattern() &&
               Comma() && Var() && Token(TK_RParen) ||
           SubOp() && Token(TK_RParen) && Pattern() && Comma() && Pattern() &&
               Comma() && Var() && Token(TK_RParen) SUBSTR() &&
               Token(TK_LParen) && Pattern() && Comma() && Pattern() &&
               Comma() && Pattern() && Token(TK_RParen) ||
           SUBSTR() && Token(TK_LParen) && Pattern() && Comma() && Pattern() &&
               Token(TK_RParen) ||
           Var();
  }

  bool Var() {
    return VarName() ||
           VarName() && Token(TK_LSquare) && PatList() && Token(TK_RSquare) ||
           IVAR() || INDIRECT() && TERM();
  }

  bool VarList() { return empty() || VAR() || VarList() && Comma() && Var(); }

  bool VarName() { return VAR() || ARG() || VARNF(); }

  bool While() { return WHILE() && Token(TK_LParen) && Pattern() && RParen(); }
};
