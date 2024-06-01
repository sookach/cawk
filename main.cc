enum struct TokenKind {
  TK_LSquare,
  TK_RSquare,
  TK_LParen,
  TK_RParen,
  TK_LBrace,
  TK_RBrace,
  TK_Dot,
  TK_Comma,
  TK_Exclaim,
  TK_Tilde,
  TK_ExclaimTilde,
  TK_Equal,
  TK_EqualEqual,
  TK_ExclaimEqual,
  TK_Less,
  TK_LessEqual,
  TK_Greater,
  TK_GreaterGreater,
  TK_GreaterEqual,
  TK_Plus,
  TK_PlusPlus,
  TK_PlusEqual,
  TK_Minus,
  TK_MinusMinus,
  TK_MinusEqual,
  TK_Star,
  TK_StarStar,
  TK_StarEqual,
  TK_Slash,
  TK_Caret,
  TK_CaretEqual,
  TK_Percent,
  TK_PercentEqual,
  TK_Amp,
  TK_AmpAmp,
  TK_Pipe,
  TK_PipePipe,
  TK_Semi,
  TK_Question,
  TK_Colon,
  TK_Dollar,
  TK_NewLine,

  // keywords
  TK_Begin,
  TK_Break,
  TK_Close,
  TK_Continue,
  TK_Do,
  TK_Delete,
  TK_Else,
  TK_End,
  TK_Exit,
  TK_For,
  TK_Func,
  TK_Function,
  TK_Getline,
  TK_Gsub,
  TK_In,
  TK_Index,
  TK_If,
  TK_Match,
  TK_Next,
  TK_Nextfile,
  TK_Print,
  TK_Printf,
  TK_Split,
  TK_Sprintf,
  TK_Sub,
  TK_Substr,
  TK_Switch,
  TK_Return,
  TK_While,

  // Tokens
  TK_BuiltIn,
  TK_Identifier,
  TK_NumericLiteral,
  TK_RegexLiteral,
  TK_StringLiteral,
};

class Parser {
  using enum TokenKind;

  bool Token(TokenKind) { return true; }

  bool Error() { return false; }

  bool Empty() { return true; }

  bool Program() { return Pas() || Error(); }

  bool And() { return Token(TK_AmpAmp) || And() && Nl(); }

  bool Bor() { return Token(TK_PipePipe) || Bor() && Nl(); }

  bool Comma() { return Token(TK_Comma) || Comma() && Nl(); }

  bool Do() { return Token(TK_Do) || Do() && Nl(); }

  bool Else() { return Token(TK_Else) || Else() && Nl(); }

  bool For() {
    return Token(TK_For) && Token(TK_LParen) && OptSimpleStmt() &&
               Token(TK_Semi) && OptNl() && Pattern() && Token(TK_Semi) &&
               OptNl() && OptSimpleStmt() && RParen() && Stmt() ||
           Token(TK_For) && Token(TK_LParen) && OptSimpleStmt() &&
               Token(TK_Semi) && Token(TK_Semi) && OptNl() && OptSimpleStmt() &&
               RParen() && Stmt() ||
           Token(TK_For) && Token(TK_LParen) && VarName() && Token(TK_In) &&
               VarName() && RParen() && Stmt();
  }

  bool FuncName() { return Token(TK_Identifier) /* || CALL() */; }

  bool If() {
    return Token(TK_If) && Token(TK_LParen) && Pattern() && RParen();
  }

  bool LBrace() { return Token(TK_LBrace) || LBrace() && Token(TK_NewLine); }

  bool Nl() { return Token(TK_NewLine) || Nl() && Token(TK_NewLine); }

  bool OptNl() { return Empty() || Nl(); }

  bool OptPst() { return Empty() || Pst(); }

  bool OptSimpleStmt() { return Empty() || SimpleStmt(); }

  bool Pas() { return OptPst() || OptPst() && PaStats() && OptPst(); }

  bool PaPat() { return Pattern(); }

  bool PaStat() {
    return PaPat() || PaPat() && LBrace() && StmtList() && Token(TK_RBrace) ||
           PaPat() && Token(TK_Comma) && OptNl() && PaPat() ||
           PaPat() && Token(TK_Comma) && OptNl() && PaPat() && LBrace() &&
               StmtList() && Token(TK_RBrace) ||
           LBrace() && StmtList() && Token(TK_RBrace) ||
           Token(TK_Begin) && LBrace() && StmtList() && RBrace() ||
           Token(TK_End) && LBrace() && StmtList() && RBrace() ||
           (Token(TK_Function) || Token(TK_Func)) && FuncName() &&
               Token(TK_LParen) && VarList() && RParen() && LBrace() &&
               StmtList() && Token(TK_RBrace);
  }

  bool PaStats() { return PaStat() || PaStats() && OptPst() && PaStat(); }

  bool PatList() { return Pattern() || PatList() && Comma() && Pattern(); }

  bool ASGNOP() {
    return Token(TK_Equal) || Token(TK_PlusEqual) || Token(TK_MinusEqual) ||
           Token(TK_StarEqual) || Token(TK_CaretEqual) ||
           Token(TK_PercentEqual);
  }

  bool MATCHOP() { return Token(TK_Tilde) || Token(TK_ExclaimTilde); }

  bool PPattern() {
    return Var() && ASGNOP() && PPattern() ||
           PPattern() && Token(TK_Question) && PPattern() && Token(TK_Colon) &&
               PPattern() /* prec ? */
           || PPattern() && Bor() && PPattern() /* prec BOR */ ||
           PPattern() && And() && PPattern() /* prec AND */ ||
           PPattern() && MATCHOP() && RegExpr() ||
           PPattern() && MATCHOP() && PPattern() ||
           PPattern() && Token(TK_In) && VarName() ||
           Token(TK_LParen) && PList() && Token(TK_RParen) && Token(TK_In) &&
               VarName() ||
           PPattern() && Term() /* prec CAT */ || Re() || Term();
  }

  bool Pattern() {
    return Var() && ASGNOP() && Pattern() ||
           Pattern() && Token(TK_Question) && Pattern() && Token(TK_Colon) &&
               Pattern() /* prec ? */
           || Bor() && Pattern() /* prec BOR */ ||
           And() && Pattern() /* prec AND */ ||
           Pattern() && Token(TK_EqualEqual) && Pattern() ||
           Pattern() && Token(TK_GreaterEqual) && Pattern() ||
           Pattern() && Token(TK_Greater) && Pattern() ||
           Pattern() && Token(TK_LessEqual) && Pattern() ||
           Pattern() && Token(TK_Less) && Pattern() ||
           Pattern() && Token(TK_ExclaimEqual) && Pattern() ||
           Pattern() && MATCHOP() && RegExpr() ||
           Pattern() && MATCHOP() && Pattern() ||
           Pattern() && Token(TK_In) && VarName() ||
           Token(TK_LParen) && PList() && Token(TK_RParen) && Token(TK_In) &&
               VarName() ||
           Pattern() && Token(TK_Pipe) && Token(TK_Getline) && Var() ||
           Pattern() && Token(TK_Pipe) && Token(TK_Getline) ||
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

  bool Print() { return Token(TK_Print) || Token(TK_Printf); }

  bool Pst() {
    return Token(TK_NewLine) || Token(TK_Semi) || Pst() && Token(TK_NewLine) ||
           Pst() && Token(TK_Semi);
  }

  bool RBrace() { return Token(TK_RBrace) || RBrace() && Token(TK_NewLine); }

  bool Re() { return RegExpr() || Token(TK_Exclaim) && Re(); }

  bool RegExpr() {
    return Token(TK_Slash) && Token(TK_RegexLiteral) && Token(TK_Slash);
  }

  bool RParen() { return Token(TK_RParen) || RParen() && Token(TK_NewLine); }

  bool SimpleStmt() {
    return Print() && PrArg() && Token(TK_Pipe) && Term() ||
           Print() && PrArg() && Token(TK_GreaterGreater) && Term() ||
           Print() && PrArg() && Token(TK_Greater) && Term() ||
           Print() && PrArg() ||
           Token(TK_Delete) && VarName() && Token(TK_LSquare) && PatList() &&
               Token(TK_RSquare) ||
           Pattern() || Error();
  }

  bool St() { return Nl() || Token(TK_Semi) && OptNl(); }

  bool Stmt() {
    return Token(TK_Break) && St() || Token(TK_Continue) && St() ||
           Do() && Stmt() && Token(TK_While) && Token(TK_LParen) && Pattern() &&
               Token(TK_RParen) && St() ||
           Token(TK_Exit) && Pattern() && St() || Token(TK_Exit) && St() ||
           For() || If() && Stmt() && Else() && Stmt() ||
           If() && Stmt() && LBrace() && StmtList() && RBrace() ||
           Token(TK_Next) && St() || Token(TK_Nextfile) && St() ||
           Token(TK_Return) && Pattern() && St() || SimpleStmt() && St() ||
           While() && Stmt() || Token(TK_Semi) && Stmt();
  }

  bool StmtList() { return Stmt() || StmtList() && Stmt(); }

  bool SubOp() { return Token(TK_Sub) || Token(TK_Gsub); }

  bool String() {
    return Token(TK_StringLiteral) || String() && Token(TK_StringLiteral);
  }

  bool POWER() { return Token(TK_StarStar) || Token(TK_Caret); }

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
           Token(TK_Exclaim) && Term() /* prec UMINUS */ ||
           Token(TK_BuiltIn) && Token(TK_LParen) && Token(TK_RParen) ||
           Token(TK_BuiltIn) ||
           Token(TK_Identifier) && Token(TK_LParen) && Token(TK_LParen) ||
           Token(TK_Identifier) && Token(TK_LParen) && PatList() &&
               Token(TK_LParen) ||
           Token(TK_Close) && Term() || Token(TK_MinusMinus) && Term() ||
           Token(TK_PlusPlus) && Var() || Var() && Token(TK_MinusMinus) ||
           Var() && Token(TK_PlusPlus) ||
           Token(TK_Getline) && Var() && Token(TK_Less) && Term() ||
           Token(TK_Getline) && Token(TK_Greater) && Term() ||
           Token(TK_Getline) && Var() || Token(TK_Getline) ||
           Token(TK_Index) && Token(TK_LParen) && Pattern() && Comma() &&
               Pattern() && Token(TK_RParen) ||
           Token(TK_Index) && Token(TK_LParen) && Pattern() && Comma() &&
               RegExpr() && Token(TK_RParen) ||
           Token(TK_LParen) && Pattern() && Token(TK_RParen) ||
           Token(TK_Match) && Token(TK_LParen) && Pattern() && Comma() &&
               RegExpr() && Token(TK_RParen) ||
           Token(TK_Match) && Token(TK_LParen) && Pattern() && Comma() &&
               Pattern() && Token(TK_RParen) ||
           Token(TK_NumericLiteral) ||
           Token(TK_Split) && Token(TK_LParen) && Pattern() && Comma() &&
               VarName() && Comma() && Pattern() && Token(TK_RParen) ||
           Token(TK_Split) && Token(TK_LParen) && Pattern() && Comma() &&
               VarName() && Comma() && RegExpr() && Token(TK_RParen) ||
           Token(TK_Split) && Token(TK_LParen) && Pattern() && Comma() &&
               VarName() && Token(TK_RParen) ||
           Token(TK_Sprintf) && Token(TK_LParen) && PatList() &&
               Token(TK_RParen) ||
           String() ||
           SubOp() && Token(TK_RParen) && RegExpr() && Comma() && Pattern() &&
               Token(TK_RParen) ||
           SubOp() && Token(TK_RParen) && Pattern() && Comma() && Pattern() &&
               Token(TK_RParen) ||
           SubOp() && Token(TK_RParen) && RegExpr() && Comma() && Pattern() &&
               Comma() && Var() && Token(TK_RParen) ||
           SubOp() && Token(TK_RParen) && Pattern() && Comma() && Pattern() &&
               Comma() && Var() && Token(TK_RParen) ||
           Token(TK_Substr) && Token(TK_LParen) && Pattern() && Comma() &&
               Pattern() && Comma() && Pattern() && Token(TK_RParen) ||
           Token(TK_Substr) && Token(TK_LParen) && Pattern() && Comma() &&
               Pattern() && Token(TK_RParen) ||
           Var();
  }

  bool Var() {
    return VarName() ||
           VarName() && Token(TK_LSquare) && PatList() && Token(TK_RSquare);
  }

  bool VarList() {
    return Empty() || Token(TK_Identifier) || VarList() && Comma() && Var();
  }

  bool VarName() { return Token(TK_Identifier); }

  bool While() {
    return Token(TK_While) && Token(TK_LParen) && Pattern() && RParen();
  }
};
