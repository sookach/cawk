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

  TokenKind CurrTok;

  bool Token(TokenKind) { return true; }

  TokenKind Peek() { return CurrTok; }

  void NextTok() {}

  void Expect(TokenKind) {}

  bool Error() { return false; }

  bool Empty() { return true; }

  void SkipNewLine() {
    for (; Token(TK_NewLine);)
      ;
  }

  void Program() { Pas(); }

  void And() {
    Expect(TK_AmpAmp);
    SkipNewLine();
  }

  void Bor() {
    Expect(TK_PipePipe);
    SkipNewLine();
  }

  void Comma() {
    Expect(TK_Comma);
    SkipNewLine();
  }

  void Do() {
    Expect(TK_Do);
    SkipNewLine();
  }

  void Else() {
    Expect(TK_Else);
    SkipNewLine();
  }

  bool For() {
    Expect(TK_For);
    Expect(TK_LParen);

    if (Token(TK_Identifier)) {
      Expect(TK_In);
      Expect(TK_Identifier);
      RParen();
      Stmt();
    }

    if (!Token(TK_Semi)) {
      SimpleStmt();
      Expect(TK_Semi);
    }

    if (!Token(TK_Semi)) {
      SkipNewLine();
      Pattern();
      Expect(TK_Semi);
    }

    SkipNewLine();

    if (!Token(TK_RParen)) {
      SimpleStmt();
      Expect(TK_RParen);
    }

    Stmt();

    return {};
  }

  bool FuncName() { return Token(TK_Identifier) /* || CALL() */; }

  bool If() {
    Expect(TK_If);
    Expect(TK_LParen);
    Pattern();
    RParen();
    return {};
  }

  bool LBrace() { return Token(TK_LBrace) || LBrace() && Token(TK_NewLine); }

  bool Nl() { return Token(TK_NewLine) || Nl() && Token(TK_NewLine); }

  bool OptNl() { return Empty() || Nl(); }

  void Pas() {
    Pst();

    // Maybe
    PaStats();
    Pst();
  }

  bool PaPat() { return Pattern(); }

  bool PaStat() {
    switch (CurrTok) {
    default:
      break;
    case TK_LBrace:
      LBrace();
      StmtList();
      Expect(TK_RBrace);
      break;
    case TK_Begin:
      Expect(TK_Begin);
      LBrace();
      StmtList();
      Expect(TK_RBrace);
      break;
    case TK_End:
      Expect(TK_End);
      LBrace();
      StmtList();
      Expect(TK_RBrace);
      break;
    case TK_Func:
    case TK_Function:
      NextTok();
      Expect(TK_Identifier);
      Expect(TK_LParen);
      VarList();
      RParen();
      LBrace();
      StmtList();
      Expect(TK_RBrace);
    }

    Pattern();

    switch (CurrTok) {
    default:
      break;
    case TK_LBrace:
      LBrace();
      StmtList();
      Expect(TK_RBrace);
      break;
    case TK_Comma:
      Expect(TK_Comma);
      SkipNewLine();
      Pattern();
      if (CurrTok != TK_LBrace)
        break;
      LBrace();
      StmtList();
      Expect(TK_RBrace);
    }
  }

  bool PaStats() { return PaStat() || PaStats() && Pst() && PaStat(); }

  bool PatList() { return Pattern() || PatList() && Comma() && Pattern(); }

  bool ASGNOP() {
    return Token(TK_Equal) || Token(TK_PlusEqual) || Token(TK_MinusEqual) ||
           Token(TK_StarEqual) || Token(TK_CaretEqual) ||
           Token(TK_PercentEqual);
  }

  bool MATCHOP() { return Token(TK_Tilde) || Token(TK_ExclaimTilde); }

  bool PPattern() {
    switch (CurrTok) {
    default:
      break;
    case TK_Identifier:
      Expect(TK_Identifier);
      ASGNOP();
      PPattern();
      break;
    case TK_LParen:
      Expect(TK_LParen);
      PList();
      Expect(TK_RParen);
      Expect(TK_In);
      Expect(TK_Identifier);
    }

    PPattern();

    switch (CurrTok) {
    default:
      break;
    case TK_Question:
      Expect(TK_Question);
      PPattern();
      Expect(TK_Colon);
      PPattern();
      break;
    case TK_PipePipe:
      Bor();
      PPattern();
      break;
    case TK_AmpAmp:
      And();
      PPattern();
      break;
    case TK_Tilde:
    case TK_ExclaimTilde:
      NextTok();
      if (Peek() == TK_Slash) {
        RegExpr();
      } else {
        PPattern();
      }
      break;
    case TK_In:
      Expect(TK_In);
      VarName();
    case TK_LParen:
      Expect(TK_LParen);
      PList();
      Expect(TK_RParen);
      Expect(TK_In);
      VarName();
      break;
    case (TK_Exclaim):
    case (TK_Slash):
      Re();
    }

    return PPattern() && Token(TK_Question) && PPattern() && Token(TK_Colon) &&
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
           || Pattern() && Bor() && Pattern() /* prec BOR */ ||
           Pattern() && And() && Pattern() /* prec AND */ ||
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

  void PList() {
    Pattern();
    for (; Token(TK_Comma);)
      Pattern();
  }

  void PPList() {
    PPattern();
    for (; Token(TK_Comma);)
      PPattern();
  }

  void PrArg() {
    if (Token(TK_LParen)) {
      PPList();
      Expect(TK_RParen);
    } else
      PPList();
  }

  bool Print() { return Token(TK_Print) || Token(TK_Printf); }

  void Pst() {
    for (; Token(TK_NewLine) || Token(TK_Semi);)
      ;
  }

  bool RBrace() {
    if (!Token(TK_RBrace))
      return false;
    NextTok();

    for (; Token(TK_NewLine);)
      NextTok();

    return true;
  }

  bool Re() {
    for (; Token(TK_Exclaim);)
      NextTok();

    return RegExpr();
  }

  bool RegExpr() {
    return Token(TK_Slash) && Token(TK_RegexLiteral) && Token(TK_Slash);
  }

  void RParen() {
    Expect(TK_RParen);
    SkipNewLine();
  }

  void SimpleStmt() {
    switch (CurrTok) {
    default:
      Pattern();
    case TK_Print:
    case TK_Printf:
      NextTok();
      PrArg();
      switch (CurrTok) {
      default:
        break;
      case TK_Pipe:
      case TK_GreaterGreater:
      case TK_Greater:
        NextTok();
        Term();
      }
      break;
    case TK_Delete:
      NextTok();
      VarName();
      Token(TK_LSquare);
      PatList();
      Token(TK_RSquare);
    }
  }

  void St() {
    Token(TK_Semi);
    SkipNewLine();
  }

  void Stmt() {
    switch (CurrTok) {
    default:
      SimpleStmt();
      St();
      break;
    case TK_Break:
    case TK_Continue:
    case TK_Next:
    case TK_Nextfile:
      NextTok();
      St();
      break;
    case TK_Do:
      Do();
      Stmt();
      Token(TK_While);
      Token(TK_LParen);
      Pattern();
      Token(TK_RParen);
      St();
    case TK_Exit:
      NextTok();
      Pattern(); // Opt
      St();
      break;
    case TK_For:
      For();
    case TK_If:
      If();
      Stmt();
    case TK_LBrace:
      NextTok();
      StmtList();
      NextTok();
      break;
    case TK_Return:
      Pattern();
      St();
      break;
    case TK_Semi:
      SkipNewLine();
      break;
    case TK_While:
      While();
      Stmt();
    }
  }

  void StmtList() {
    for (; !Token(TK_LBrace);)
      Stmt();
  }

  bool SubOp() { return Token(TK_Sub) || Token(TK_Gsub); }

  bool String() {
    for (; Token(TK_StringLiteral);)
      ;
  }

  bool POWER() { return Token(TK_StarStar) || Token(TK_Caret); }

  void NUD() {
    switch (CurrTok) {}
  }

  int LBP() {}

  void Expr(int RBP = 0) { NUD(); }

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

  void Var() {
    Expect(TK_Identifier);
    if (Token(TK_LSquare)) {
      PatList();
      Expect(TK_RSquare);
    }
  }

  bool VarList() {
    return Empty() || Token(TK_Identifier) || VarList() && Comma() && Var();
  }

  bool VarName() { return Token(TK_Identifier); }

  bool While() {
    Expect(TK_While);
    Expect(TK_LParen);
    Pattern();
    RParen();
  }
};
