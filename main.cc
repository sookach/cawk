enum struct TokenType {
  LSquare,
  RSquare,
  LParen,
  RParen,
  LBrace,
  RBrace,
  Dot,
  Comma,
  Bang,
  Tilde,
  Equal,
  EqualEqual,
  BangEqual,
  Less,
  LessEqual,
  Greater,
  GreaterEqual,
  Plus,
  Minus,
  Slash,
  Star,
  Caret,
  Amp,
  AmpAmp,
  Pipe,
  PipePipe,
  Semi,
  Question,
  Colon,
  Dollar,

  // keywords
  For,
  While,
  In,
  If,
  Function,
  Switch,
  Getline,
  Print,
  BEGIN,
  END,

  // Tokens
  Identifier,
  StringLiteral,
  RegexLiteral,
};

class Parser {
  using enum TokenType;

  bool Token(TokenType) { return true; }

  bool Program() { PAS(); }

  bool Empty() {}

  bool PST() {}

  bool OptPST() { return Empty() || PST(); }

  bool PAPat() {}

  bool OptNL() {}

  bool StmtList() {}

  bool VarList() {}

  bool PAStat() {
    return PAPat() || PAPat() && Token(LBrace) && StmtList() && Token(RBrace) ||
           PAPat() && Token(Comma) && OptNL() && PAPat() ||
           PAPat() && Token(Comma) && OptNL() && PAPat() && Token(LBrace) &&
               StmtList() && Token(RBrace) ||
           Token(LBrace) && StmtList() && Token(RBrace) ||
           Token(BEGIN) && Token(LBrace) && StmtList() && Token(RBrace) ||
           Token(END) && Token(LBrace) && StmtList() && Token(RBrace) ||
           Token(Function) && Token(Identifier) && Token(LParen) && VarList() &&
               Token(RParen) && Token(LBrace) && StmtList() && Token(RBrace);
  }

  bool PAStats() { return PAStat() || PAStats() && OptPST() && PAStat(); }

  bool PatList() { return Pattern() || PatList() && Token(Comma) && Pattern(); }

  bool PPattern() {
    return Token(Identifier) && Token(Equal) && PPattern() ||
           PPattern() && Token(Question) && PPattern() && Token(Colon) &&
               PPattern()                                 /* %prec '?' */
           || PPattern() && Token(PipePipe) && PPattern() /* %prec BOR */
           || PPattern() && Token(AmpAmp) && PPattern()   /* %prec AND */
           || PPattern() && Token(Tilde) && Token(RegexLiteral) ||
           PPattern() && Token(Tilde) && PPattern() ||
           PPattern() && Token(In) && Token(Identifier) ||
           Token(LParen) && PList() && Token(RParen) && Token(In) &&
               Token(Identifier) ||
           PPattern() && Term() /* %prec CAT */
           || RE() || Term();
  }

  bool Pattern() {
    return Token(Identifier) && Token(Equal) && Pattern() ||
           Pattern() && Token(Question) && Pattern() && Token(Colon) &&
               Pattern() /* %prec */ && Token(Question) ||
           Pattern() && Bor() && Pattern() /* %prec BOR */ ||
           Pattern() && And() && Pattern() /* %prec AND */ ||
           Pattern() && Token(Equal) && Pattern() ||
           Pattern() && Token(GreaterEqual) && Pattern() ||
           Pattern() && Token(Greater) && Pattern() ||
           Pattern() && Token(LessEqual) && Pattern() ||
           Pattern() && Token(Less) && Pattern() ||
           Pattern() && Token(BangEqual) && Pattern() ||
           Pattern() && Token(Tilde) && Token(RegexLiteral) ||
           Pattern() && Token(Tilde) && Pattern() ||
           Pattern() && Token(In) && Token(Identifier) ||
           Token(LParen) && PList() && Token(RParen) && Token(In) &&
               Token(Identifier) ||
           Pattern() && Token(Pipe) && Token(Getline) && Token(Identifier) ||
           Pattern() && Token(Pipe) && Token(Getline) ||
           Pattern() && Term() /* %prec CAT */
           || RE() || Term();
  }

  bool PList() {
    return Pattern() && Token(Comma) && Pattern() ||
           PList() && Token(Comma) && Pattern();
  }

  bool PPList() { return PPattern() || PPList() && Token(Comma) && PPattern(); }

#if 0
pplist:
	  ppattern
	| pplist comma ppattern		{ $$ = linkum($1, $3); }
	;
#endif
};
