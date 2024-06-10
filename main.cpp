#include "Lexer/Lexer.h"
#include "Parse/Parser.h"
#include "Support/SourceSpan.h"

int main(int argc, char **argv) {
  SourceSpan Sources;
  Sources.AddSource(argv[1]);
  cawk::Lexer Lex(Sources.GetSource(argv[1]));
  cawk::Parser Parse(Lex);
  Parse.Parse();
}
