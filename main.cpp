#include "Lexer/Lexer.h"
#include "Parse/Parser.h"

int main(int argc, char **argv) {
  cawk::Lexer Lex(argv[1]);
  cawk::Parser Parse(Lex);
  Parse.Parse();
}
