#include "code_gen.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"

#include "llvm/Support/CommandLine.h"

#include <concepts>
#include <functional>
#include <iostream>
#include <sysexits.h>

llvm::cl::opt<std::string> input_filename{
    llvm::cl::Positional, llvm::cl::desc{"<file>"}, llvm::cl::Required};

llvm::cl::opt<std::string> output_filename{
    "o", llvm::cl::desc{"Write output to <file>"},
    llvm::cl::value_desc{"file"}};

int main(int argc, char **argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv);

  std::ofstream os{std::empty(output_filename) ? "main.cc"
                                               : output_filename.c_str()};

  cawk::code_gen(cawk::parser{cawk::lexer{input_filename}()}());
  cawk::code_gen(os);

  os.close();

  std::system("clang-format -style=llvm -i main.cc");
}