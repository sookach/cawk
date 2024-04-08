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

llvm::cl::opt<std::string> compiler{"c++",
                                    llvm::cl::desc{"Use <compiler> to compile"},
                                    llvm::cl::value_desc{"compiler"}};

llvm::cl::opt<bool> emit_cc{"emit-cc", llvm::cl::desc{"Generate C++ code"}};

int main(int argc, char **argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv);

  if (std::empty(output_filename))
    output_filename = "main";

  if (std::empty(compiler))
    compiler = "clang++";

  std::ofstream os{(output_filename + ".cc").c_str()};

  cawk::code_gen(cawk::parser{cawk::lexer{input_filename}()}());
  cawk::code_gen(os);

  os.close();

  const auto exit_code{std::system((compiler + " " + output_filename +
                                    ".cc -std=gnu++2c -o" + output_filename)
                                       .c_str())};

  if (!emit_cc)
    std::system(("rm " + output_filename + ".cc").c_str());

  exit(exit_code);
}