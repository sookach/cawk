#include "lexer.h"
#include "parser.h"
#include "llvm/Support/CommandLine.h"

#include <concepts>
#include <functional>
#include <sysexits.h>

llvm::cl::opt<std::string> input_filename{
    llvm::cl::Positional, llvm::cl::desc{"<file>"}, llvm::cl::Required};

llvm::cl::opt<std::string> output_filename{
    "o", llvm::cl::desc{"Write output to <file>"},
    llvm::cl::value_desc{"file"}};

int main(int argc, char **argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv);

  cawk::lexer l{input_filename};
  std::ofstream out{std::empty(output_filename) ? "main.cc"
                                                : output_filename.c_str()};
  out << std::ifstream{"include/cawk.h"}.rdbuf();

  out << std::endl << std::endl;

  auto tree{cawk::parser{l()}()};
  for (auto &&x : tree)
    if (dynamic_cast<cawk::fn_decl *>(x.get()) != nullptr ||
        dynamic_cast<cawk::var_decl *>(x.get()) != nullptr)
      x->operator()(out);

  out << std::endl << std::endl;

  for (auto &&x : tree)
    if (dynamic_cast<cawk::fn_decl *>(x.get()) != nullptr)
      x->operator()(out);

  out << std::endl << std::endl;

  out << "inline void cawk::init__() noexcept {";

  out << "run_begin__ = [&]() noexcept -> void {";
  for (auto &&x : tree)
    if (dynamic_cast<cawk::pattern_action_decl *>(x.get()) != nullptr &&
        dynamic_cast<cawk::pattern_action_decl *>(x.get())->pos_ ==
            cawk::pattern_action_decl::type::begin)
      x->operator()(out);
  out << "};";

  out << "run_mid__ = [&]() noexcept -> void {";
  for (auto &&x : tree)
    if (dynamic_cast<cawk::pattern_action_decl *>(x.get()) != nullptr &&
        dynamic_cast<cawk::pattern_action_decl *>(x.get())->pos_ ==
            cawk::pattern_action_decl::type::mid)
      x->operator()(out);
  out << "};";

  out << "run_end__ = [&]() noexcept -> void {";
  for (auto &&x : tree)
    if (dynamic_cast<cawk::pattern_action_decl *>(x.get()) != nullptr &&
        dynamic_cast<cawk::pattern_action_decl *>(x.get())->pos_ ==
            cawk::pattern_action_decl::type::end)
      x->operator()(out);
  out << "};";

  out << '}';

  out.close();

  std::system("clang-format -style=llvm -i main.cc");
}