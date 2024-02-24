#include "lexer.h"
#include "parser.h"

#include <concepts>
#include <functional>

int main() {
  // // g(f, 4);
  cawk::lexer l{"main.cawk"};
  std::ofstream out{"main.cc"};
  out << std::ifstream{"cawk.h"}.rdbuf();

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

  out << "struct {";
  out << "void operator()() const {";
  for (auto &&x : tree)
    if (dynamic_cast<cawk::if_stmt *>(x.get()) != nullptr)
      x->operator()(out);
  out << "}";
  out << "} run__;\n\n";

  out << "struct {";
  out << "bool operator()(std::istream &is__) const {";
  out << "fields__.clear();";
  out << "NF = 0;";
  out << "fields__.emplace_back();";
  out << "if (!std::getline(is__, fields__.back()))";
  out << "return false;";
  out << "std::stringstream ss__{fields__.back()};";
  out << "for (std::string s__; ss__ >> s__; ++NF)";
  out << "fields__.push_back(std::move(s__));";
  out << "++NR;";
  out << "return true;";
  out << "}";
  out << "} read_line__;\n\n";

  out << "struct {";
  out << "void operator()(std::istream &is__) const {";
  out << "run__();";
  out << "BEGIN = false;";
  out << "for (; read_line__(is__);)";
  out << "run__();";
  out << "END = true;";
  out << "run__();";
  out << "}";
  out << "} run_all__;\n\n";

  out << "int main(int argc, char** argv) {";
  out << "if (argc == 2) {";
  out << "std::ifstream in__{argv[1]};";
  out << "run_all__(in__);";
  out << "} else {";
  out << "END = true;";
  out << "run__();";
  out << "}";
  out << "}\n";

  out.flush();
  out.close();

  std::system("clang-format -style=llvm -i main.cc");
}