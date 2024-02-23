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
  out << "bool operator()() const {";
  for (auto &&x : tree)
    if (dynamic_cast<cawk::if_stmt *>(x.get()) != nullptr)
      x->operator()(out);
  out << "}";
  out << "} cawk_run;\n\n";

  out << "struct {";
  out << "bool operator()(std::istream &cawk_is) const {";
  out << "cawk_fields.clear();";
  out << "if (!std::getline(cawk_is, cawk_record))";
  out << "return false;";
  out << "std::stringstream cawk_ss{cawk_record};";
  out << "for (std::string cawk_s; cawk_ss >> cawk_s; "
         "cawk_fields.push_back(std::move(cawk_s)));";
  out << "return true;";
  out << "}";
  out << "} cawk_read_line;\n\n";

  out << "void cawk_run_all(std::istream &is) {";
  out << "cawk_run();";
  out << "BEGIN = false;";
  out << "for (; cawk_read_line(is);)";
  out << "cawk_run();";
  out << "END = true;";
  out << "cawk_run();";
  out << "}\n\n";

  out << "int main(int argc, char** argv) {";
  out << "std::ifstream in{argv[1]};";
  out << "cawk_run_all(in);";
  out << "}\n";

  out.flush();
  out.close();

  std::system("clang-format -style=llvm -i main.cc");
}