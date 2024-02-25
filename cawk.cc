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

  out << "inline  void init__() noexcept {";

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