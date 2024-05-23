#include "code_gen.h"
#include "lexer.h"
#include "parser.h"
#include "sema/sema.h"

#include <concepts>
#include <functional>
#include <iostream>
#include <ranges>
#include <sysexits.h>

int main(int argc, char **argv) {
  std::vector<std::string_view> argvs{};
  std::unordered_map<std::string_view, std::string> args{};

  for (auto &&x : std::views::iota(1, argc))
    argvs.push_back(argv[x]);

  args["-o"] = "main";

  for (auto i{std::cbegin(argvs)}; i != std::cend(argvs);) {
    switch (i->front()) {
    default:
      args["in"] = *i++;
      break;
    case '-':
      if (*i == "-cc") {
        args["-cc"] = "";
        ++i;
      } else {
        args[*i] = *(i + 1);
        i += 2;
      }
    }
  }

  std::ofstream os{(args["-o"] + ".cc").c_str()};

  cawk::code_gen(cawk::parser{cawk::lexer{args["in"]}()}());
  cawk::code_gen(os);

  os.close();

  std::system(("clang-format -style=llvm -i " + args["-o"] + ".cc").c_str());

  if (args.contains("-cc"))
    return;

  const auto exit_code{std::system(("clang++ -stdlib=libc++ " + args["-o"] +
                                    ".cc -std=gnu++2c -o" + args["-o"].data())
                                       .c_str())};

  std::system(("rm " + args["-o"] + ".cc").c_str());

  exit(exit_code);
}