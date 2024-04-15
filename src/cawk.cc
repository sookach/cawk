#include "code_gen.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"

#include <concepts>
#include <functional>
#include <iostream>
#include <sysexits.h>

int main(int argc, char **argv) {
  std::vector<std::string_view> argvs{};
  std::unordered_map<std::string_view, std::string> args{};

  for (int i{1}; i != argc; ++i)
    argvs.push_back(argv[i]);

  args["-o"] = "main";
  args["-c++"] = "c++";

  for (auto i{std::cbegin(argvs)}; i != std::cend(argvs);) {
    switch (i->front()) {
    default:
      args["in"] = *i++;
      break;
    case '-':
      if (*i == "-emit-cc") {
        args["-emit-cc"] = "";
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

  const auto exit_code{std::system((args["-c++"] + " " + args["-o"] +
                                    ".cc -std=gnu++2c -o" + args["-o"].data())
                                       .c_str())};

  if (!args.contains("-emit-cc"))
    std::system(("rm " + args["-o"] + ".cc").c_str());

  exit(exit_code);
}