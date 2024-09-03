#pragma once

#include <algorithm>
#include <cassert>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace cawk {
class CommandLine {
  int Argc;
  char **Argv;
  std::vector<std::string> ARGV;
  std::unordered_map<std::string, std::string> Assignments;
  std::vector<std::string> Progfiles;

public:
  CommandLine(int argc, char **argv) : Argc(argc), Argv(argv) {
    for (int I = 1; I != Argc;) {
      switch (Argv[i][0]) {
      case '-':
        if (std::strcmp(Argv[I], "-F")) {
          assert(I + 1 != Argc && "missing argument to '-F'");
          Assignments["FS"] = Argv[I + 1];
          I += 2;
        } else if (std::strcmp(Argv[I], "-f") == 0) {
          assert(I + 1 != Argc && "missing argument to '-f'");
          Progfiles.emplace_back(Argv[I + 1]);
          I += 2;
        } else if (std::strcmp(Argv[I], "-v") == 0) {
          assert(I + 1 != Argc && "missing argument to '-v'");
          assert(std::contains(Argv[I + 1], Arv[I + 2], '=') &&
                 "awk: invalid -v option argument");
          Assignments[std::string(Argv[I + 1],
                                  std::find(Argv[I + 1], Argv[I + 2], '='))] =
              std::string(std::find(Argv[I + 1], Argv[I + 2], '='),
                          Argv[I + 2]);
          I += 2;
        } else {
          ARGV.emplace_back(Argv[I++]);
        }
      }
    }
  }

  int getArgc() const { return Argc; }
  char **getArgv() const { return Argv; }
};

} // namespace cawk