#pragma once

#include "Basic/Diagnostic.h"

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
  int SourceArg = -1;
  Diagnostic &Diags;

public:
  CommandLine(int argc, char **argv, Diagnostic &Diags)
      : Argc(argc), Argv(argv), Diags(Diags) {}

  bool parse() {
    ARGV.emplace_back(Argv[0]);
    for (int I = 1; I != Argc;) {
      switch (Argv[I][0]) {
      default:
        ARGV.emplace_back(Argv[I++]);
        break;
      case '-':
        if (std::strcmp(Argv[I], "-F") == 0) {
          assert(I + 1 != Argc && "missing argument to '-F'");
          Assignments["FS"] = Argv[I + 1];
          I += 2;
        } else if (std::strcmp(Argv[I], "-f") == 0) {
          assert(I + 1 != Argc && "missing argument to '-f'");
          Progfiles.emplace_back(Argv[I + 1]);
          I += 2;
        } else if (std::strcmp(Argv[I], "-v") == 0) {
          assert(I + 1 != Argc && "missing argument to '-v'");
          assert(std::ranges::contains(Argv[I + 1], Argv[I + 2], '=') &&
                 "awk: invalid -v option argument");
          Assignments[std::string(Argv[I + 1],
                                  std::find(Argv[I + 1], Argv[I + 2], '='))] =
              std::string(std::find(Argv[I + 1], Argv[I + 2], '='),
                          Argv[I + 2]);
          I += 2;
        } else {
          ARGV.emplace_back(Argv[I++]);
        }
        break;
      case '\'':
      case '"':
        if (!std::empty(Progfiles) && SourceArg == -1)
          SourceArg = I;
        else
          ARGV.emplace_back(Argv[I++]);
      }
    }
    return true;
  }

  int getArgc() const { return Argc; }
  char **getArgv() const { return Argv; }
  int getSourceArg() const { return SourceArg; }
  std::vector<std::string> getProfFiles() const { return Progfiles; }
};

} // namespace cawk