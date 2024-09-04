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
      : Argc(argc), Argv(argv), ARGV({Argv[0]}), Diags(Diags) {}

  bool parse() {
    int I = 1;
    for (; I != Argc && Argv[I][0] == '-' && Argv[I][1] != '\0';) {
      if (std::strcmp(Argv[I], "-version") == 0 ||
          std::strcmp(Argv[I], "--version") == 0) {
        outs().print("awk 1.0\n");
        return false;
      }

      if (std::strcmp(Argv[I], "--") == 0) {
        // explicit end of args
        ++I;
        break;
      }

      if (std::strcmp(Argv[I], "--csv") == 0) {
        // turn on csv input processing
        ++I;
        continue;
      }

      switch (Argv[I][1]) {
      case 's':
        if (std::strcmp(Argv[I], "-safe") == 0)
          Assignments["safe"] = "true";
        break;
      case 'f':
        // next argument is program filename.
        assert(I + 1 != Argc && "no program filename");
        Progfiles.emplace_back(Argv[I + 1]);
        I += 2;
        break;
      case 'F':
        // set field separator.
        assert(I + 1 != Argc && "no field separator");
        Assignments["FS"] = Argv[I + 1];
        I += 2;
        break;
      case 'v':
        assert(I + 1 != Argc && "no variable name");
        assert(std::ranges::contains(Argv[I + 1], Argv[I + 2], '=') &&
               "awk: invalid -v option argument");
        Assignments[std::string(Argv[I + 1],
                                std::find(Argv[I + 1], Argv[I + 2], '='))] =
            std::string(std::find(Argv[I + 1], Argv[I + 2], '='), Argv[I + 2]);
        I += 2;
        break;
      case 'd':
        Assignments["dbg"] = std::to_string(std::atoi(Argv[I] + 2));
        if (Assignments["dbg"] == "0")
          Assignments["dbg"] = "1";
        outs().print("awk 1.0\n");
        ++I;
        break;
      default:
        outs().printf("unknown option %s ignored", Argv[I]);
        ++I;
      }
    }

    if (I == Argc) {
      if (!std::empty(Progfiles))
        return true;
      errs().printf("awk: no program filename\n");
      return false;
    }

    SourceArg = I++;

    for (; I != Argc;)
      ARGV.emplace_back(Argv[I++]);

    return true;
  }

  int getArgc() const { return Argc; }
  char **getArgv() const { return Argv; }
  std::vector<std::string> getARGV() const { return ARGV; }
  int getSourceArg() const { return SourceArg; }
  std::vector<std::string> getProfFiles() const { return Progfiles; }
};

} // namespace cawk