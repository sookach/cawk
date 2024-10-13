#pragma once

#include "Basic/Diagnostic.h"
#include "Frontend/CommandLine.h"

#include <string_view>

namespace cawk {
class Instance {
  Diagnostic Diags;
  CommandLine CmdLine;

public:
  Instance(int Argc, char **Argv) : CmdLine(Argc, Argv, Diags) {}

  int execute();
};
} // namespace cawk