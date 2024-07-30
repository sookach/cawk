#include "Exec/IO.h"
#include "Frontend/Instance.h"

#include <cassert>
#include <filesystem>

int cawk_main(int Argc, char **Argv) {
  assert(Argc == 2);
  auto Source = [Argv]() {
    if (std::filesystem::exists(Argv[1])) {
      cawk::InputFile Source(Argv[1]);
      return Source.toString();
    }
    return std::string(Argv[1]);
  }();

  cawk::Instance Cawk(Source);
  return Cawk.execute();
}