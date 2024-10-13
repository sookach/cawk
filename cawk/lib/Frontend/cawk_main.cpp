#include "cawk/Exec/IO.h"
#include "cawk/Frontend/Instance.h"

#include <cassert>
#include <filesystem>

int cawk_main(int Argc, char **Argv) {
  cawk::Instance Cawk(Argc, Argv);
  return Cawk.execute();
}