#pragma once

#include <cstdint>
#include <vector>

#include "Exec/IO.h"

namespace cawk {
namespace inst {
enum InstKind : unsigned {
#define INST(ID, SP) ID,
#include "IR/Instruction.def"
};

std::string_view getInstructionName(InstKind Kind) {
  switch (Kind) {
#define INST(ID, SP)                                                           \
  case ID:                                                                     \
    return #SP;
#include "IR/Instruction.def"
#undef INST
  }
}
} // namespace inst
} // namespace cawk