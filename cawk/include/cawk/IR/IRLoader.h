#pragma once

#include "cawk/Exec/IO.h"
#include "cawk/IR/Instruction.h"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace cawk {
class IRLoader {
  std::vector<std::uint8_t> Code;
  std::vector<Value> Constants;
  std::unordered_map<std::string, decltype(Code)::size_type> Functions;

  void load(std::string Source) {
    std::stringstream Stream(Source);
    std::string Line;
    bool InsideFunction = false;

    for (; std::getline(Stream, Line);) {
      std::vector<std::string> Tokens = split(Line);
      if (std::empty(Tokens))
        continue;

      if (Tokens.front() == "define") {
        InsideFunction = true;
        assert(std::size(Tokens) == 3);
        assert(Tokens.back() == '{');
        if (Functions.contains(Tokens[1])) {
          errs().printf("Function %s already defined\n", Tokens[1]).flush();
          std::exit(1);
        }
        Functions[Tokens[1]] = std::size(Code);
        continue;
      }

      if (Tokens.front() == "}") {
        InsideFunction = false;
        continue;
      }
    }
  }

  std::vector<std::string> split(std::string_view S, char D = ' ') {
    std::vector<std::string> V;
    for (auto I = std::cbegin(S), E = std::cend(S); I != E;) {
      I = std::find_if(I, E, [D](char C) { return C != D; });
      if (I == E)
        break;
      auto J = std::find(I, E, D);
      V.emplace_back(I, J);
      if (J == E)
        break;
      I = J + 1;
    }
    return V;
  }
};
} // namespace cawk