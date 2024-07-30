#pragma once

#include "Support/Support.h"

namespace cawk {

class CompoundStmt;

class FunctionSymbol {
  std::vector<std::string> Params;
  const CompoundStmt *Body;

public:
  FunctionSymbol() {
    cawk_unreachable("attempt to default construct FunctionSymbol");
  }

  FunctionSymbol(std::vector<std::string> Params, const CompoundStmt *Body)
      : Params(Params), Body(Body) {}

  std::vector<std::string> getParams() const { return Params; }

  const CompoundStmt *getBody() const { return Body; }
};
} // namespace cawk