#pragma once

#include "Support/Support.h"

namespace cawk {

class CompoundStmt;

class FunctionSymbol {
  std::string Name;
  std::vector<std::string> Params;
  const CompoundStmt *Body;

public:
  FunctionSymbol() {
    cawk_unreachable("attempt to default construct FunctionSymbol");
  }

  FunctionSymbol(std::string_view Name, std::vector<std::string> Params,
                 const CompoundStmt *Body)
      : Name(std::string(Name)), Params(Params), Body(Body) {}

  std::string getName() const { return Name; }

  std::vector<std::string> getParams() const { return Params; }

  const CompoundStmt *getBody() const { return Body; }
};
} // namespace cawk