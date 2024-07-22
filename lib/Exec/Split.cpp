#include "Exec/Split.h"

#include <regex>

using namespace cawk;

Value cawk::split(Value String, Value &Array, Value FieldSep) {
  int I = 0;
  auto S = String.toString();
  std::regex Re(FieldSep.toString());
  std::sregex_token_iterator It(std::cbegin(S), std::cend(S), Re, -1), End;

  for (; It != End; ++It)
    Array[++I] = Value(It->str());

  return I;
}