#include "Exec/Split.h"

#include <numeric>
#include <regex>

using namespace cawk;

Value cawk::split(Value String, Value &Array, Value FieldSep) {
  int I = 0;
  auto S = String.getAs<StringTy>();
  std::regex Re(FieldSep.getAs<StringTy>());
  std::sregex_token_iterator It(std::cbegin(S), std::cend(S), Re, -1), End;

  for (; It != End; ++It)
    Array[std::to_string(++I)] = It->str();

  return I;
}

std::vector<std::string> cawk::split(std::string String, std::string Delim) {
  std::regex Re(Delim);
  std::sregex_token_iterator It(std::cbegin(String), std::cend(String), Re, -1),
      End;

  return std::accumulate(It, End, std::vector<std::string>(),
                         [](std::vector<std::string> Accum,
                            const std::sregex_token_iterator::value_type &Tok) {
                           Accum.push_back(Tok.str());
                           return Accum;
                         });
}