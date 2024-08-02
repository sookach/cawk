#pragma once

#include <cassert>
#include <cstdlib>
#include <string>
#include <unordered_map>

namespace cawk {

class Scalar {
public:
  enum TypeKind { Null, Number, String };

private:
  std::variant<double, std::string> Value;
  auto Type = Null;

public:
  template <TypeKind T> auto getAs() {
    if constexpr (T == Number) {
      if (Type == Null)
        return 0;
      if (Type == Number)
        return std::get<double>(Value);
      assert(Type == String);
      char *C;
      return std::strtod(std::get<std::string>(Value).c_str(), &C);
    } else {
      assert(T == String);
      if (Type == Null)
        return "";
      if (Type == Number)
        return std::to_string(std::get<double>(Value));
      assert(Type == String);
      char *C;
      return std::get<std::string>(Value);
    }
  }

  template <TypeKind T> auto get() {
    if constexpr (T == Number) {
      return std::get<double>(Value);
    } else {
      assert(T == String);
      return std::get<std::string>(Value);
    }
  }

  void setValue(double V) {
    Value = V;
    Type = Number;
  }

  void setValue(std::string V) {
    Value = V;
    Type = String;
  }

  bool isTrue() {
    if (Type == Null)
      return false;

    if (Type == Number)
      return Value != 0;

    assert(Type == String);
    return !std::empty(Value);
  }

  void is(TypeKind T) { return Type == T; }
};

class Array {
  std::unordered_map<std::size_t, Scalar> Data;

  std::size_t hash(Scalar X) {
    if (X.is(Scalar::Null))
      return 0;
    if (X.is(Scalar::Number))
      return std::hash<double>()(X.get<Scalar::Number>());
    assert(X.is(Scalar::String));
    return std::hash<std::string>()(X.get<Scalar::String>());
  }

public:
  Scalar getValue(auto &&...X) { return Data[(hash(X) ^ ...)]; }
};
} // namespace cawk