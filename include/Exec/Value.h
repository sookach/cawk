#pragma once

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <variant>

namespace cawk {

enum TypeKind { NullTy, NumberTy, StringTy, ArrayTy };

class Value {
public:
  struct Scalar;
  using Array = std::unordered_map<std::string, Scalar>;

private:
  TypeKind Type;

  struct Scalar {
    TypeKind Type;
    std::variant<double, std::string> Raw;
  };

  std::variant<Scalar, Array> Raw;

public:
  TypeKind getType() { return Type; }

  template <TypeKind T> auto get() {
    if constexpr (T == NumberTy)
      return std::get<double>(std::get<Scalar>(Raw).Raw);
    if constexpr (T == StringTy)
      return std::get<std::string>(std::get<Scalar>(Raw).Raw);
    if constexpr (T == Array)
      return std::get<ArrayTy>(Raw);
  }

  template <TypeKind T> auto getAs() {
    if constexpr (T == NumberTy) {
      if (Type == NumberTy)
        return std::get<double>(std::get<Scalar>(Raw).Raw);

      if (Type == StringTy)
        return std::strtod(std::get<std::string>(std::get<Scalar>(Raw).Raw),
                           nullptr);

      assert(is<NullTy>());
      return 0;
    }

    if constexpr (T == StringTy) {
      if (Type == NumberTy)
        return std::to_string(std::get<double>(std::get<Scalar>(Raw).Raw));

      if (Type == StringTy)
        return std::get<std::string>(std::get<Scalar>(Raw).Raw);

      assert(is<NullTy>());
      return "";
    }

    if constexpr (T == ArrayTy)
      static_assert("use get<Array>() instead");
  }

  template <TypeKind T> bool is() { return Type == T; }

  void setValue(Value V) {
    if (V.is<NumberTy>()) {
      Type = NumberTy;
      Raw = Scalar(NumberTy, V.get<NumberTy>());
    } else if (V.is<StringTy>()) {
      Type = StringTy;
      Raw = Scalar(StringTy, V.get<StringTy>());
    } else {
      assert(V.is<Null>());
      Type = NullTy;
      Raw = Scalar(NumberTy, 0);
    }
  }
};
} // namespace cawk