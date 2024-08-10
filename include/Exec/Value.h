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
  struct Scalar {
    TypeKind Type;
    std::variant<double, std::string> Raw;
  };
  using Array = std::unordered_map<std::string, Scalar>;

private:
  TypeKind Type;

  std::variant<Scalar, Array> Raw;

public:
  TypeKind getType() { return Type; }

  template <TypeKind T> auto get() {
    if constexpr (T == NumberTy)
      return std::get<double>(std::get<Scalar>(Raw).Raw);
    if constexpr (T == StringTy)
      return std::get<std::string>(std::get<Scalar>(Raw).Raw);
    if constexpr (T == ArrayTy)
      return std::get<Array>(Raw);
  }

  template <TypeKind T> auto getAs() {
    if constexpr (T == NumberTy) {
      if (Type == NumberTy)
        return std::get<double>(std::get<Scalar>(Raw).Raw);

      if (Type == StringTy)
        return std::strtod(
            std::get<std::string>(std::get<Scalar>(Raw).Raw).c_str(), nullptr);

      assert(is<NullTy>());
      return 0.0;
    }

    if constexpr (T == StringTy) {
      if (Type == NumberTy)
        return std::to_string(std::get<double>(std::get<Scalar>(Raw).Raw));

      if (Type == StringTy)
        return std::get<std::string>(std::get<Scalar>(Raw).Raw);

      assert(is<NullTy>());
      return std::string();
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
      assert(V.is<NullTy>());
      Type = NullTy;
      Raw = Scalar(NumberTy, 0.0);
    }
  }
};
} // namespace cawk