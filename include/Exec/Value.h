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

    Scalar() { Type = NullTy; }

    Scalar(double D) {
      Type = NumberTy;
      Raw = D;
    }

    Scalar(std::string S) {
      Type = StringTy;
      Raw = S;
    }
  };
  using Array = std::unordered_map<std::string, Value *>;

private:
  TypeKind Type;

  std::variant<Scalar, Array> Raw;

public:
  explicit Value() : Type(NullTy), Raw(Scalar()) {}

  explicit Value(TypeKind Type) : Type(Type), Raw(Array()) {}

  explicit Value(double Raw) : Type(NumberTy), Raw(Raw) {}

  explicit Value(std::string Raw) : Type(StringTy), Raw(Raw) {}

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

  void setValue(Scalar S) {
    if (S.Type == NumberTy) {
      Type = NumberTy;
    } else if (S.Type == StringTy) {
      Type = StringTy;
    } else {
      assert(S.Type == NullTy);
      Type = NullTy;
    }
    Raw = std::variant<Scalar, Array>(S);
  }

  void setValue(Value V) { setValue(V.getValue()); }

  Scalar getValue() {
    assert(Type == NumberTy || Type == StringTy || Type == NullTy);
    return std::get<Scalar>(Raw);
  }

  Value *operator[](Value Key) {
    if (Type == NullTy) {
      Raw = Array();
      Type = ArrayTy;
    }
    assert(Type == ArrayTy);
    if (!std::get<Array>(Raw).contains(Key.getAs<StringTy>()))
      std::get<Array>(Raw)[Key.getAs<StringTy>()] = new Value();
    return std::get<Array>(Raw)[Key.getAs<StringTy>()];
  }
};
} // namespace cawk