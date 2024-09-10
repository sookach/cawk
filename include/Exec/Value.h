#pragma once

#include "Exec/IO.h"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <variant>

namespace cawk {

class FunctionDecl;

enum TypeKind { NullTy, NumberTy, StringTy, ArrayTy, FunctionTy };

static std::string toString(TypeKind Ty) {
  switch (Ty) {
  case NullTy:
    return "null";
  case NumberTy:
    return "number";
  case StringTy:
    return "string";
  case ArrayTy:
    return "array";
  case FunctionTy:
    return "function";
  }
}

class Value {
public:
  using Array = std::unordered_map<std::string, Value *>;

private:
  TypeKind Type;
  double NumberValue;
  std::string StringValue;
  std::unordered_map<std::string, Value *> ArrayValue;
  FunctionDecl *FunctionValue;

public:
  explicit Value() : Type(NullTy) {}

  explicit Value(TypeKind Type) : Type(Type) {}

  explicit Value(double NumberValue)
      : Type(NumberTy), NumberValue(NumberValue) {}

  explicit Value(std::string StringValue)
      : Type(StringTy), StringValue(StringValue) {}

  explicit Value(FunctionDecl *FunctionValue)
      : Type(FunctionTy), FunctionValue(FunctionValue) {}

  TypeKind getType() { return Type; }

  void setType(TypeKind Ty) { Type = Ty; }

  template <TypeKind T> auto get() {
    if constexpr (T == NumberTy)
      return NumberValue;
    if constexpr (T == StringTy)
      return StringValue;
    if constexpr (T == ArrayTy)
      return ArrayValue;
    if constexpr (T == FunctionTy)
      return FunctionValue;
  }

  template <TypeKind T> auto getAs() {
    if constexpr (T == NumberTy) {
      if (Type == NumberTy)
        return NumberValue;

      if (Type == StringTy)
        return std::strtod(StringValue.c_str(), nullptr);

      if (Type == NullTy)
        return 0.0;

      cawk_fatal("Invalid conversion from ", toString(Type), " to number");
    }

    if constexpr (T == StringTy) {
      if (Type == NumberTy) {
        auto S = std::to_string(NumberValue);
        if (std::ranges::contains(S, '.')) {
          S.erase(S.find_last_not_of('0') + 1, std::string::npos);
          if (S.back() == '.')
            S.pop_back();
        }
        return S;
      }

      if (Type == StringTy)
        return StringValue;

      if (Type == NullTy)
        return std::string();

      cawk_fatal("Invalid conversion from ", toString(Type), " to string");
    }

    if constexpr (T == ArrayTy) {
      assert(Type == ArrayTy || Type == NullTy);
      return ArrayValue;
    }

    if constexpr (T == FunctionTy) {
      assert(Type == FunctionTy || Type == NullTy);
      return FunctionValue;
    }
  }

  template <TypeKind T> bool is() { return Type == T; }

  void setValue(Value *V) { setValue(*V); }

  void setValue(Value V) {
    switch (V.getType()) {
    case NullTy:
      break;
    case NumberTy:
      assert(is<NumberTy>() || is<StringTy>() || is<NullTy>());
      NumberValue = V.get<NumberTy>();
      break;
    case StringTy:
      assert(is<NumberTy>() || is<StringTy>() || is<NullTy>());
      StringValue = V.get<StringTy>();
      break;
    case ArrayTy:
      assert(is<ArrayTy>() || is<NullTy>());
      ArrayValue = V.get<ArrayTy>();
      break;
    case FunctionTy:
      assert(is<FunctionTy>() || is<NullTy>());
      FunctionValue = V.get<FunctionTy>();
    }
    Type = V.getType();
  }

  Value *operator[](Value Key) {
    if (Type == NullTy)
      Type = ArrayTy;

    if (Type != ArrayTy)
      cawk_fatal("Invalid subscripting of ", toString(Type));

    if (!ArrayValue.contains(Key.getAs<StringTy>()))
      ArrayValue[Key.getAs<StringTy>()] = new Value;
    return ArrayValue[Key.getAs<StringTy>()];
  }
};
} // namespace cawk