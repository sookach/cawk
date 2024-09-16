#pragma once

namespace cawk {
enum ValueTy { NullVal, NumberVal };

struct Value {
  ValueTy Type;
  union {
    double Number;
  } As;

  Value() : Type(NullVal) {}
  Value(double Number) : Type(NumberVal) { As.Number = Number; }

  bool is(ValueTy Ty) const { return Type == Ty; }
};
} // namespace cawk