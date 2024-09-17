#pragma once

namespace cawk {

struct Object;
struct StringObject;

enum ValueTy { NullVal, NumberVal, ObjectVal };

struct Value {
  ValueTy Type;
  union {
    double Number;
    Object *Obj;
  } As;

  Value() : Type(NullVal) {}
  Value(double Number) : Type(NumberVal) { As.Number = Number; }
  Value(Object *Obj) : Type(ObjectVal) { As.Obj = Obj; }

  bool is(ValueTy Ty) const { return Type == Ty; }

  bool operator==(const Value &RHS) const {
    if (Type != RHS.Type)
      return false;
    switch (Type) {
    default:
      return false;
    case NullVal:
      return true;
    case NumberVal:
      return As.Number == RHS.As.Number;
    }
  }
};
} // namespace cawk