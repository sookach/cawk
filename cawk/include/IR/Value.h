#pragma once

namespace cawk {
enum ValueTy { NullVal, NumberVal, ObjectVal };

struct Object;

struct Value {
  ValueTy Type;
  union {
    double Number;
    Object *Obj;
  } As;

  Value() : Type(NullVal) {}
  Value(double Number) : Type(NumberVal) { As.Number = Number; }
  Value(Object *Obj) : Type(ObjectVal) { As.Obj = Obj; }
  ~Value() {
    if (Type == ObjectVal)
      delete As.Obj;
  }

  bool is(ValueTy Ty) const { return Type == Ty; }
};
} // namespace cawk