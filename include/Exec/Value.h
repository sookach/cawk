#pragma once

#include "Support/Sequence.h"
#include <unordered_map>

namespace cawk {

class Value {
public:
  enum ValueKind { VK_Number, VK_String, VK_Array, VK_Vector };

private:
  const ValueKind Kind;

protected:
  Value(ValueKind Kind) : Kind(Kind) {}

  static Value *Create(ValueKind Kind) { return new Value(Kind); }

public:
  ValueKind GetKind() const { return Kind; }
};

template <typename T, Value::ValueKind V> class Type : public Value {
private:
  T Data;

protected:
  Type(T Data) : Value(V), Data(Data) {}
  Type() : Value(V), Data({}) {}

public:
  static Type *Create(T Data) { return new Type(Data); }
  static Type *CreateEmpty() { return new Type; }
};

using Number = Type<double, Value::VK_Number>;
using String = Type<std::string, Value::VK_Number>;
using Array = Type<std::unordered_map<Value *, Value *>, Value::VK_Array>;
using Vector = Type<Sequence<Value *>, Value::VK_Vector>;

} // namespace cawk
