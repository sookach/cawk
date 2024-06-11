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

  static Value Create(ValueKind Kind) { return Value(Kind); }

public:
  ValueKind GetKind() const { return Kind; }
};

class Primitive : public Value {
protected:
  Primitive(ValueKind Kind) : Value(Kind) {}
};

class Composite : public Value {
protected:
  Composite(ValueKind Kind) : Value(Kind) {}
};

template <typename T, typename P, Value::ValueKind V> class Type : public P {
private:
  T Data;

protected:
  Type(T Data) : P(V), Data(Data) {}
  Type() : P(V), Data({}) {}

public:
  static Type Create(T Data) { return Type(Data); }
  static Type CreateEmpty() { return Type(); }

  T GetData() const { return Data; }
  void SetData(T D) { Data = D; }
};

using Number = Type<double, Primitive, Value::VK_Number>;
using String = Type<std::string, Primitive, Value::VK_Number>;
using Array =
    Type<std::unordered_map<Primitive, Primitive>, Composite, Value::VK_Array>;
using Vector = Type<Sequence<Primitive>, Composite, Value::VK_Vector>;


} // namespace cawk
