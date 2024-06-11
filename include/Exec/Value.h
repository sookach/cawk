#pragma once

#include "Support/Sequence.h"
#include "Support/Support.h"
#include <cassert>
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

template <typename T, typename P, Value::ValueKind V>
class ValueType : public P {
private:
  T Data;

protected:
  ValueType(T Data) : P(V), Data(Data) {}
  ValueType() : P(V), Data({}) {}

public:
  static bool classof(auto &&X) { return X->GetKind() == V; }

  static ValueType *Create(T Data) { return new ValueType(Data); }
  static ValueType *CreateEmpty() { return new ValueType; }

  const T &GetData() const { return Data; }
  void SetData(T D) { Data = D; }
};

using Number = ValueType<double, Primitive, Value::VK_Number>;
using String = ValueType<std::string, Primitive, Value::VK_Number>;
using Array =
    ValueType<std::unordered_map<Value *, Value *>, Composite, Value::VK_Array>;
using Vector = ValueType<Sequence<Value *>, Composite, Value::VK_Vector>;

template <typename T> T *value_cast(Value *V) {
  if (isa<T>(V))
    return static_cast<T *>(V);

  if (!isa<Number>(V) && !isa<String>(V)) {
    assert(false && "invalid conversion source");
    return nullptr;
  }

  if constexpr (std::is_same_v<T, Number>)
    return Number::Create(
        ToFloat(static_cast<String *>(V)->GetData()).value_or(0));
  else if (std::is_same_v<T, String>)
    return String::Create(std::to_string(static_cast<Number *>(V)->GetData()));
  else
    static_assert(false && "invalid conversion target");
}

template <typename T> T raw_cast(Value *V) {
  if constexpr (std::is_same_v<T, bool>) {
    switch (V->GetKind()) {
    default:
      assert(false && "invalid raw cast target");
      std::terminate();
    case Value::VK_String:
      return !std::empty(static_cast<String *>(V)->GetData());
    case Value::VK_Number:
      return static_cast<Number *>(V)->GetData();
    }
  } else if constexpr (std::is_same_v<T, double>) {
    switch (V->GetKind()) {
    default:
      assert(false && "invalid raw cast target");
      std::terminate();
    case Value::VK_String: {
      char *Ptr;
      auto X = std::strtod(static_cast<String *>(V)->GetData().data(), &Ptr);
      return Ptr == static_cast<String *>(V)->GetData().data() ? 0 : X;
    }
    case Value::VK_Number:
      return static_cast<Number *>(V)->GetData();
    }
  } else if constexpr (std::is_same_v<T, std::string>) {
    switch (V->GetKind()) {
    default:
      assert(false && "invalid raw cast target");
      std::terminate();
    case Value::VK_String:
      return static_cast<String *>(V)->GetData();
    case Value::VK_Number:
      return std::to_string(static_cast<Number *>(V)->GetData());
    }
  }
}

} // namespace cawk
