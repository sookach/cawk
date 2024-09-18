#pragma once

#include "IR/Instruction.h"
#include "IR/Value.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <unordered_map>
#include <vector>

namespace cawk {
class ExecutionEngine {
  std::vector<std::uint8_t> Code;
  decltype(Code)::iterator PC;
  std::vector<Value *> Stack;
  std::array<Value, std::numeric_limits<std::uint8_t>::max()> Constants;
  std::unordered_map<std::string, Value *> Globals;

  int run() {
    auto Push = [this](Value *V) { Stack.push_back(V); };

    auto Pop = [this] {
      assert(!std::empty(Stack));
      Value *V = Stack.back();
      Stack.pop_back();
      return V;
    };

    auto NextInst = [this](int Incr = 1) {
      return static_cast<inst::InstKind>(*std::exchange(PC, PC + Incr));
    };

    auto IsScalar = [](Value *V) {
      return V->is(NullVal) || V->is(NumberVal) || V->As.Obj->is(StringObj);
    };

    auto IsString = [](Value *V) {
      return V->is(ObjectVal) && V->As.Obj->is(StringObj);
    };

    auto ToNumber = [](Value *V) {
      if (V->is(NumberVal))
        return V->As.Number;
      if (V->is(NullVal))
        return 0.0;
      if (V->is(ObjectVal) && V->As.Obj->is(StringObj))
        return obj::cast<StringObj>(V->As.Obj)->strtod();
      errs().printf("Invalid conversion to number\n").flush();
      std::exit(1);
    };

    auto ToString = [](Value *V) {
      if (V->is(NumberVal))
        return std::to_string(V->As.Number);
      if (V->is(ObjectVal) && V->As.Obj->is(StringObj))
        return obj::cast<StringObj>(V->As.Obj)->String;
      errs().printf("Invalid conversion to string\n").flush();
      std::exit(1);
    };

    auto ToBool = [](Value *V) -> bool {
      if (V->is(NumberVal))
        return V->As.Number;
      if (V->is(NullVal))
        return false;
      if (V->is(ObjectVal) && V->As.Obj->is(StringObj))
        return !std::empty(obj::cast<StringObj>(V->As.Obj)->String);
      errs().printf("Invalid conversion to boolean\n").flush();
      std::exit(1);
    };

    auto ExecBinaryArithmetic = [&, this](auto Op) {
      assert(std::size(Stack) >= 2);
      double RHS = ToNumber(Pop()), LHS = ToNumber(Pop());
      Push(new Value(Op(LHS, RHS)));
    };

    auto ExecBinaryString = [&, this](auto Op) {
      assert(std::size(Stack) >= 2);
      std::string RHS = ToString(Pop()), LHS = ToString(Pop());
      Push(new Value(StringObject::Create(Op(LHS, RHS))));
    };

    auto ExecBinaryLogical = [&, this](auto Op) {
      assert(std::size(Stack) >= 2);
      double RHS = ToBool(Pop());
      double LHS = ToBool(Pop());
      Push(new Value(Op(LHS, RHS)));
    };

    auto ExecCompare = [&, this](auto Op) {
      assert(std::size(Stack) >= 2);
      if (IsString(Stack.back()) || IsString(Stack[std::size(Stack) - 2])) {
        std::string RHS = ToString(Pop()), LHS = ToString(Pop());
        Push(new Value(Op(LHS, RHS)));
      } else {
        ExecBinaryArithmetic(Op);
      }
    };

    for (auto End = std::cend(Code); PC != End;) {
      switch (NextInst()) {
      case inst::Add:
        ExecBinaryArithmetic(std::plus());
        break;
      case inst::And:
        ExecBinaryLogical(std::logical_and());
        break;
      case inst::Con:
        ExecBinaryString(std::plus());
        break;
      case inst::Div:
        ExecBinaryArithmetic(std::divides());
        break;
      case inst::Eq:
        ExecCompare(std::equal_to());
        break;
      case inst::Ge:
        ExecCompare(std::greater_equal());
        break;
      case inst::Gt:
        ExecCompare(std::greater());
        break;
      case inst::Le:
        ExecCompare(std::less_equal());
        break;
      case inst::Load: {
        assert(PC != std::end(Code));
        assert(IsString(&Constants[*PC]));
        Value *V = Globals[ToString(&Constants[*PC++])];
        V->IsLValue = true;
        Push(V);
        break;
      }
      case inst::Lt:
        ExecCompare(std::less());
        break;
      case inst::Mul:
        ExecBinaryArithmetic(std::multiplies());
        break;
      case inst::Ne:
        ExecCompare(std::not_equal_to());
        break;
      case inst::Neg:
        assert(!std::empty(Stack));
        Push(new Value(-ToNumber(Pop())));
        break;
      case inst::Not:
        assert(!std::empty(Stack));
        Push(new Value(!ToBool(Pop())));
        break;
      case inst::Or:
        ExecBinaryLogical(std::logical_or());
        break;
      case inst::Pop:
        Pop();
        break;
      case inst::Pow:
        ExecBinaryArithmetic(
            [](double LHS, double RHS) { return std::pow(LHS, RHS); });
        break;
      case inst::Print: {
        std::uint8_t N = *PC++;
        assert(N <= std::size(Stack));
        std::reverse(std::end(Stack) - N, std::end(Stack));
        for (std::uint8_t I = 0; I != N; ++I) {
          if (I != 0)
            outs().print(" ").flush();
          outs().print(ToString(Pop())).flush();
        }
        outs().print("\n").flush();
        break;
      }
      case inst::Printf: {
        std::uint8_t N = *PC++;
        assert(N <= std::size(Stack));
        if (N == 0)
          break;

        std::vector<Value *> Args;
        for (; --N != 0;)
          Args.push_back(Pop());

        std::string Format = ToString(Pop());
        outs().printf(Format.data()).flush();
        break;
      }
      case inst::Push:
        assert(PC != std::end(Code));
        Push(&Constants[*PC++]);
        break;
      case inst::Rem:
        ExecBinaryArithmetic(
            [](double LHS, double RHS) { return std::fmod(LHS, RHS); });
        break;
      case inst::Ret:
        assert(!std::empty(Stack));
        return ToNumber(Pop());
      case inst::Store: {
        assert(std::size(Stack) >= 2);
        Value *RHS = Pop(), *LHS = Pop();
        assert(LHS->IsLValue);
        break;
      }
      case inst::Sub:
        ExecBinaryArithmetic(std::minus());
      }
    }
    return 0;
  }

  void dumpCode() {
    for (auto I = std::cbegin(Code), E = std::cend(Code); I != E;) {
      auto Inst = static_cast<inst::InstKind>(*I++);
      outs().printf("%04d %s", inst::getInstructionName(Inst).data()).flush();
      if (Inst == inst::Push)
        outs().printf(" %f\n", Constants[*I++].As.Number).flush();
      else
        outs().printf("\n").flush();
    }
  }
};
} // namespace cawk