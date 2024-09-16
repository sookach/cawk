#pragma once

#include "IR/Instruction.h"
#include "IR/Value.h"

#include <array>
#include <cassert>
#include <cstdint>
#include <vector>

namespace cawk {
class ExecutionEngine {
  std::vector<std::uint8_t> Code;
  decltype(Code)::iterator PC;
  std::vector<Value> Stack;
  std::array<Value, std::numeric_limits<std::uint8_t>::max()> Constants;

  int run() {
    auto NextInst = [this](int Incr = 1) {
      return static_cast<inst::InstKind>(*std::exchange(PC, PC + Incr));
    };
    for (inst::InstKind Inst;;) {
      switch (Inst = NextInst()) {
      case inst::Add: {
        assert(std::size(Stack) >= 2);
        Value RHS = Stack.back();
        Stack.pop_back();
        assert(Stack.back().is(ValueTy::NumberVal));
        assert(RHS.is(ValueTy::NumberVal));
        Stack.back().As.Number += RHS.As.Number;
        break;
      }
      case inst::Cmp: {
        assert(std::size(Stack) >= 2);
        Value RHS = Stack.back();
        Stack.pop_back();
        assert(Stack.back().is(ValueTy::NumberVal));
        assert(RHS.is(ValueTy::NumberVal));
        Stack.back().As.Number = Stack.back().As.Number - RHS.As.Number;
        break;
      }
      case inst::Const: {
        assert(PC != std::end(Code));
        Stack.push_back(NextInst());
        break;
      }
      case inst::Div: {
        assert(std::size(Stack) >= 2);
        Value RHS = Stack.back();
        Stack.pop_back();
        assert(Stack.back().is(ValueTy::NumberVal));
        assert(RHS.is(ValueTy::NumberVal));
        Stack.back().As.Number /= RHS.As.Number;
        break;
      }
      case inst::Mul: {
        assert(std::size(Stack) >= 2);
        Value RHS = Stack.back();
        Stack.pop_back();
        assert(Stack.back().is(ValueTy::NumberVal));
        assert(RHS.is(ValueTy::NumberVal));
        Stack.back().As.Number *= RHS.As.Number;
        break;
      }
      case inst::Neg: {
        assert(!std::empty(Stack));
        assert(Stack.back().is(ValueTy::NumberVal));
        Stack.back().As.Number = -Stack.back().As.Number;
        break;
      }
      case inst::Not: {
        assert(!std::empty(Stack));
        assert(Stack.back().is(ValueTy::NumberVal));
        Stack.back().As.Number = !Stack.back().As.Number;
        break;
      }
      case inst::Ret: {
        assert(!std::empty(Stack));
        auto Result = Stack.back();
        Stack.pop_back();
        return Result.As.Number;
      }
      case inst::Sub: {
        assert(std::size(Stack) >= 2);
        Value RHS = Stack.back();
        Stack.pop_back();
        assert(Stack.back().is(ValueTy::NumberVal));
        assert(RHS.is(ValueTy::NumberVal));
        Stack.back().As.Number -= RHS.As.Number;
        break;
      }
      }
    }
  }

  void dumpCode() {
    for (auto I = std::cbegin(Code), E = std::cend(Code); I != E;) {
      auto Inst = static_cast<inst::InstKind>(*I++);
      outs().printf("%04d %s", inst::getInstructionName(Inst).data()).flush();
      if (Inst == inst::Const)
        outs().printf(" %f\n", Constants[*I++].As.Number).flush();
      else
        outs().printf("\n").flush();
    }
  }
};
} // namespace cawk