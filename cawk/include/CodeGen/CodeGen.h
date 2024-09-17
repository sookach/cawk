#pragma once

#include "AST/AST.h"
#include "IR/Instruction.h"
#include "IR/Value.h"
#include "Support/Support.h"

#include <array>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace cawk {

namespace detail {
auto HashValue = [](const Value &V) {
  if (V.is(NumberVal))
    return std::hash<double>{}(V.As.Number);
  cawk_fatal("Invalid value type");
};
} // namespace detail

class CodeGen {
  std::vector<std::uint8_t> Code;
  std::array<Value, std::numeric_limits<std::uint8_t>::max()> ConstantPool;
  std::uint8_t ConstantIndex = 0;
  std::unordered_map<Value, std::uint8_t, decltype(detail::HashValue)>
      ConstantMap;

  void emitCompoundStatement(CompoundStmt *C) {
    for (Stmt *S : C->getBody()) {
      switch (S->getKind()) {
      default:
        cawk_fatal("Invalid statement type");
      case Stmt::SK_Value:
        emitValueStatement(ptr_cast<ValueStmt>(S));
      }
    }
  }

  void emitStatement(Stmt *S) {
    switch (S->getKind()) {
    default:
      cawk_fatal("Invalid statement type");
    case Stmt::SK_Compound:
      emitCompoundStatement(ptr_cast<CompoundStmt>(S));
      break;
    case Stmt::SK_Return:
      emitReturnStatement(ptr_cast<ReturnStmt>(S));
      break;
    }
  }

  void emitReturnStatement(ReturnStmt *R) {
    if (R->getValue() != nullptr)
      emitExpr(R->getValue());
    emitInstruction(inst::Ret);
  }

  void emitValueStatement(ValueStmt *V) {
    emitExpr(V->getValue());
    emitInstruction(inst::Pop);
  }

  void emitExpr(Expr *E) {
    switch (E->getKind()) {
    default:
      cawk_fatal("Invalid expression type");
    case Expr::EK_FloatingLiteral:
      emitFloatingLiteral(ptr_cast<FloatingLiteral>(E));
    }
  }

  void emitFloatingLiteral(FloatingLiteral *F) {
    emitConstant(Value(std::stod(std::string(F->getLiteralData()))));
  }

  template <typename... Ts>
    requires(std::is_same_v<Ts, std::uint8_t> && ...)
  void emitInstruction(inst::InstKind Inst, Ts... Args) {
    Code.push_back(Inst);
    (Code.push_back(Args), ...);
  }

  void emitConstant(Value V) {
    if (!ConstantMap.contains(V)) {
      ConstantPool[ConstantIndex] = V;
      ConstantMap[V] = ConstantIndex;
      ++ConstantIndex;
    }
    emitInstruction(inst::Push, ConstantMap[V]);
  }
};
} // namespace cawk