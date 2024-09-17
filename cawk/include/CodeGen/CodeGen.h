#pragma once

#include "AST/AST.h"
#include "IR/Instruction.h"
#include "IR/Object.h"
#include "IR/Value.h"
#include "Support/Support.h"

#include <array>
#include <cstdint>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

namespace cawk {

namespace detail {
auto HashValue = [](const Value &V) {
  if (V.is(NumberVal))
    return std::hash<double>()(V.As.Number);
  if (V.is(ObjectVal) && V.As.Obj->is(StringObj))
    return std::hash<std::string>()(
        reinterpret_cast<StringObject *>(V.As.Obj)->String);
  cawk_fatal("Invalid value type");
};
} // namespace detail

class CodeGen {
  std::vector<std::uint8_t> Code;
  std::array<Value, std::numeric_limits<std::uint8_t>::max()> ConstantPool;
  std::uint8_t ConstantIndex = 0;
  std::unordered_map<double, std::uint8_t> NumberMap;
  std::unordered_map<std::string, std::uint8_t> StringMap;

  std::pair<decltype(Code), decltype(ConstantPool)>
  emitByteCode(TranslationUnitDecl *T) {
    Code.clear();
    ConstantIndex = 0;
    NumberMap.clear();
    StringMap.clear();

    for (Decl *D : T->getDecls()) {
      switch (D->getKind()) {
      default:
        cawk_fatal("Invalid declaration type");
      case Decl::DK_Function:
        // emitFunctionDecl(ptr_cast<FunctionDecl>(D));
        break;
      }
    }

    return {Code, ConstantPool};
  }

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

  void emitBinaryOperator(BinaryOperator *B) {
    emitExpr(B->getLHS());
    emitExpr(B->getRHS());
    switch (B->getOpcode().getKind()) {
    default:
      cawk_fatal("Invalid binary operator");
    case tok::plus:
      emitInstruction(inst::Add);
      break;
    case tok::minus:
      emitInstruction(inst::Sub);
      break;
    case tok::star:
      emitInstruction(inst::Mul);
      break;
    case tok::slash:
      emitInstruction(inst::Div);
      break;
    case tok::percent:
      emitInstruction(inst::Rem);
      break;
    case tok::starstar:
    case tok::caret:
      emitInstruction(inst::Pow);
      break;
    case tok::ampamp:
      emitInstruction(inst::And);
      break;
    case tok::pipepipe:
      emitInstruction(inst::Or);
      break;
    case tok::equalequal:
      emitInstruction(inst::Eq);
      break;
    case tok::exclaimequal:
      emitInstruction(inst::Ne);
      break;
    case tok::greater:
      emitInstruction(inst::Gt);
      break;
    case tok::greaterequal:
      emitInstruction(inst::Ge);
      break;
    case tok::less:
      emitInstruction(inst::Lt);
      break;
    case tok::lessequal:
      emitInstruction(inst::Le);
      break;
    }
  }

  void emitFloatingLiteral(FloatingLiteral *F) {
    emitNumberConstant(std::stod(std::string(F->getLiteralData())));
  }

  void emitStringLiteral(StringLiteral *S) {
    emitStringConstant(std::string(S->getLiteralData()));
  }

  void emitUnaryOperator(UnaryOperator *U) {
    emitExpr(U->getSubExpr());
    switch (U->getOpcode().getKind()) {
    default:
      cawk_fatal("Invalid unary operator");
    case tok::minus:
      emitInstruction(inst::Neg);
      break;
    case tok::exclaim:
      emitInstruction(inst::Not);
      break;
    }
  }

  template <typename... Ts>
    requires(std::is_same_v<Ts, std::uint8_t> && ...)
  void emitInstruction(inst::InstKind Inst, Ts... Args) {
    Code.push_back(Inst);
    (Code.push_back(Args), ...);
  }

  void emitNumberConstant(double D) {
    if (!NumberMap.contains(D)) {
      ConstantPool[ConstantIndex] = Value(D);
      NumberMap[D] = ConstantIndex++;
    }
    emitInstruction(inst::Push, NumberMap[D]);
  }

  void emitStringConstant(std::string S) {
    if (!StringMap.contains(S)) {
      ConstantPool[ConstantIndex] = StringObject::Create(S);
      StringMap[S] = ConstantIndex++;
    }
    emitInstruction(inst::Push, StringMap[S]);
  }
};
} // namespace cawk