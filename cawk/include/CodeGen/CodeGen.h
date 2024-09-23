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
  std::vector<std::unordered_map<std::string, std::uint8_t>> SymbolTable;
  std::uint8_t SymbolCount = 0;

  std::pair<decltype(Code), decltype(ConstantPool)>
  emitByteCode(TranslationUnitDecl *T) {
    SymbolTable.resize(1);

    for (Decl *D : T->getDecls())
      if (FunctionDecl *F = dyn_cast<FunctionDecl>(D))
        ; // emitFunctionDecl(ptr_cast<FunctionDecl>(D));

    for (Decl *D : T->getDecls())
      if (RuleDecl *R = dyn_cast<RuleDecl>(D))
        ; // emitRuleDecl(R);

    return {Code, ConstantPool};
  }

  void emitRuleDecl(RuleDecl *R) {
    emitExpr(R->getPattern());
    auto BranchAddr = std::size(Code);
    emitInstruction(inst::Br, std::uint8_t(), std::uint8_t());
    emitCompoundStatement(R->getAction());
    auto Offset = std::size(Code) - BranchAddr;
    Code[BranchAddr + 1] = (Offset >> 8) & 0xff;
    Code[BranchAddr + 2] = Offset & 0xff;
  }

  void emitStatement(Stmt *S) {
    switch (S->getKind()) {
    default:
      cawk_fatal("Invalid statement type");
    case Stmt::SK_Compound:
      emitCompoundStatement(ptr_cast<CompoundStmt>(S));
      break;
    case Stmt::SK_If:
      // emitIfStatement(ptr_cast<IfStmt>(S));
      break;
    case Stmt::SK_Return:
      emitReturnStatement(ptr_cast<ReturnStmt>(S));
      break;
    case Stmt::SK_Value:
      emitValueStatement(ptr_cast<ValueStmt>(S));
    }
  }

  void emitCompoundStatement(CompoundStmt *C) {
    for (Stmt *S : C->getBody())
      emitStatement(S);
  }

  void emitIfStatement(IfStmt *I) {
    emitExpr(I->getCond());
    auto BranchAddr = std::size(Code);
    emitInstruction(inst::Br, std::uint8_t(), std::uint8_t());
    emitStatement(I->getThen());
    auto Offset = std::size(Code) - BranchAddr;
    int JumpAddr = 0;
    if (I->getElse() != nullptr) {
      JumpAddr = std::size(Code);
      emitInstruction(inst::Jmp, std::uint8_t(), std::uint8_t());
    }
    Code[BranchAddr + 1] = Offset & 0xff;
    Code[BranchAddr + 2] = (Offset >> 8) & 0xff;
    if (I->getElse() != nullptr) {
      emitStatement(I->getElse());
      auto Offset = std::size(Code) - JumpAddr;
      Code[JumpAddr + 1] = Offset & 0xff;
      Code[JumpAddr + 2] = (Offset >> 8) & 0xff;
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
    case Expr::EK_BinaryOperator:
      return emitBinaryOperator(ptr_cast<BinaryOperator>(E));
    case Expr::EK_DeclRef:
      // return emitDeclRef(ptr_cast<DeclRefExpr>(E));
    case Expr::EK_FloatingLiteral:
      return emitFloatingLiteral(ptr_cast<FloatingLiteral>(E));
    case Expr::EK_StringLiteral:
      return emitStringLiteral(ptr_cast<StringLiteral>(E));
    case Expr::EK_UnaryOperator:
      return emitUnaryOperator(ptr_cast<UnaryOperator>(E));
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
    case tok::equal:
      emitInstruction(inst::Store);
      break;
    case tok::plusequal:
      emitInstruction(inst::Inc);
      break;
    case tok::minusequal:
      emitInstruction(inst::Dec);
      break;
    case tok::starequal:
      emitInstruction(inst::Scale);
      break;
    case tok::slashequal:
      emitInstruction(inst::Frac);
      break;
    case tok::starstarequal:
    case tok::caretequal:
      emitInstruction(inst::Exp);
      break;
    case tok::percentequal:
      emitInstruction(inst::Mod);
      break;
    }
  }

  void emitDeclRefExpr(DeclRefExpr *D) {
    if (SymbolTable.back().contains(std::string(D->getName())))
      return emitInstruction(inst::Load,
                             SymbolTable.back().at(std::string(D->getName())));

    if (SymbolTable.front().contains(std::string(D->getName())))
      return emitInstruction(inst::Load,
                             SymbolTable.front().at(std::string(D->getName())));

    SymbolTable.front()[std::string(D->getName())] = SymbolCount;
    emitInstruction(inst::Load, SymbolCount++);
  }

  void emitFloatingLiteral(FloatingLiteral *F) {
    emitNumberConstant(std::stod(std::string(F->getLiteralData())));
  }

  void emitLambdaExpr(LambdaExpr *L) {
    // emitFunctionDecl(L->getFunctionDecl());
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
    ConstantPool[ConstantIndex] = Value(D);
    emitInstruction(inst::Push, ConstantIndex++);
  }

  void emitStringConstant(std::string S) {
    ConstantPool[ConstantIndex] = StringObject::Create(S);
    emitInstruction(inst::Push, ConstantIndex++);
  }
};
} // namespace cawk