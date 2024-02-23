//===- ast.h - AST node definitions ---------------------------------------===//
//
//  This file defines and impelements all of the AST interfaces.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "token.h"

#include <iostream>
#include <memory>
#include <ranges>

namespace cawk {
struct expr {
  virtual void operator()(std::ostream &) const {};
};

struct binary_expr final : public expr {
  token op_{};
  std::unique_ptr<expr> lhs_{}, rhs_{};

  binary_expr(token op, std::unique_ptr<expr> lhs, std::unique_ptr<expr> rhs)
      : op_{op}, lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  virtual void operator()(std::ostream &os) const override final {
    lhs_->operator()(os);
    os << op_.lexeme_;
    rhs_->operator()(os);
  }
};

struct unary_expr final : public expr {
  token op_{};
  std::unique_ptr<expr> rhs_{};

  unary_expr(token op, std::unique_ptr<expr> rhs)
      : op_{op}, rhs_{std::move(rhs)} {}

  virtual void operator()(std::ostream &os) const override final {
    os << op_.lexeme_;
    rhs_->operator()(os);
  }
};

struct atom_expr final : public expr {
  token atom_{};

  atom_expr(token atom) : atom_{atom} {}

  virtual void operator()(std::ostream &os) const override final {
    os << atom_.lexeme_;
  }
};

struct stmt {
  virtual void operator()(std::ostream &) const {};
};

struct var_decl final : public stmt {
  token type_{};
  token iden_{};
  std::unique_ptr<expr> init_{};

  var_decl(token type, token iden, std::unique_ptr<expr> init)
      : type_{type}, iden_{iden}, init_{std::move(init)} {
    type_.lexeme_ = "cawk_val";
  }

  var_decl(token iden, std::unique_ptr<expr> init)
      : iden_{iden}, init_{std::move(init)} {
    type_.lexeme_ = "cawk_val";
  }

  virtual void operator()(std::ostream &os) const override final {
    os << type_.lexeme_ << ' ';
    os << iden_.lexeme_;
    os << '=';
    init_->operator()(os);
    os << ';';
  }
};

struct fn_decl final : public stmt {
  std::string iden_{};
  std::unique_ptr<stmt> body_{};
  std::vector<std::string> params_{};
  bool ret_{};

  fn_decl(std::string iden, std::unique_ptr<stmt> body,
          std::vector<std::string> params = {}, bool ret = false)
      : iden_{iden}, body_{std::move(body)}, params_{params}, ret_{ret} {}

  virtual void operator()(std::ostream &os) const override final {
    static bool proto_{true};
    os << (ret_ ? "cawk_val " : "void ");
    os << iden_;

    if (std::empty(params_))
      os << "()";
    else {
      for (os << '('; auto &&x : params_)
        os << "cawk_val " << x << ',';
      os << "\b)";
    }

    if (proto_)
      os << ';';
    else
      body_->operator()(os);

    proto_ = false;
  }
};

struct block_stmt final : public stmt {
  std::vector<std::unique_ptr<stmt>> body_{};

  virtual void operator()(std::ostream &os) const override final {
    os << '{';
    for (auto &&x : body_)
      x->operator()(os);
    os << '}';
  }
};

struct expr_stmt final : public stmt {
  std::unique_ptr<expr> e_{};

  expr_stmt(std::unique_ptr<expr> e) : e_{std::move(e)} {}

  virtual void operator()(std::ostream &os) const override final {
    e_->operator()(os);
    os << ';';
  }
};

struct if_stmt final : public stmt {
  std::unique_ptr<expr> cond_{};
  std::unique_ptr<stmt> then_{};
  std::unique_ptr<stmt> else_{};

  if_stmt(std::unique_ptr<expr> c, std::unique_ptr<stmt> t,
          std::unique_ptr<stmt> e)
      : cond_{std::move(c)}, then_{std::move(t)}, else_{std::move(e)} {}

  virtual void operator()(std::ostream &os) const override final {
    os << "if(";
    cond_->operator()(os);
    os << ')';
    then_->operator()(os);

    if (else_ != nullptr) {
      std::cout << "else";
      else_->operator()(os);
    }
  }
};

struct for_stmt final : public stmt {
  std::unique_ptr<stmt> init_{};
  std::unique_ptr<expr> cond_{};
  std::unique_ptr<expr> incr_{};
  std::unique_ptr<stmt> body_{};

  for_stmt(std::unique_ptr<stmt> init, std::unique_ptr<expr> cond,
           std::unique_ptr<expr> incr, std::unique_ptr<stmt> body)
      : init_{std::move(init)}, cond_{std::move(cond)}, incr_{std::move(incr)},
        body_{std::move(body)} {}

  virtual void operator()(std::ostream &os) const override final {
    os << "for(";
    if (init_ == nullptr)
      os << ';';
    else
      init_->operator()(os);
    if (cond_ != nullptr)
      cond_->operator()(os);
    os << ';';
    if (incr_ != nullptr)
      incr_->operator()(os);
    os << ')';
    if (body_ != nullptr)
      body_->operator()(os);
    else
      os << ';';
  }
};

struct range_stmt final : public stmt {
  std::unique_ptr<stmt> init_{};
  std::unique_ptr<expr> range_{};
  std::unique_ptr<stmt> body_{};

  range_stmt(std::unique_ptr<stmt> init, std::unique_ptr<expr> range,
             std::unique_ptr<stmt> body)
      : init_{std::move(init)}, range_{std::move(range)},
        body_{std::move(body)} {}

  virtual void operator()(std::ostream &os) const override final {
    os << "for(";
    init_->operator()(os);
    os << ':';
    range_->operator()(os);
    os << ')';
    if (body_ != nullptr)
      body_->operator()(os);
    else
      os << ';';
  }
};

} // namespace cawk