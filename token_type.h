//===- token_type.h - cawk token types ------------------------------------===//
//
//  This file defines the different token types.
//
//===----------------------------------------------------------------------===//

#pragma once

#include <iostream>

namespace cawk {
enum struct token_type {
  // Tokens.
  unknown,
  eof,
  identifier,
  numeric_constant,
  char_constant,
  regex_literal,
  string_literal,

  // Punctuators.
  l_square,
  r_square,
  l_paren,
  r_paren,
  l_brace,
  r_brace,
  period,
  ellipsis,
  amp,
  ampamp,
  ampequal,
  star,
  starequal,
  plus,
  plusplus,
  plusequal,
  minus,
  arrow,
  minusminus,
  minusequal,
  tilde,
  exclaim,
  exclaimequal,
  exclaiml_square,
  slash,
  slashequal,
  percent,
  percentequal,
  less,
  lessless,
  lessequal,
  lesslessequal,
  spaceship,
  greater,
  greatergreater,
  greaterequal,
  greatergreaterequal,
  caret,
  caretequal,
  pipe,
  pipepipe,
  pipeequal,
  question,
  colon,
  semi,
  equal,
  equalequal,
  comma,
  hash,
  hashhash,
  hashat,
  dollar,

  // Keywords.
  kw_auto,
  kw_begin,
  kw_bool,
  kw_break,
  kw_case,
  kw_char,
  kw_const,
  kw_continue,
  kw_default,
  kw_else,
  kw_end,
  kw_enum,
  kw_exit,
  kw_extern,
  kw_f32,
  kw_f64,
  kw_false,
  kw_for,
  kw_fn,
  kw_goto,
  kw_i8,
  kw_i16,
  kw_i32,
  kw_i64,
  kw_i128,
  kw_if,
  kw_in,
  kw_print,
  kw_return,
  kw_set,
  kw_slice,
  kw_static,
  kw_string,
  kw_struct,
  kw_switch,
  kw_true,
  kw_u8,
  kw_u16,
  kw_u32,
  kw_u64,
  kw_u128,
  kw_union,
  kw_void,
};

std::ostream &operator<<(std::ostream &os, token_type t) {
  switch (t) {
  default:
    os << "undefined token";
    break;
  case token_type::unknown:
    os << "unknown";
    break;
  case token_type::eof:
    os << "eof";
    break;
  case token_type::identifier:
    os << "identifier";
    break;
  case token_type::numeric_constant:
    os << "numeric_constant";
    break;
  case token_type::char_constant:
    os << "char_constant";
    break;
  case token_type::string_literal:
    os << "string_literal";
    break;
  case token_type::l_square:
    os << "l_square";
    break;
  case token_type::r_square:
    os << "r_square";
    break;
  case token_type::l_paren:
    os << "l_paren";
    break;
  case token_type::r_paren:
    os << "r_paren";
    break;
  case token_type::l_brace:
    os << "l_brace";
    break;
  case token_type::r_brace:
    os << "r_brace";
    break;
  case token_type::period:
    os << "period";
    break;
  case token_type::ellipsis:
    os << "ellipsis";
    break;
  case token_type::amp:
    os << "amp";
    break;
  case token_type::ampamp:
    os << "ampamp";
    break;
  case token_type::ampequal:
    os << "ampequal";
    break;
  case token_type::star:
    os << "star";
    break;
  case token_type::starequal:
    os << "starequal";
    break;
  case token_type::plus:
    os << "plus";
    break;
  case token_type::plusplus:
    os << "plusplus";
    break;
  case token_type::plusequal:
    os << "plusequal";
    break;
  case token_type::minus:
    os << "minus";
    break;
  case token_type::arrow:
    os << "arrow";
    break;
  case token_type::minusminus:
    os << "minusminus";
    break;
  case token_type::minusequal:
    os << "minusequal";
    break;
  case token_type::tilde:
    os << "tilde";
    break;
  case token_type::exclaim:
    os << "exclaim";
    break;
  case token_type::exclaimequal:
    os << "exclaimequal";
    break;
  case token_type::exclaiml_square:
    os << "exclaiml_square";
    break;
  case token_type::slash:
    os << "slash";
    break;
  case token_type::slashequal:
    os << "slashequal";
    break;
  case token_type::percent:
    os << "percent";
    break;
  case token_type::percentequal:
    os << "percentequal";
    break;
  case token_type::less:
    os << "less";
    break;
  case token_type::lessless:
    os << "lessless";
    break;
  case token_type::lessequal:
    os << "lessequal";
    break;
  case token_type::lesslessequal:
    os << "lesslessequal";
    break;
  case token_type::spaceship:
    os << "spaceship";
    break;
  case token_type::greater:
    os << "greater";
    break;
  case token_type::greatergreater:
    os << "greatergreater";
    break;
  case token_type::greaterequal:
    os << "greaterequal";
    break;
  case token_type::greatergreaterequal:
    os << "greatergreaterequal";
    break;
  case token_type::caret:
    os << "caret";
    break;
  case token_type::caretequal:
    os << "caretequal";
    break;
  case token_type::pipe:
    os << "pipe";
    break;
  case token_type::pipepipe:
    os << "pipepipe";
    break;
  case token_type::pipeequal:
    os << "pipeequal";
    break;
  case token_type::question:
    os << "question";
    break;
  case token_type::colon:
    os << "colon";
    break;
  case token_type::semi:
    os << "semi";
    break;
  case token_type::equal:
    os << "equal";
    break;
  case token_type::equalequal:
    os << "equalequal";
    break;
  case token_type::comma:
    os << "comma";
    break;
  case token_type::hash:
    os << "hash";
    break;
  case token_type::hashhash:
    os << "hashhash";
    break;
  case token_type::hashat:
    os << "hashat";
    break;
  case token_type::dollar:
    os << "dollar";
    break;
  case token_type::kw_auto:
    os << "kw_auto";
    break;
  case token_type::kw_begin:
    os << "kw_begin";
    break;
  case token_type::kw_bool:
    os << "kw_bool";
    break;
  case token_type::kw_break:
    os << "kw_break";
    break;
  case token_type::kw_case:
    os << "kw_case";
    break;
  case token_type::kw_char:
    os << "kw_char";
    break;
  case token_type::kw_const:
    os << "kw_const";
    break;
  case token_type::kw_continue:
    os << "kw_continue";
    break;
  case token_type::kw_default:
    os << "kw_default";
    break;
  case token_type::kw_else:
    os << "kw_else";
    break;
  case token_type::kw_end:
    os << "kw_end";
    break;
  case token_type::kw_enum:
    os << "kw_enum";
    break;
  case token_type::kw_exit:
    os << "kw_exit";
    break;
  case token_type::kw_extern:
    os << "kw_extern";
    break;
  case token_type::kw_f32:
    os << "kw_f32";
    break;
  case token_type::kw_f64:
    os << "kw_f64";
    break;
  case token_type::kw_false:
    os << "kw_false";
    break;
  case token_type::kw_for:
    os << "kw_for";
    break;
  case token_type::kw_fn:
    os << "kw_fn";
    break;
  case token_type::kw_goto:
    os << "kw_goto";
    break;
  case token_type::kw_i8:
    os << "kw_i8";
    break;
  case token_type::kw_i16:
    os << "kw_i16";
    break;
  case token_type::kw_i32:
    os << "kw_i32";
    break;
  case token_type::kw_i64:
    os << "kw_i64";
    break;
  case token_type::kw_i128:
    os << "kw_i128";
    break;
  case token_type::kw_if:
    os << "kw_if";
    break;
  case token_type::kw_in:
    os << "kw_in";
    break;
  case token_type::kw_print:
    os << "kw_print";
    break;
  case token_type::kw_return:
    os << "kw_return";
    break;
  case token_type::kw_set:
    os << "kw_set";
    break;
  case token_type::kw_slice:
    os << "kw_slice";
    break;
  case token_type::kw_static:
    os << "kw_static";
    break;
  case token_type::kw_string:
    os << "kw_string";
    break;
  case token_type::kw_struct:
    os << "kw_struct";
    break;
  case token_type::kw_switch:
    os << "kw_switch";
    break;
  case token_type::kw_true:
    os << "kw_true";
    break;
  case token_type::kw_u8:
    os << "kw_u8";
    break;
  case token_type::kw_u16:
    os << "kw_u16";
    break;
  case token_type::kw_u32:
    os << "kw_u32";
    break;
  case token_type::kw_u64:
    os << "kw_u64";
    break;
  case token_type::kw_u128:
    os << "kw_u128";
    break;
  case token_type::kw_union:
    os << "kw_union";
    break;
  case token_type::kw_void:
    os << "kw_void";
    break;
  }

  return os;
}
} // namespace cawk