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
  starstar,
  starequal,
  starstarequal,
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
  slashslash,
  slashequal,
  slashslashequal,
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
  kw_function,
  kw_getline,
  kw_goto,
  kw_hmap,
  kw_hset,
  kw_i8,
  kw_i16,
  kw_i32,
  kw_i64,
  kw_i128,
  kw_if,
  kw_in,
  kw_map,
  kw_print,
  kw_return,
  kw_set,
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
  kw_vector,
  kw_void,
};

std::ostream &operator<<(std::ostream &os, token_type t) {
  switch (t) {
  default:
    return os << "undefined token";
  case token_type::unknown:
    return os << "unknown";
  case token_type::eof:
    return os << "eof";
  case token_type::identifier:
    return os << "identifier";
  case token_type::numeric_constant:
    return os << "numeric_constant";
  case token_type::char_constant:
    return os << "char_constant";
  case token_type::string_literal:
    return os << "string_literal";
  case token_type::l_square:
    return os << "l_square";
  case token_type::r_square:
    return os << "r_square";
  case token_type::l_paren:
    return os << "l_paren";
  case token_type::r_paren:
    return os << "r_paren";
  case token_type::l_brace:
    return os << "l_brace";
  case token_type::r_brace:
    return os << "r_brace";
  case token_type::period:
    return os << "period";
  case token_type::ellipsis:
    return os << "ellipsis";
  case token_type::amp:
    return os << "amp";
  case token_type::ampamp:
    return os << "ampamp";
  case token_type::ampequal:
    return os << "ampequal";
  case token_type::star:
    return os << "star";
  case token_type::starstar:
    return os << "starstar";
  case token_type::starequal:
    return os << "starequal";
  case token_type::starstarequal:
    return os << "starstarequal";
  case token_type::plus:
    return os << "plus";
  case token_type::plusplus:
    return os << "plusplus";
  case token_type::plusequal:
    return os << "plusequal";
  case token_type::minus:
    return os << "minus";
  case token_type::arrow:
    return os << "arrow";
  case token_type::minusminus:
    return os << "minusminus";
  case token_type::minusequal:
    return os << "minusequal";
  case token_type::tilde:
    return os << "tilde";
  case token_type::exclaim:
    return os << "exclaim";
  case token_type::exclaimequal:
    return os << "exclaimequal";
  case token_type::exclaiml_square:
    return os << "exclaiml_square";
  case token_type::slash:
    return os << "slash";
  case token_type::slashslash:
    return os << "slashslash";
  case token_type::slashequal:
    return os << "slashequal";
  case token_type::slashslashequal:
    return os << "slashslashequal";
  case token_type::percent:
    return os << "percent";
  case token_type::percentequal:
    return os << "percentequal";
  case token_type::less:
    return os << "less";
  case token_type::lessless:
    return os << "lessless";
  case token_type::lessequal:
    return os << "lessequal";
  case token_type::lesslessequal:
    return os << "lesslessequal";
  case token_type::spaceship:
    return os << "spaceship";
  case token_type::greater:
    return os << "greater";
  case token_type::greatergreater:
    return os << "greatergreater";
  case token_type::greaterequal:
    return os << "greaterequal";
  case token_type::greatergreaterequal:
    return os << "greatergreaterequal";
  case token_type::caret:
    return os << "caret";
  case token_type::caretequal:
    return os << "caretequal";
  case token_type::pipe:
    return os << "pipe";
  case token_type::pipepipe:
    return os << "pipepipe";
  case token_type::pipeequal:
    return os << "pipeequal";
  case token_type::question:
    return os << "question";
  case token_type::colon:
    return os << "colon";
  case token_type::semi:
    return os << "semi";
  case token_type::equal:
    return os << "equal";
  case token_type::equalequal:
    return os << "equalequal";
  case token_type::comma:
    return os << "comma";
  case token_type::hash:
    return os << "hash";
  case token_type::hashhash:
    return os << "hashhash";
  case token_type::hashat:
    return os << "hashat";
  case token_type::dollar:
    return os << "dollar";
  case token_type::kw_auto:
    return os << "kw_auto";
  case token_type::kw_begin:
    return os << "kw_begin";
  case token_type::kw_bool:
    return os << "kw_bool";
  case token_type::kw_break:
    return os << "kw_break";
  case token_type::kw_case:
    return os << "kw_case";
  case token_type::kw_char:
    return os << "kw_char";
  case token_type::kw_const:
    return os << "kw_const";
  case token_type::kw_continue:
    return os << "kw_continue";
  case token_type::kw_default:
    return os << "kw_default";
  case token_type::kw_else:
    return os << "kw_else";
  case token_type::kw_end:
    return os << "kw_end";
  case token_type::kw_enum:
    return os << "kw_enum";
  case token_type::kw_exit:
    return os << "kw_exit";
  case token_type::kw_extern:
    return os << "kw_extern";
  case token_type::kw_f32:
    return os << "kw_f32";
  case token_type::kw_f64:
    return os << "kw_f64";
  case token_type::kw_false:
    return os << "kw_false";
  case token_type::kw_for:
    return os << "kw_for";
  case token_type::kw_function:
    return os << "kw_function";
  case token_type::kw_getline:
    return os << "kw_getline";
  case token_type::kw_goto:
    return os << "kw_goto";
  case token_type::kw_hmap:
    return os << "kw_hmap";
  case token_type::kw_hset:
    return os << "kw_hset";
  case token_type::kw_i8:
    return os << "kw_i8";
  case token_type::kw_i16:
    return os << "kw_i16";
  case token_type::kw_i32:
    return os << "kw_i32";
  case token_type::kw_i64:
    return os << "kw_i64";
  case token_type::kw_i128:
    return os << "kw_i128";
  case token_type::kw_if:
    return os << "kw_if";
  case token_type::kw_in:
    return os << "kw_in";
  case token_type::kw_map:
    return os << "kw_map";
  case token_type::kw_print:
    return os << "kw_print";
  case token_type::kw_return:
    return os << "kw_return";
  case token_type::kw_set:
    return os << "kw_set";
  case token_type::kw_static:
    return os << "kw_static";
  case token_type::kw_string:
    return os << "kw_string";
  case token_type::kw_struct:
    return os << "kw_struct";
  case token_type::kw_switch:
    return os << "kw_switch";
  case token_type::kw_true:
    return os << "kw_true";
  case token_type::kw_u8:
    return os << "kw_u8";
  case token_type::kw_u16:
    return os << "kw_u16";
  case token_type::kw_u32:
    return os << "kw_u32";
  case token_type::kw_u64:
    return os << "kw_u64";
  case token_type::kw_u128:
    return os << "kw_u128";
  case token_type::kw_union:
    return os << "kw_union";
  case token_type::kw_vector:
    return os << "kw_vector";
  case token_type::kw_void:
    return os << "kw_void";
  }

  return os;
}
} // namespace cawk