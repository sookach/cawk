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

  // Keywords.
  kw_auto,
  kw_begin,
  kw_break,
  kw_case,
  kw_char,
  kw_const,
  kw_continue,
  kw_default,
  kw_else,
  kw_end,
  kw_enum,
  kw_extern,
  kw_f32,
  kw_f64,
  kw_f128,
  kw_for,
  kw_fn,
  kw_goto,
  kw_i8,
  kw_i16,
  kw_i32,
  kw_i64,
  kw_i128,
  kw_if,
  kw_print,
  kw_return,
  kw_static,
  kw_string,
  kw_struct,
  kw_switch,
  kw_typedef,
  kw_u8,
  kw_u16,
  kw_u32,
  kw_u64,
  kw_u128,
  kw_union,
  kw_void,
};
} // namespace cawk