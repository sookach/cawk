//===- parser.h - cawk language parser ------------------------------------===//
//
//  This file defines and implements the parser interface. It's a relatively
//  simple handwritten recursive descent parser with a mix of top down operator
//  precedence (Pratt) parsing and precedence climbing for handling expressions.
//
//===----------------------------------------------------------------------===//

#pragma once