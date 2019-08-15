<!--******************************************************************
 * Copyright (c) 2000-2018, 2019 S.Bhatnagar
 *
 *   This file is part of fussy.
 *
 *   fussy is a free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   fussy is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with fussy.  If not, see <https://www.gnu.org/licenses/>.
 *
******************************************************************-->
# fussy
The _fussy_ program implements a scripting language with an algorithm
for automatic error propagation of random measurement errors in an
arbitrary mathematical expression.  The program can be used as a simple
interactive calculator with error propagation.  Mathematical
expressions can be implemented as a collection of sub-expressions, as
sub-program units (functions or procedures) or as single atomic
expressions.  Sub-expressions can be assigned to temporary variables
which can then be used to write the final expression.  The interpreter
is internally implemented as a virtual machine for efficient runtime
performance and can be used as an interpreter in a client code as
well.  A simple C binding to the interpreter is also provided.

The scripting [syntax](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#syntax-for-the-fussy-scripting-language) of _fussy_ is similar to the C programming language. It is easy, particularly for those familiar with programming in C, to use _fussy_ with minimal learning. In its simplest form, _fussy_ interpretor can be used interactively as a simple calculator with the added feature of automatic error prograpation (see syntax for [simple expressions](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#expressionsstatements), [sub-expressions](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#sub-expressions), and [functions/procedure](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#functionprocedure)).

**If you find the software useful, we look forward to hearing from you with your comments/suggestions.**

**Please also refer to its usage in your publication by quoting the following:**

  * **Project location: https://github.com/sanbee/fussy**
  * **Web-paper describing the algorithm: http://sanbee.github.io/fussy/HTML/fussy.html**
