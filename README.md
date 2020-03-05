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
# GNU _fussy_

__This project is now part of the GNU project. This repos will be sync'ed with the official [GNU _fussy_](https://www.gnu.org/software/fussy) repos on [Savannah](https://savannah.gnu.org/projects/fussy/).__

The GNU _fussy_ program implements a scripting language with an algorithm
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

The scripting [syntax](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#syntax-for-the-fussy-scripting-language) of GNU _fussy_ is similar to the C programming language. It is easy, particularly for those familiar with programming in C, to use GNU _fussy_ with minimal learning. In its simplest form, GNU _fussy_ interpretor can be used interactively as a simple calculator with the added feature of automatic error prograpation (see syntax for [simple expressions](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#expressionsstatements), [sub-expressions](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#sub-expressions), and [functions/procedure](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#functionprocedure)).

## Installation 

The build system now uses the standard GNU build tools (automake, autoconfigure).  On most standard Linux distributions following commands should be sufficient to build and test _fussy_
    
    ./configure
    make check
    
A successful build should produce the following Testsuite summary:

```
============================================================================
Testsuite summary for fussy 2.0
============================================================================
# TOTAL: 1
# PASS:  1
# SKIP:  0
# XFAIL: 0
# FAIL:  0
# XPASS: 0
# ERROR: 0
```    
In case this does not work, you can re-build the "./configure" script by running the "build" script.

In case this still fails to build GNU _fussy_, please let us know.

**If you find the software useful, we look forward to hearing from you with your comments/suggestions.**

**Please also refer to its usage in your publication by quoting the following:**

  *  **Project homepage:**
         https://www.gnu.org/software/fussy

  *  **Code reposatories:**
         https://savannah.gnu.org/projects/fussy , HTTPS://GITHUB.COM/SANBEE/FUSSY

  * **GNU fussy manual:**
         https://www.gnu.org/software/fussy/manual

  * **Web-paper:** 
         HTTP://SANBEE.GITHUB.IO/FUSSY/HTML/FUSSY.HTML
