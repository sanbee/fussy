# fussy
The _fussy_ scripting language implements an algorithm for automatic propagation of random measurement errors in an arbitrary mathematical expression. It is internally implemented as a virtual machine for efficient runtime performance and can be used as an interpreter by the user. A simple C binding to the interpreter is also provided. Mathematical expressions can be implemented as a collection of sub-expressions, as sub-program units (functions or procedures) or as single atomic expressions.  Sub-expressions can be assigned to temporary variables which can then be used to write the final expression.

The scripting [syntax](https://github.com/sanbee/fussy/blob/wiki/FussySyntax.md#syntax-for-the-fussy-scripting-language) of _fussy_ is similar to that of C. It is therefore easy to use with minimal learning.

**If you find the software useful, we look forward to hearing from you with your comments/suggestions.**

**Please also refer to its usage in your publication by quoting the following:**

  * **Project location: https://github.com/sanbee/fussy**
  * **Web-paper describing the algorithm: http://sanbee.github.io/fussy/HTML/fussy.html**
