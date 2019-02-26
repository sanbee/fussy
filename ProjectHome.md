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

The scripting [syntax](http://code.google.com/p/fussy/wiki/FussySyntax) of _fussy_ is similar to that of C. It is therefore easy to use with minimal learning.

**If you find the software useful, we look forward to hearing from you with your comments/suggestions.**

**Please also refer to its usage in your publication by quoting the following:**

  * **Project location: http://code.google.com/p/fussy**
  * **Web-paper describing the algorithm: http://www.aoc.nrao.edu/~sbhatnag/Softwares/fussy/fussy/fussy.html**
