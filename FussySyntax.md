# Syntax for the fussy scripting language #

This page describes the fussy syntax. Statements are interactively executed as soon as they are completed. The virtual code for the sub-programs (function or procedure) is held in the memory and executed when the sub-programs are called.

## Numbers ##
Numbers in fussy are represented as floating point numbers and can be specified with or without the decimal point, or in the exponent format. Optionally, an error can also be associated with the numbers via the pm directive. E.g., 75.3+/-10.1 can be expressed as `75.3pm10.1`. Numbers can also be tagged with units (see Section on [Units][Units)) or a C-styled printing format (see Section on [FussySyntax#Formatting](FussySyntax#Formatting.md)).

## Units ##

Numerical values can be specified along with their units. As of now, the only units supported are degree, arcmin, arcsec, hours, minute, and seconds. These can be specified by appending 'd', ''', '"', 'h', 'm', 's' respectively to the numeric values. Internally, all numeric values are always stored in the MKS system of units. The default units for a variable used to specify angles or time is radians. If the values are specified along with any of the above mentioned units, the values are still stored internally as radians. However while printing (see Section on [FussySyntax#Print\_Statement](FussySyntax#Print_Statement.md)), the values are formatted automatically and printed with the appropriate units.

## Operators and built-in functions ##

The normal binary operators of type `expr <op> expr`, where `expr` is any expression/variable/constant and `<op>` is one of '+', '-', '/', `'*'`, '^' and `'**'` binary operators perform the usual mathematical operations in addition to error propagation. The comparison operators '<', '>', '=', '!=', '<=', '>=' and the logical operators '||' and '&&' have the usual meaning. Apart from the usual operation, the var=expr assignment operator also does the error propagation in the expression on the RHS and assigns it as the error for the variable on the LHS. In addition to this, the assignment operator for partial variables (pvar:=expr) is also defined. This does not propagate the errors on the RHS but instead transfers all the required information for error propagation to the variable on the LHS (see Section [FussySyntax#Sub-expressions](FussySyntax#Sub-expressions.md)). The result of these assignment statements is the value of the variable on the LHS. Hence expressions like `sin(x=0.1pm0.02)` are equivalent to `x=0.1pm0.02;sin(x);`. The prefix and postfix operators `<op>var` and `var<op>` where `<op>` is either '++' or '--' and var is any user defined variable are also defined. These increment or decrement the value of the variables by one. The prefix and postfix operators operate on the variables before and after the variable is further used respectively.

In addition, two operators of type `expr.<op>` where `<op>` is either val or rms are also defined. These operators extract the value and the associated (propagated) error in expr which can be any mathematical expression or a variable.


## Expressions/Statements ##

Numbers and variables can be combined with the mathematical operators and logical operators to form an expression. Expressions can be used as arguments to built-in or user defined functions (see Section [FussySyntax#Function/procedure](FussySyntax#Function/procedure.md)). An expression followed by a NEWLINE prints its result on the output stream (see Section [FussySyntax#Print\_Statement](FussySyntax#Print_Statement.md)) in the default format (see Section [FussySyntax#Formatting](FussySyntax#Formatting.md)).

For the purpose of error propagation, the print statement and the assignment operator (the "='' operator but not the ":='' operator; see Section on [FussySyntax#Sub-expressions](FussySyntax#Sub-expressions.md)) are treated as the terminal nodes of the parsing tree which invokes the final error propagation.

Assigning a value to a variable also creates the variable. The type of the value assigned to the variable determines its type (and overrides the value or the type of a previously declared variable). E.g.

```
   >H_0=75pm10
   >H_0
             75.00000 +/-   10.00000
   >H_0="The Hubble constant\n"
   >H_0
    The Hubble constant
```

A semi-colon (';') is a delimiter to separate multiple expressions in a single line. Statements on separate lines need not be delimited by semi-colons (though it is not an error to do so). Compound statements are a group of simple statements, grouped using the curly-brace pair ('{' and '}') (e.g. `{a=1.5; b=2;`}). As may be obvious, compound statements can also be nested. The `'/*'` and`'*/'` pair can be used as comment delimiters. Comment delimiters however cannot be nested.


## Sub-expressions ##

A special assignment operator ':=' is used to assign sub-expressions to user defined variables. Sub-expression variables are different from normal variables in that their propagated error is computed on-the-fly when required, i.e. when they are printed or are assigned to a normal variable using the '=' operator or at an operator node of a parsing tree when used in another expression. E.g.

```
   >x=1pm0.1
   >s:=sin(x);c:=cos(x);
   >sin(x)/cos(x) /* Compute tan(x) as sin(x)/cos(x) */
       1.55741 +/-    0.34255
   >s/c           /* Compute tan(x) using two PARTIAL_VAR */
       1.55741 +/-    0.34255
   >tan(x)        /* Direct computation of tan(x) */
       1.55741 +/-    0.34255
   >s2=s;
   >s2/c          /* Compute tan(x) with a normal variable
                     and one PARTIAL_VAR.  Error propagates 
                     differently */
       1.55741 +/-    0.26236
```

## Variables and function/procedure names ##

Variable/function/procedure names can be of any length and must match the regular expression `[a-zA-Z_]+[a-zA-Z0-9_]*`. That is, the names must start with an alphabet or `'_'` and can be followed by one or more alpha-numeric characters or `'_'`.


## Function/procedure ##

Sub-programs can be written as functions or procedures. The only difference between functions and procedures is that functions must return a value while procedures must not return a value. The type of a sub-program which returns a value using the `return <expression>` statement becomes func. If return is not used, or is used without an expression, the type becomes proc. The type of the sub-program therefore need not be declared. It is an error to use a procedure in an expression or pass a procedure as an argument to another sub-program where a function should have been passed.

A function or procedure declaration begins with a variable name followed by an argument list. The argument list is enclosed by a round bracket pair ('(' and ')'). A '()' specifies an empty argument list. The function body is in enclosed between the '{' and '}' brackets. E.g.

```
   >/* An example of a funtion declaration */
   >f() { return sin(PI/2); }
   >/* An example of a procedure declaration */ 
   >p() {print "Value of f() = ",f(),"\n";}
   >f()
              1.00000
   >p() 
   Value of f() =    1.00000
```

A sub-program can be passed as an argument to another sub-program. An argument corresponding to a sub-program can be specified using the func (for a function) or proc (for a procedure) directive. E.g.

```
   >f(x) { return sin(x); }
   >p(func fa,x) {print "The value of f(",x%5.2f,") =",fa(x),"\n";}
   >p(f,10)
   The value of f(10.00) =  -0.54402
```

All symbols (variables, functions, procedures) used in the sub-program code must be either global variables declared before the sub-program declaration or must be one of the argument list. Temporary variables, the scope of which is within the sub-program only, can be declared using the auto directive. E.g.

```
   >f(x) { return sin(x); }
   >p(func fa,x)
     {
       auto t;
       t=fa(x);
       print "The value of f(",x%5.2f,") =",t,"\n";
     }
   >p(f,10)
   The value of f(10.00) =  -0.54402
```

## Control statements ##

The if-else, while- and for-loops constitute the program control statements. These loops can be broken at any stage with the use of the break statement. As of now, the conditions which control the logic is evaluated ignoring the error with the control variables. Ultimately the goal is to provide a language feature to specify a significance level and the conditional statements return true if the error on the evaluated value is within the significance level, else return false.

### if-else ###

The syntax for the if-else statement is:

```
    if (condition)
       if-body-statment;

         or

    if (condition)
       if-body-statment else
       else-body-statment;
```

The if-body-statement and the else-body-statement can be any valid compound or simple statement. In case of a simple statement, the terminating semi-colon is necessary.

### while-loop ###

The syntax for the while-loop is:

```
    while (condition)
       body-statment
```

The body-statement can be either a simple or a compound statement and in case it is a simple statement, the terminating semi-colon defines the end of the loop.

### for-loop ###

The syntax for the for-loop is:

```
    for (init;condition;incr)
      body-statment
```

where init is a comma (',') separate list of simple statements for initializing the loop variables. E.g. init can be `i=0,j=0,k=0`. condition is a simple, single statement while incr is a list of comma separated statement(s). The body-statement can be any valid simple or compound statement. init statements are executed first followed by the condition statement. If the result of the condition statement is non-zero (logical true), the body-statements, the incr statement(s) and the condition statements are executed in a sequence till the result of the condition statement is zero (logical false). E.g. following is a valid for-loop with 3 loop-variables, only one of which is checked in the condition:

```
    for (i=0,j=0,k=0;i<10;i=i+1,j=j+1,k=k+1) 
       print "i= ",i," j= ",j," k= ",k,"\n";
```


## Print statement ##

The print statement takes a comma separated list of objects to be printed. These objects can be quoted-strings, variables, constants, condition statements or user defined function names. The list can consist of any number of objects and is terminated by a semi-colon. The format in which the numeric values are printed is defined by the format modifier associated with the values (see Section [FussySyntax#Formatting](FussySyntax#Formatting.md)). All escaped-characters used in C-styled printing have the same effect as in the output of the C-styled printf statement.


## Formatting ##

Values can be formatted for printing in a variety of ways. The format in which a variable is printed is associated with the variable and consists of a printf styled formatting string (with extensions for specifying the units of the numerical values as well). E.g., if `x=75pm10`, by default `x` will be printed using the `'%10.5f'` format. The default print format can be modified using the '.' operator on a variable. E.g., one can fix the default print format of `x` to `'%5.2f'` by `x.=%5.2f`.

The print format of a value can also be temporarily modified by specifying the format along with the variable/value. E.g. the value of `x` can be printed in the exponent format as print `x%E` or in the in hexadecimal format as print `x%x`.

An extra formatting, not available in printf formatting, is that of printing the individual bit values using the `%b` format. With this, the value is printed in binary (1 or 0) format. `%B` does the same thing except that it prints a space after every 8 bits. The value is casted into a unsigned long integer before printing.

```
   >x=10;x%B
        00000000 00000000 00000000 00001010
```

If the units of a value are specified, the print format is also appropriately modified. If a variable has units of time or angle, its print format is automatically set to `%hms` or `%dms` and are printed in the `XXhXXmXX.XXs` and `XXdXX'XX.XX"` styles respectively.