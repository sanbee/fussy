# Developer Notes #

## 5 Aug 2006 ##
### Changes in table.cc (`Calc_Symbol *installConst(Calc_Symbol& d,int NewID)`) ###


The strategy of managing the table of constants is the following now:
> calcgetConst() for NUMBER\_TYPES always returns NULL

> A constant is always put on the table and a new ID assigned depending
> upon NewID

> Since its always pushed at the end, pick up the pointer to the latest
> constant from the end

> if in function/procedure compilation set symb->name[0](0.md) to 1

Cleanup() cleans up the ConstTab of all symbols which are not
referenced. i.e. for which name[0](0.md) != 1.

This essentially make `ConstTab` is a permanent storage for constants
used in sub-program code and temp. store for constants used in vm code
for the interactive session and/or parameters (all temps).
Functionally, this then treats all constants, as indep. variates, even
if their values are the same.  Same name symbols on the `SymbTab`.


### Changes in table.cc (`void cleanupSymbTab(SymbTabType& Tab, int OP,TypeType)`) ###

Now removes unreferenced entries in the ConstTab.  These are temp. numbers that may
be generated (either in interactive sessions or while runing sub-programs).  This allows
treating all numbers are indep. variates.

### Changes in fussy.y ###

The `InFuncDefn` flag is removed right a the end of func. def. code.  With this, constants
that are generated during sub-program compilation go on the ConstTab with the flag which
tells the Cleanup() that they are referenced and hence should not be removed.

## 7 Aug. 2006 ##

The above changes, while operationally correct, did not optimize the
usage of ConstTab (left a lot of redundent info. on it).  Following
changes optimizes the ConstTab for NUMBER\_TYPE.  Similar optimization
can be done for other types on ConstTab.

### Changes in `Calc_Symbol *installConst(Calc_Symbol&d,int NewID)` ###

---

> The strategy of managing the table of constants is the following now:

> For NUMBER\_TYPE, look on ConsTab if (1) installing a number for
> sub-program code (InFuncDefn==True), and (2) the symbol being
> installed has no error (Symbol.rms()==0).  Unless these two
> conditions are met in calcgetConst(), it does not search in ConsTab.

> If ConsTab is searched, return an entry if (1) it is an entry
> referenced by some sub-program code - i.e., it is permanent, and
> (2) its value (including RMS) matches.

> In case a new symbol is added to the ConstTab, since its always
> pushed at the end, pick up the pointer to the latest constant
> from the end (instead of searching in ConsTab again!).

> Finally, if in function/procedure compilation set symb->name[0](0.md) to 1

> Cleanup() cleans up the ConstTab of all symbols which are not referenced.
> i.e. for which name[0](0.md) != 1.

> This essentially makes ConstTab a permanent storage for constants used in
> sub-program code and temp. store for constants used in vm code for the interactive
> session and/or parameters (all temps).
> Functionally, this then treats all constants, as indep. variates, even if their
> values are the same.  Same name symbols on the SymbTab.

### Changes in `Calc_Symbol *calcgetConst(Calc_Symbol& v)` ###

---


> If in sub-prog. code definition and the supplied symbol will NOT
> participate in error propagation (i.e. it has no error), then proceed
> to search for in ConstTab.

> If the ConstTab entry is a referenced entry, and it is of type
> NUMBER\_TYPE, and it's value (including RMS) matches, then return
> the pointer to this entry.