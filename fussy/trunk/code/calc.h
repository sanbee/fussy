/* $Id: calc.h,v 1.3 2006/03/10 21:38:37 sbhatnag Exp $ */
/***************************************************************
 * Copyright (c) 2000-2006, 2007 S.Bhatnagar
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *

  History:

    First version.

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in

    Added the setgfmt() forward declaration.
               Sanjay Bhatnagar, March 10, 2006
***************************************************************/

#if !defined(CALC_H)
#if !defined(VMDBGHELP)

#define CALC_H
#include <math.h>
#include <emath.h>
#include <vector>
#include <units.h>
#include <string>
#include <defns.h>
#include <stdio.h>
#include <iostream>
#include "namespace.h"

/* int yyerror(char *); */
int           yylex();
int           yyparse();
int           calc(const char *InStr, ostream& ResultStream, FILE *Inp, FILE *Output);
int           calc(const char *, NUMTYPE *,FILE *,FILE *);
Instruction   calcgetInst(char *);
NUMTYPE       todeg(NUMTYPE);
NUMTYPE       todms(NUMTYPE);
NUMTYPE       Eint(NUMTYPE);
NUMTYPE       b2j(NUMTYPE, NUMTYPE);
NUMTYPE       j2b(NUMTYPE, NUMTYPE);
int           calc_error(char *);
int           ywarn(char *s, char *t);

Instruction   emit(Instruction);
NUMTYPE       Run(VMac &P);
void          InitSymbTab();
void          InitKeyWordTab();
void          InitFussy();
void          setDefaultType(int);
int           ywarn(char *s, char *t);
void          showCopyright(char *Msg=NULL);
void          showWarranty();
void          showHelp();
void          prtVM();
void          prtSymbTab();
//void          prtSymb(SymbTabType::iterator&);
template<class T> void prtSymb(T&);
void          prtLocSymbTab();
void          prtCSymbTab();
int           prtIDList();
int           mpush(Calc_Symbol *);
void          ReportErr(const char *Msg, const char *ErrType, const int& ErrLevel);
void          CollectGarbage();
void          MakePersistant(Calc_Symbol*);
void          MakeANumber(Calc_Symbol&S,float V,float E,char *Fmt=NULL);
/*
  StarLink library calls.  The are in FORTRAN (and hence the "_" in
  the name and all arguments as poitners).
*/
extern "C" {
  void slacaldj_(int *, int *, int *, double *, int *);
  double slagmst_(double *);
}
#endif
/********************************************************************
   Put only VM OPCODE related functions here.  The table of opcodes
   used by the function prtVM to print the compiled VM code is
   automatically generated from here at compile time.
********************************************************************/
int boot(), add(), sub(), vdiv(), mul(), assgn(),passgn();
int vpush(),rvpush(),cpush(),fcpush(),pop();
int bltin1(),bltin2(),print(),uminus(),power(),mod();
int ifcode(),whilecode(),forcode(),printcode();
int eq(), gt(), lt(), le(), ge(), door(), doand(), donot(), ne();
int setfmt(), call(), ret(), break_code();
int timeofday(), mjd(), fmjd(), lst(), getday(), getmonth(), getyear(), prtStckSize();
int setlong(),setlat(),getval(),getrms(),quit(),pre_incr(),post_incr(),pre_decr(),post_decr();
int setgfmt();
#endif
