// $Id: defns.h,v 1.3 2006/03/10 21:38:37 sbhatnag Exp $
/******************************************************************
 * Copyright (c) 2000-2018, 2019 S.Bhatnagar
 *
 *  This file is part of fussy.
 *
 *  fussy is a free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  fussy is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with fussy.  If not, see <https://www.gnu.org/licenses/>.
 *

  History:

    First version.

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in
    
    Changed the DEFAULT_FMT marco to return the string form the
    global default print format (which is a SYS_VAR and resides
    in the ConstTab). Defined DEFAULT_FMT_STR to be "10.5%f"
    which is set as the default print format at the start.

               Sanjay Bhatnagar, March 10, 2006

******************************************************************/
#if !defined(DEFNS_H)
#define DEFNS_H

#include <vector>
#include <list>
#include <emath.h>
#include <math.h>
#include <BitField.h>
#include <string>
#include <set>
#include "namespace.h"
//
// Type of symbols, used internally to find
// invalid symbol in the symbol table.
//
#define OP_ON_TYPE        0
#define OP_ON_REF         1
//
// Flags used internally for parsing and interactive
// sessions.
//
#define END_OF_INPUT      -2
#define SYNTAX_ERROR      1
#define FLAG_INIT         0
#define FLAG_CTRL_C       1 
#define FLAG_MORE_INP     2
#define FLAG_IN_CSTMT     256
#define FRAME_STACK_DEPTH 100
#define CONST_FMT_LEN     32
#define PROCRET           0x8000
#define FMTCOPY           -1

#define yyin              calc_in
#define yyout             calc_out
//
// The default number and string printing formats.
//
#define DEFAULT_FMT_STR      "%10.5f"
#define DEFAULT_FMT          GlobalDefaultFmt->fmt
#define DEFAULT_STRING_FMT    "%s"
//
// Value of various builtin constants.
//
#define F_D2R             ((M_PI/180.0))
#define F_H2R             (((M_PI)/12.0))
#define F_R2D             ((180.0/M_PI))
#define F_R2H             ((12.0/M_PI))
#define F_S2R             (7.272205E-05)
#define F_A2R             ((M_PI/180.0/3600.0))
#define F_C               ((299792458.0))  /* speed of light in m/s */
#define F_KB              ((1.380470e-23)) /* Bolzman's constant */
#define F_PC2M            ((3.085678e+16)) /* Parsec to meters */
#define F_PC2LY           ((3.261633))     /* Parsec to lightyears */
#define F_AU2M            ((1.495979e+11)) /* Astro.units to meters */
#define LONGITUDE         ((74.051))       /* Pune Long. in deg. */
#define LATITUDE          ((19.08))        /* Pune Lat.  in deg. */
//
// The number formats used internally.  edouble is the
// extended double format to hold the value and the associated
// measurement error.
//
#define NUMTYPE           edouble
#define BASIC_NUM         double
//
// The the value of a NUMTYPE (a) to b and associated
// measurement error of c.
//
#define SETVAL(a,b,c)     (a.setval((b),(c)))
//
// Various types of symbol (used internally in the VM code
// execution).
//
#define AUTOVAR_TYPE      0x80000000
#define PARTIALVAR_TYPE   0x40000000
#define RETVAR_TYPE       0x20000000
#define QSTRING_TYPE      0x10000000
#define VAR_TYPE          0x08000000
#define CONSTANT_TYPE     0x04000000
#define SYS_VAR_TYPE      0x02000000
#define BUILTIN_TYPE      0x01000000
#define BUILTIN2_TYPE     0x00800000
#define FMT_TYPE          0x00400000
#define NUMBER_TYPE       0x00200000
#define PROC_TYPE         0x00100000
#define FUNC_TYPE         0x00080000
#define UNDEF_TYPE        0x00040000
#define FSUC_TYPE         0x00020000
#define PSUC_TYPE         0x00010000

#define SETBIT(V,MASK)    ((V) |= (MASK))  // Bitwise OR with the mask
#define RESETBIT(V,MASK)  ((V) &= ~(MASK)) // Bitwise AND with the complement of the mask
#define ISSET(V,MASK)     ((((V) & (MASK))==(MASK)))
#define ISPARTIAL(V)      (((ISSET((V),(PARTIALVAR_TYPE))==1)?'P':'V'))
#define ISPERMANENT(V)    ((ISSET((V),(NUMBER_TYPE))|ISSET((V),(CONSTANT_TYPE))|ISSET((V),(SYS_VAR_TYPE))))

typedef unsigned int      IDType;
typedef unsigned long     TypeType;
typedef set<IDType>       IDList;
typedef int               (*OPCode)();
typedef int               (*Instruction)();
typedef Instruction       VMacType ;
typedef vector<VMacType>  VMac;
#define STOP              (VMacType)(-1)

typedef struct Calc_Symbol {
  TypeType            type;

  IDType              ID;
  IDList              IDL;
  vector<BASIC_NUM>   DSList, dx;

  NUMTYPE             value;
  long int                 units;
  string              fmt;
  union {
    NUMTYPE           (*func1)(NUMTYPE);
    NUMTYPE           (*func2)(NUMTYPE,NUMTYPE);
    int               FuncStartPC;
  } otype;
  string              qstr;
  string              name;
  
  // void cleanupQStr() {if (otype.qstr) delete otype.qstr;/*otype.qstr=NULL;*/};
  // void makeQStr(const string& s) {if (otype.qstr==NULL) otype.qstr=new string(s);};

} Calc_Symbol;

struct StackType;

typedef struct StackType {
  TypeType            type;
  IDList              ID;
  NUMTYPE             val;
  Calc_Symbol         *symb;
  int                 units;
  string              fmt;
  
  // void cleanupSymb() {symb->cleanupQStr();}
} StackType;

typedef struct FrameType {
  unsigned int RetPC,SP;
} FrameType;

typedef vector<StackType> Stack;
typedef vector<BASIC_NUM> DStack;
typedef vector<DStack>    DSType;
typedef vector<FrameType> FrameStackType;
typedef list<Calc_Symbol> SymbTabType;
typedef list<Calc_Symbol> TmpSymbTabType;
typedef list<Calc_Symbol> ConstTabType;
typedef list<Calc_Symbol> LocalSymbTabType;

typedef struct BreakException  { } BreakException;
typedef struct ReturnException { } ReturnException;
typedef struct ExitException   { } ExitException;

unsigned long gettype(unsigned long);

#endif
