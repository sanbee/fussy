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
#include <units.h>
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
// The varaibles that use these codes are of type unsigned long int (64 bits).
//
// #define AUTOVAR_TYPE      0b0000000000000000000000000000000000000000000000001000000000000000
// #define PARTIALVAR_TYPE   0b0000000000000000000000000000000000000000000000000100000000000000
// #define RETVAR_TYPE       0b0000000000000000000000000000000000000000000000000010000000000000
// #define QSTRING_TYPE      0b0000000000000000000000000000000000000000000000000001000000000000

// #define VAR_TYPE          0b0000000000000000000000000000000000000000000000000000100000000000
// #define CONSTANT_TYPE     0b0000000000000000000000000000000000000000000000000000010000000000
// #define SYS_VAR_TYPE      0b0000000000000000000000000000000000000000000000000000001000000000
// #define BUILTIN_TYPE      0b0000000000000000000000000000000000000000000000000000000100000000

// #define BUILTIN2_TYPE     0b0000000000000000000000000000000000000000000000000000000010000000
// #define FMT_TYPE          0b0000000000000000000000000000000000000000000000000000000001000000
// #define NUMBER_TYPE       0b0000000000000000000000000000000000000000000000000000000000100000
// #define PROC_TYPE         0b0000000000000000000000000000000000000000000000000000000000010000

// #define FUNC_TYPE         0b0000000000000000000000000000000000000000000000000000000000001000
// #define UNDEF_TYPE        0b0000000000000000000000000000000000000000000000000000000000000100
// #define FSUC_TYPE         0b0000000000000000000000000000000000000000000000000000000000000010
// #define PSUC_TYPE         0b0000000000000000000000000000000000000000000000000000000000000001

#define AUTOVAR_TYPE      0x0000000008000000
#define PARTIALVAR_TYPE   0x0000000004000000
#define RETVAR_TYPE       0x0000000002000000
#define QSTRING_TYPE      0x0000000001000000
#define VAR_TYPE          0x0000000000080000
#define CONSTANT_TYPE     0x0000000000040000
#define SYS_VAR_TYPE      0x0000000000020000
#define BUILTIN_TYPE      0x0000000000010000
#define BUILTIN2_TYPE     0x0000000000000800
#define FMT_TYPE          0x0000000000000400
#define NUMBER_TYPE       0x0000000000000200
#define PROC_TYPE         0x0000000000000100
#define FUNC_TYPE         0x0000000000000008
#define UNDEF_TYPE        0x0000000000000004
#define FSUC_TYPE         0x0000000000000002
#define PSUC_TYPE         0x0000000000000001

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

typedef union VMacObjType
{
  /*  NUMTYPE Const; */
  OPCode Inst;
} VMacObjType;


typedef struct Calc_Symbol {
  TypeType            type;

  IDType              ID;
  IDList              IDL;
  vector<BASIC_NUM>   DSList, dx;

  NUMTYPE             value;
  long int            units;
  string              fmt;
  union {
    NUMTYPE           (*func1)(NUMTYPE);
    NUMTYPE           (*func2)(NUMTYPE,NUMTYPE);
    int               FuncStartPC;
  } otype;
  string              qstr;
  string              name;

  Calc_Symbol(): type(0), ID(),IDL(),DSList(),dx(),value(0),units(U_UNDEFINED),fmt(),otype(), qstr(),name() {};

} Calc_Symbol;

struct StackType;

typedef struct StackType {
  TypeType            type;
  IDList              ID;
  NUMTYPE             val;
  Calc_Symbol         *symb;
  int                 units;
  string              fmt;

  StackType(): type(0), ID(),val(), symb(NULL),fmt() {};
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
