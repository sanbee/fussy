// $Id: calcinit.cc,v 1.3 2006/03/10 21:38:37 sbhatnag Exp $
/******************************************************************
 * Copyright (c) 2000-2018, 2019 S.Bhatnagar
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

    Added the "fmt" symbol as a SYS_VAR and set its fmt string to 
    the DEFAULT_FMT.  Also set the GlobalDefaultFmt to point to this
    symbol in the ConstTab.
               Sanjay Bhatnagar, March 10,2006

******************************************************************/

#include <stdio.h>
#include <calc.h>
#include <emath.h>
#include <fussyparse.hh>
#include <calcinit.h>
#include <math.h>
//#include <AngFmt.h>
#include <ErrorObj.h>
#include "defns.h"
#include <IDResource.h>

extern IDResource IDR;
extern short int InCStmt;
extern int InFuncDefn;
extern unsigned int sp;
extern SymbTabType SymbTab;
extern ConstTabType ConstTab;
extern LocalSymbTabType LocalSymbTab;
extern FrameStackType FrameStack;
extern Stack stck;
extern double DefaultSigma;
extern NUMTYPE GlobalLong, GlobalLat;
extern IDType GlobalLongID;
extern Calc_Symbol* GlobalDefaultFmt;
#define PUTIDList(s,i)  {(s).IDL.insert((s).IDL.end(),(i));}
#define PUTID(s,i)  {(s).ID=(i);}
//
//-------------------------------------------------------------------
// Given a VM type, return the equivalent type used in the parser.
//   unsigned long type    One of the types defined in defns.h
//
unsigned long gettype(unsigned long type)
{
  if      (ISSET(type,VAR_TYPE))         return VAR;
  else if (ISSET(type,QSTRING_TYPE))     return QSTRING;
  else if (ISSET(type,CONSTANT_TYPE))    return CONSTANT;
  else if (ISSET(type,SYS_VAR_TYPE))     return SYS_VAR;
  //  else if (ISSET(type,AUTOVAR_TYPE)) return AUTOVAR;
  else if (ISSET(type,PARTIALVAR_TYPE))  return PARTIAL_VAR;
  else if (ISSET(type,NUMBER_TYPE))      return NUMBER;
  else if (ISSET(type,PROC_TYPE))        return PROC;
  else if (ISSET(type,FUNC_TYPE))        return FUNC;
  else if (ISSET(type,FMT_TYPE))         return FMT;
  else if (ISSET(type,BUILTIN_TYPE))     return BUILTIN;
  else if (ISSET(type,BUILTIN2_TYPE))    return BUILTIN2;
  else if (ISSET(type,FSUC_TYPE))        return FSUC;
  else if (ISSET(type,PSUC_TYPE))        return PSUC;
  else if (ISSET(type,UNDEF_TYPE))       return UNDEF;
  else return UNDEF_TYPE;
}
//
//
//-------------------------------------------------------------------
// Initialize the internal tables and the global ID resource.
//
void InitFussy()
{
  //
  // Allocate ID=0.  That's used for all values which are assured to
  // to have Measurement Error=0 (like the result of var.val)
  //
  GetNewID();
  //  IDR.GetNewID(); 
  InitSymbTab();
}
//
//-------------------------------------------------------------------
// Initialize the symbol table with the builtin functions and constants.
//
void InitSymbTab()
{
  Calc_Symbol s;
  PUTID(s,0); s.name="sin";   s.type=BUILTIN_TYPE;  s.otype.func1=sin;   SymbTab.push_back(s);
  PUTID(s,0); s.name="cos";   s.type=BUILTIN_TYPE;  s.otype.func1=cos;   SymbTab.push_back(s);
  PUTID(s,0); s.name="tan";   s.type=BUILTIN_TYPE;  s.otype.func1=tan;   SymbTab.push_back(s);
  PUTID(s,0); s.name="asin";  s.type=BUILTIN_TYPE;  s.otype.func1=asin;  SymbTab.push_back(s);
  PUTID(s,0); s.name="acos";  s.type=BUILTIN_TYPE;  s.otype.func1=acos;  SymbTab.push_back(s);
  PUTID(s,0); s.name="atan";  s.type=BUILTIN_TYPE;  s.otype.func1=atan;  SymbTab.push_back(s);
  PUTID(s,0); s.name="atan2"; s.type=BUILTIN2_TYPE; s.otype.func2=atan2; SymbTab.push_back(s);
  PUTID(s,0); s.name="cosh";  s.type=BUILTIN_TYPE;  s.otype.func1=cosh;  SymbTab.push_back(s);
  PUTID(s,0); s.name="sinh";  s.type=BUILTIN_TYPE;  s.otype.func1=sinh;  SymbTab.push_back(s);
  PUTID(s,0); s.name="tanh";  s.type=BUILTIN_TYPE;  s.otype.func1=tanh;  SymbTab.push_back(s);
  PUTID(s,0); s.name="acosh"; s.type=BUILTIN_TYPE;  s.otype.func1=acosh; SymbTab.push_back(s);
  PUTID(s,0); s.name="asinh"; s.type=BUILTIN_TYPE;  s.otype.func1=asinh; SymbTab.push_back(s);
  PUTID(s,0); s.name="atanh"; s.type=BUILTIN_TYPE;  s.otype.func1=atanh; SymbTab.push_back(s);
  PUTID(s,0); s.name="exp";   s.type=BUILTIN_TYPE;  s.otype.func1=exp;   SymbTab.push_back(s);
  PUTID(s,0); s.name="ln";    s.type=BUILTIN_TYPE;  s.otype.func1=log;   SymbTab.push_back(s);
  PUTID(s,0); s.name="log";   s.type=BUILTIN_TYPE;  s.otype.func1=log;   SymbTab.push_back(s);
  PUTID(s,0); s.name="sqrt";  s.type=BUILTIN_TYPE;  s.otype.func1=sqrt;  SymbTab.push_back(s);
  PUTID(s,0); s.name="fabs";  s.type=BUILTIN_TYPE;  s.otype.func1=fabs;  SymbTab.push_back(s);
  PUTID(s,0); s.name="fmod";  s.type=BUILTIN2_TYPE; s.otype.func2=fmod;  SymbTab.push_back(s);
  PUTID(s,0); s.name="int";   s.type=BUILTIN_TYPE;  s.otype.func1=Eint;  SymbTab.push_back(s);
  PUTID(s,0); s.name="floor"; s.type=BUILTIN_TYPE;  s.otype.func1=Efloor; SymbTab.push_back(s);
  PUTID(s,0); s.name="ceil"; s.type=BUILTIN_TYPE;  s.otype.func1=Eceil; SymbTab.push_back(s);
  //
  // Constants
  //
  PUTID(s,0);
  s.name="PI";s.type=CONSTANT_TYPE;   s.value=M_PI;        s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="C";s.type=CONSTANT_TYPE;    s.value=F_C;         s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="R2D";s.type=CONSTANT_TYPE;  s.value=F_R2D;       s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="D2R";s.type=CONSTANT_TYPE;  s.value=F_D2R;       s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="A2R";s.type=CONSTANT_TYPE;  s.value=F_A2R;       s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="S2R";s.type=CONSTANT_TYPE;  s.value=F_S2R;       s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="H2R";s.type=CONSTANT_TYPE;  s.value=F_H2R;       s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="R2H";s.type=CONSTANT_TYPE;  s.value=F_R2H;       s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="kb";s.type=CONSTANT_TYPE;   s.value=F_KB;        s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="PC2M";s.type=CONSTANT_TYPE; s.value=F_PC2M;      s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="PC2LY";s.type=CONSTANT_TYPE;s.value=F_PC2LY;     s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="AU2M";s.type=CONSTANT_TYPE; s.value=F_AU2M;      s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="sigma";s.type=SYS_VAR_TYPE; s.value=DefaultSigma;s.fmt="%E";SymbTab.push_back(s);

  PUTID(s,0);
  s.name="fmt",SETBIT(s.type,SYS_VAR_TYPE|FMT_TYPE); s.fmt=DEFAULT_FMT_STR;SymbTab.push_back(s);
  GlobalDefaultFmt = calcgetSymb("fmt");
  //
  // The following are system variables which can be set by the user.
  // Hence can have associated error and hence should be allocated
  // IDs.  New Calc_Symbol declared in new scopes so that the lists in
  // Calc_Symbol do not carry over in the definition of the next
  // symbol.
  {
    Calc_Symbol s0;
    PUTID(s0,GetNewID());
    s0.name="LATITUDE";s0.type=SYS_VAR_TYPE; s0.value=LATITUDE*F_D2R; s0.fmt="%dms";
    GlobalLat = s0.value; 
    s0.dx.resize(1);s0.DSList.resize(1); s0.dx[0]=0.0; s0.DSList[0]=1.0;
    PUTIDList(s0,s0.ID);
    SymbTab.push_back(s0);
  }
  {
    Calc_Symbol s0;
    PUTID(s0,GetNewID());
    s0.name="LONGITUDE";s0.type=SYS_VAR_TYPE; s0.value=LONGITUDE*F_D2R; s0.fmt="%dms";
    GlobalLong = s0.value; GlobalLongID = s0.ID;
    s0.dx.resize(1);s0.DSList.resize(1); s0.dx[0]=0.0; s0.DSList[0]=1.0;
    PUTIDList(s0,s0.ID);
    SymbTab.push_back(s0);
  }
}
//
//-------------------------------------------------------------------
// The Garbage Collector.  
//
// It emptys the local symbol table (used in sub-program code), resets
// the call frame stack, the VM stack and removes any symbols from the
// global symbol table which are either not used or invalid (e.g. when
// user error occurs during sub-program definition).
//
void CollectGarbage()
{
  //
  // This should be called immediately after the
  // VM program terminates
  //
  EmptyLocalSymbTab();  
  RESET_STACK(FrameStack);
  RESET_STACK(stck);
  //
  // Symbols that remained UNDEF are product of errors in the
  // middle of expression.  Remove them.
  // FSUC, PSUC (Function/Procedures Under Construction) on the
  // symbol table are product of error while building the sub-
  // programs.  Remove them too.
  //
  cleanupSymbTab(SymbTab,OP_ON_TYPE,UNDEF_TYPE);
  cleanupSymbTab(SymbTab,OP_ON_TYPE,FSUC_TYPE);
  cleanupSymbTab(SymbTab,OP_ON_TYPE,PSUC_TYPE);
  //
  // Remove constants which are marked as temporary
  // (==> no VM code is referring to them any more).
  // 
  cleanupSymbTab(ConstTab,OP_ON_REF,CONSTANT_TYPE);
}
//
//-------------------------------------------------------------------
//Report an error by throwing a exception of type ErrorObj.  Also does
//garbage collection first.
//
//   const char *Msg     The NULL terminated error message.
//   const char *ErrType The type of error.  This string is 
//                       printed before the error message.
//   const int &ErrLevel The severity of the error.  This can be
//                       used to make decisions where the execption 
//                       is caught.
//                      
void ReportErr(const char *Msg, const char *ErrType, const int& ErrLevel)
{
  CollectGarbage();
  //
  // The following line was before CollectGarbage().  I think it
  // should be here - but not sure if there are other unintended
  // consqeuences of this.
  //
  sp=InCStmt=InFuncDefn=0;
  if (Msg) throw(ErrorObj(Msg,ErrType,ErrLevel));
}
//
//-------------------------------------------------------------------
// Make a given symbol from the symbol table persistant (i.e., 
// garbage collector will not remove it, even it is not referenced 
// elsewhere).
//   Calc_Symbol* C   Pointer to the symbol to make persistant.
//
void MakePersistant(Calc_Symbol* C)
{
  if (ISSET(C->type,NUMBER_TYPE) || ISSET(C->type,QSTRING_TYPE))
    (C->name[0])=(int)1;
}
//
//-------------------------------------------------------------------
// Convert a given symbol to a number with asscoiated measurement
// error.
//
//   Calc_Symbol& S        The symbol to be converted.
//   float V               The number the given symbol represents.
//   float E               The associate measurement error.
//   char *Fmt             The default format of the number.
//
void MakeANumber(Calc_Symbol &S, float V, float E, char *Fmt)
{
  S.type = NUMBER_TYPE;
  SETVAL(S.value,V,E);
  if (Fmt) S.fmt=Fmt;
  else S.fmt=DEFAULT_FMT;
  S.otype.qstr=NULL;
}
//
//-------------------------------------------------------------------
// Return the integer part of the value and the measurement error.
//
//  edouble x      The input extended number.
//
edouble Eint(edouble x)
{
  edouble t;
  t.setval((int)x.val(),(int)x.rms());
  return t;
}

edouble Efloor(edouble x)
{
  edouble t;
  t.setval(floor(x.val()),floor(x.rms()));
  return t;
}

edouble Eceil(edouble x)
{
  edouble t;
  t.setval(ceil(x.val()),ceil(x.rms()));
  return t;
}
extern ostream OUTPUT;
//
//-------------------------------------------------------------------
//Interface to the fussy interpreter.  Input can be supplied either
//via InStr or Inp file.  Res is the output stream to which output of
//the execution of the fussy program goes.  Output file pointer is
//used for error reporting.
//
//   const char *InStr      If not NULL, read the input fussy script
//                          from.  If NULL, use Inp as the input source
//   ostream &Res           The output stream of the fussy script.
//   FILE *Inp              If not NULL, use this as a the interpreter
//                          input.  If NULL, and InStr is also NULL,
//                          set the input to stdin.
//   FILE *Output           If not NULL, send the error messages here.
//                          Else set the error output stream to cerr.
//
int calc(const char *InStr, ostream &Res, FILE *Inp, FILE *Output)
{
  NUMTYPE R;
  OUTPUT.rdbuf(Res.rdbuf());
  return calc(InStr,&R,Inp,Output);
}
