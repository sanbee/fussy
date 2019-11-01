/* -*- C -*- */
/* $Id: fussy.y,v 1.4 2006/08/05 20:07:41 sbhatnag Exp $ */
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
 *

  History:

    First version.

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in

    Added POSTFIX and PREFIX -- and ++, and GETVAL and GETRMS operators.
 
               Sanjay Bhatnagar, Dec 2003
               sbhatnag@aoc.nrao.edu

    Added the setfmt command and the GlobalDefaultFmt extern pointer.
               Sanjay Bhatnagar, March 10,2006

******************************************************************/
%{
#include <calc.h>
#include <math.h>
#include <calc.h>
#include <stdlib.h>
#include <emath.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <ErrorObj.h>
#include "calc_lex_bison.h"
#include <func.h>
#include <list>
#include <IDResource.h>

#include "defns.h"

#define emit2(c1,c2)       ({emit(c1);emit(c2);})
#define emit3(c1,c2,c3)    ({emit(c1);emit(c2);emit(c3);})

  extern ofstream          ERROUT;
  extern ConstTabType      ConstTab;
  extern IDResource        IDR;
  extern int               ProgBase,sp, Calc_index;
  extern list<Calc_Symbol> SymbTab;
  extern VMac              Prog;
  extern char              *Calc_line;
  extern Stack             stck;
  extern VMac              Prog;           // The VM program
  extern unsigned int      pc;
  extern Calc_Symbol*      GlobalDefaultFmt;
  short int InCStmt=0;
  int       InFuncDefn=0;
  string    Token;
  double    DefaultSigma=1.0;
  double    DesiredSignificance;
  extern    DSType DS;

  //
  //------------------------------------------------------------------------------
  //
  Calc_Symbol *MakeSymb(string& /*Name*/,int Type=VAR_TYPE, const char *Fmt=DEFAULT_FMT.c_str())
    {
      Calc_Symbol *t;
      unsigned int ID;
      t=calcgetSymb((const char *)Token.c_str());
      if (!t)
	{
	  calcput(Token);//Create the symbol in the table
	  t=calcgetSymb(Token.c_str());
	  //
	  // If a new VAR is created in the symbol table, assign it a
	  // new ID as well.
	  //
	  if (ISSET(Type,VAR_TYPE)) 
	    {
#ifdef VERBOSE
	      ERROUT << "Adding " << Name << " to symbol table" << endl;
	      for (IDType ii=0;ii<DS.size();ii++) 
		ERROUT << "DS sizes: " << ii << " " << DS[ii].size() << endl;
#endif
	      //	  ID=IDR.GetNewID();
	      ID=GetNewID();
#ifdef VERBOSE
	      ERROUT << ID << endl;
#endif
	      //	  t->ID.insert(t->ID.end(),ID);
	      // ID--->IDList
	      t->ID=ID;
	      t->IDL.insert(t->IDL.end(),ID);
	      t->DSList.resize(t->IDL.size());
	      t->DSList[0]=1.0;
	      t->dx.resize(t->IDL.size());
	      t->dx[0]=0.0;
	    }
	}
      t->type=Type;
      t->fmt=Fmt;
      return t;
    }
//
//------------------------------------------------------------------------------
//
%}
%union {
  Calc_Symbol *symb;
  //NUMTYPE ConstVal;
  Instruction Inst;
  long int NArg;
}

%token <symb> NUMBER CONSTANT VAR SYS_VAR PARTIAL_VAR BUILTIN BUILTIN2 UNDEF QSTRING FMT GETVAL GETRMS FUNC PROC FSUC PSUC FUNCDECL PROCDECL PP MM
%type  <symb> symbtab_obj defn subprog qstr argnames
%token <Inst> PRINT PRINTN IF WHILE FOR ELSE BREAK RETURN WARRANTY PRTSY PRTID CPRTSY CPRTLSY PRTVM CSS PRTSS HELP
%token <NArg> LE GE PS EQ GT LT POW FINISH CSP AUTO ENDOFINPUT TOD MJD FMJD LST GETDAY GETMONTH GETYEAR SETLONG SETLAT SETGFMT
%type  <NArg> end stmtlist sentence stmt cstmt arglist prtlist farglist subprog_arg
%type  <NArg> cond opt_expr auto
%type  <NArg> if if_stmt while while_stmt for for_stmt stmt_link stmt_body
%right        '=' FMT DOT FORCE
%left         AND
%left         OR 
%left         LE GE EQ GT LT NE PE
%left         '@'
%token        POSTFIX_PP POSTFIX_MM
%left         '+' '-'
%left         '*' '/' '%'
%right        GETVAL GETRMS
%right        '^' POW 
%left         MINUS NOT
%token        PREFIX_PP PREFIX_MM
/*------------------------------------------------------------------*/
%%
//
//-------------------------------------------------------------------
//
sentence:  terminator       {emit(STOP);return 0;}  // End of VM
| stmtlist terminator       {emit(STOP);/*prtVM();cout << pc << endl;*/return 0;}
| error                     {ReportErr(NULL,NULL,0);return SYNTAX_ERROR;}
| ENDOFINPUT                {return END_OF_INPUT;}
;
//
//-------------------------------------------------------------------
//
seperator:  ';'             {}
;
//
//-------------------------------------------------------------------
//
terminator: '\n'            {}
;
//
//-------------------------------------------------------------------
//
cstmt_terminator:  CSP        {}
//| cstmt_terminator CSP        {}  //Uncomment this in case of grammer problems
| cstmt_terminator terminator {}
;
//
//-------------------------------------------------------------------
//
opt_terminator: /* empty */ {}
| opt_terminator CSP        {}
| opt_terminator terminator {}
;
//
//-------------------------------------------------------------------
//
opt_expr: opt_terminator    {//Empty condition is always true (be positive!)
			      Calc_Symbol s;
			      MakeANumber(s,1,0);
                              emit2(cpush,(Instruction)installConst(s,0));
                            }
| expr                      {}
;
//
//-------------------------------------------------------------------
//
ob:  '{'                    {InCStmt++;}
//| ob terminator             {}
| ob CSP                    {}
;
//
//-------------------------------------------------------------------
//
cb:  '}'                    {InCStmt--;}
| terminator cb             {}//InCStmt--;cerr<<InCStmt<<endl;
;
//
//-------------------------------------------------------------------
//
stmt_body:  ob cstmt cb     {$$=Prog.size();}
| stmt seperator            {$$=Prog.size();}
//| stmt                      {$$=Prog.size();} //Uncomment this in case of grammer problems.
| seperator                 {$$=Prog.size();}
;
//
//-------------------------------------------------------------------
//
stmtlist:  stmt             {}
| ob cstmt cb               {}
| stmtlist seperator        {}
| stmtlist seperator stmt   {}
| stmtlist seperator ob cstmt cb 
                            {}
| seperator                 {}
;
//
//-------------------------------------------------------------------
//
stmt_link: opt_terminator   {//
                             // An empty statement in the for/while
                             // loop is TRUE by default.  So "for(;;);" 
                             // is a valid infinite loop
                             //
                              Calc_Symbol S; 
			      MakeANumber(S, 1.0, 0.0);
			      emit2(cpush,(Instruction)installConst(S,0));
                            }
| arglist_components        {/*emit(pop);*/$$=Prog.size();}
| stmt_link ',' arglist_components {emit(pop);$$=Prog.size();}
;
//
//-------------------------------------------------------------------
//
cstmt:  stmtlist                  {}
| cstmt cstmt_terminator          {}  
| cstmt cstmt_terminator stmtlist {}
;
//
//-------------------------------------------------------------------
// Make place for the pointers to the if-body, and else-body emit
//
if: IF                    {$$=Prog.size();emit($1);emit3(STOP,STOP,STOP);}
;
//
//-------------------------------------------------------------------
//
//else: ELSE                {}
//;
//
//-------------------------------------------------------------------
//
if_stmt: if cond stmt_body end   
                          {
                           Prog[$1+1]=(Instruction)$2;
                           Prog[$1+2]=(Instruction)$3; // end
			   Prog[$1+3]=(Instruction)$4; // end
                           $$=Prog.size();;
                          }
| if cond stmt_body end ELSE opt_terminator stmt_body end 
                          {
                           Prog[$1+1]=(Instruction)$2;
                           Prog[$1+2]=(Instruction)$4; // end
			   Prog[$1+3]=(Instruction)$8; // Second end (else-body)
                           $$=Prog.size();
                          }
;
//
//-------------------------------------------------------------------
// Make place for the pointers to the while-condition, and while-body 
// emit
//
while: WHILE              {$$=Prog.size();emit($1);emit2(STOP,STOP);}
;
//
//-------------------------------------------------------------------
//
while_stmt: while cond stmt_body end 
                          {
                           Prog[$1+1]=(Instruction)$2;
			   Prog[$1+2]=(Instruction)$4;
			   $$=Prog.size();
                          }
;
//
//-------------------------------------------------------------------
//
for_stmt: for opt_terminator '(' stmt_link end ';' 
                                 opt_expr end  ';' 
                                 stmt_link end 
                             ')'  
                              opt_terminator 
                              stmt_body 
                              end 
                           {
			    Prog[$1+1]=(Instruction)$5;  // Cond-code
			    Prog[$1+2]=(Instruction)$11; // Body-code
			    Prog[$1+3]=(Instruction)$8;  // Pred-code
			    Prog[$1+4]=(Instruction)$15; // End
			    $$=Prog.size();
			   }
;
//
//-------------------------------------------------------------------
//
// Before starting a sub-program decleration, run the instructions 
// emitted so far starting from ProgBase.  This ensures that any non
// sub-program related instructions in the VM are executed and ProgBase
// reset via the boot().
//
subprog: UNDEF {emit(STOP);Run(Prog);boot();}
//| FSUC {}
//| PSUC {}
;
//
//-------------------------------------------------------------------
//
defn: subprog              {$<symb>1=MakeSymb(Token,FSUC_TYPE);}
      '('                  {
                            $1->otype.FuncStartPC=Prog.size();
			    InFuncDefn=1;
			    InFuncDefn |= PROCRET;
			   }
       farglist            {
	                    SETVAL($1->value,$5,0);
                            emit2(STOP,STOP);
                            Prog[$1->otype.FuncStartPC]=(Instruction)$5;
                           }
      ')' 
      opt_terminator    
      ob cstmt cb          {
                            long int NAutos;

			    $<symb>1->type=0; SETBIT($<symb>1->type,FUNC_TYPE);

			    if (InFuncDefn & (PROCRET)) 
			      {
				$<symb>1->type=0;SETBIT($<symb>1->type,PROC_TYPE);
			      }
			    $$=$1;
			    //
			    // Emit a "return 0;" statement as the
			    // last statement of a function.  This
			    // will ensure that on return, at least
			    // zero is on the stack.  Hence, a
			    // function by default returns a zero.
			    // For a procedure, just emit a return
			    // instruction.
			    // 

			    if (!ISSET($<symb>1->type,FUNC_TYPE))
			      emit(ret);
			    /* if (ISSET($<symb>1->type,FUNC_TYPE)) */
			    /*   { */
			    /* 	Calc_Symbol s; */
			    /* 	MakeANumber(s,0,0); */
			    /* 	emit2(cpush,(Instruction)installConst(s,0)); */
			    /* 	//				emit(ret); */
			    /*   } */
			    /* emit(ret); */
			    //			    else 
			    //			      emit(procret);
			      
                            emit(STOP);

			    ProgBase=Prog.size();

			    NAutos=funcInit($5);
                            Prog[$1->otype.FuncStartPC+1]=(Instruction)NAutos;
			    EmptyLocalSymbTab();
                            InFuncDefn=0;
                           }
;
//
//-------------------------------------------------------------------
//
auto: AUTO                 {if (!(InFuncDefn & ~(PROCRET))) 
                             ReportErr("\"auto\" decleration is allowed only "
				       "inside function/procedure defn",
				       "###Error",0);
                           }

 farglist                  {$$=$3;}
;
//
//-------------------------------------------------------------------
//
return: RETURN             {emit($1);}
| RETURN expr              {InFuncDefn &= ~(PROCRET);emit($1);}
;
//
//-------------------------------------------------------------------
//
stmt: asgn                 {emit(pop);       $$=Prog.size();}
| pasgn                    {emit(pop);       $$=Prog.size();}
| expr                     {emit(printcode); $$=Prog.size();}
| qstr                     {emit(printcode); }
| PRINT  prtlist           {
                            /* Calc_Symbol *s=install("",NUMBER_TYPE,$2,0); */
                            /* emit2(fcpush,(Instruction)s);//No. of args on stack */
                            Calc_Symbol s;
			    MakeANumber(s,$2,0);
			    emit2(fcpush,(Instruction)installConst(s,0));//No. of args on stack 
                            emit($1); 
                            $$=Prog.size();// Print OP code
                           }
| PRINTN  prtlist          {
                            /* Calc_Symbol *s=install("",NUMBER_TYPE,$2,0); */
                            /* emit2(fcpush,(Instruction)s);//No. of args on stack */
                            Calc_Symbol s;
			    MakeANumber(s,$2,0);
			    emit2(fcpush,(Instruction)installConst(s,0));//No. of args on stack 
                            emit($1); 
                            $$=Prog.size();// Print OP code
                           }
| if_stmt                  {$$=Prog.size();}
| while_stmt               {$$=Prog.size();}
| for_stmt                 {$$=Prog.size();}
| VAR '.' '=' FMT          { 
                             mpush($1);
                             $4->units=FMTCOPY; // Change the symb.
                             emit3(vpush,(Instruction)$4,setfmt);
			     emit(pop);
                           }
| defn                     {InFuncDefn=0;ProgBase=Prog.size();/*return 0;*/pc=ProgBase;}
| auto                     {}
| BREAK                    {emit($1);}
| return                   {}
| PROC '(' arglist ')'     {funcrun($1,$3);}
| WARRANTY                 {showWarranty();}
| HELP                     {showHelp();}
| PRTSY                    {prtSymbTab();}
| CPRTSY                   {prtCSymbTab();}
| CPRTLSY                  {prtLocSymbTab();}
| PRTVM                    {prtVM();}
| PRTID                    {prtIDList();}
| CSS                      {cerr << ConstTab.size() << endl;}
| PRTSS                    {
#ifdef VERBOSE 
                              emit(prtStckSize);
#endif
                           }
| FINISH                   {emit(quit);}
;
//
//-------------------------------------------------------------------
//
cond: opt_terminator 
         '(' 
      opt_terminator 
      arglist_components /*expr */
      opt_terminator 
         ')' 
      opt_terminator        {emit(STOP);$$=Prog.size();}
;
//
//-------------------------------------------------------------------
//
end:       /* empty */     {emit(STOP);$$=Prog.size();} 
;
//
//-------------------------------------------------------------------
//
arglist_components: expr   {}
| asgn                     {} // sin(a=1) ==> a=1;sin(a)
| pasgn                    {} // sin(a:=1) ==> a:=1;sin(a)
| qstr                     {}
| FUNC                     {mpush($1);}
| PROC                     {mpush($1);}
;
//
//-------------------------------------------------------------------
//
prtlist_components: arglist_components {}
//| qstr                                 {}
;
//
//-------------------------------------------------------------------
//
argnames: UNDEF            {$$=$1;}
| PARTIAL_VAR              {$$=$1;}
| VAR                      {$$=$1;}
| FUNC                     {$$=$1;}
| PROC                     {$$=$1;}
| CONSTANT                 {$$=$1;}
| QSTRING                  {$$=$1;}
;
//
//-------------------------------------------------------------------
//
subprog_arg: FUNCDECL argnames   {$$=FUNC_TYPE;}
| PROCDECL argnames              {$$=PROC_TYPE;}
;
//
//-------------------------------------------------------------------
//
farglist: /*empty*/          {$$=0;}
| argnames                   {$$=1;LocalSymbInstall(Token.c_str(),$$,VAR_TYPE);}
| subprog_arg                {$$=1;LocalSymbInstall(Token.c_str(),$$,$1);}
| farglist ',' argnames      {$$++;LocalSymbInstall(Token.c_str(),$$,VAR_TYPE);}
| farglist ',' subprog_arg   {$$++;LocalSymbInstall(Token.c_str(),$$,$3);}
;
//
//-------------------------------------------------------------------
//
arglist: /* empty */             {$$=0;}
| arglist_components             {$$=1;}
| arglist ',' arglist_components {$$++;}
;
//
//-------------------------------------------------------------------
//
prtlist: prtlist_components      {$$=1;}
| prtlist ',' prtlist_components {$$++;}
;
//
//-------------------------------------------------------------------
// Make place for the pointers to the various component codes of a 
// for-loop
//
for: FOR                     {
                               $$=Prog.size();emit($1);emit(STOP);
                               emit3(STOP,STOP,STOP);
                             }
;
/* force: SYS_VAR FORCE VAR {mpush($1);mpush($3);emit(assgn);} */
/* | SYS_VAR FORCE NUMBER {mpush($1);emit2(cpush,(Instruction)$3);emit(assgn);} */
/* | SYS_VAR FORCE CONSTANT {mpush($1);emit2(vpush,(Instruction)$3);emit(assgn);} */
/*; */
//
//-------------------------------------------------------------------
//
variable: VAR                {mpush($1);}
| PARTIAL_VAR                {mpush($1);}
;
symbtab_obj:variable         {}
| SYS_VAR                    {mpush($1);}
| NUMBER                     {emit2(cpush,(Instruction)$1);}
| CONSTANT                   {emit2(vpush,(Instruction)$1);}
;
uops: PP variable %prec PREFIX_PP   {emit(pre_incr);}
| variable PP %prec POSTFIX_PP      {emit(post_incr);}
| MM variable %prec PREFIX_MM       {emit(pre_decr);}
| variable MM %prec POSTFIX_MM      {emit(post_decr);}
;
//
//-------------------------------------------------------------------
//
varname: VAR                 {mpush($1);}
| PARTIAL_VAR                {mpush($1);}
| UNDEF                      {
                              if (InFuncDefn & ~(PROCRET)) 
			        {
				  string msg="Undefined variable "
				    + Token + " inside function definition";
				  ReportErr(msg.c_str(), "###Error",0);
			        }
                              $<symb>1=MakeSymb(Token); /* Also put it in the SymbTab */
                              emit2(vpush,(Instruction)$1);
                             }
| qstr                       {}
| UNDEF error                {string msg="Undefined variable \"" + 
			        Token + "\" used in rval";
                              ReportErr(msg.c_str(),"###Error",0);
                             }
;
//
//-------------------------------------------------------------------
//
asgn:  varname '=' expr      {emit(assgn);}
     | varname '=' qstr      {MakePersistant($3);emit(assgn);}
     | varname '=' asgn      {emit(assgn);} //a=b=c=1
     | varname '=' PROC      {
				 yyerror((char *)"Syntax error");
				 
				 string msg="PROC used in an assigment ('=') statement";
				 ReportErr(msg.c_str(),"###Error",ErrorObj::Fatal);
                             }
;
//
//-------------------------------------------------------------------
//
pasgn:  varname PS expr      {emit(passgn);}
        | varname PS qstr    {MakePersistant($3);emit(passgn);}
        | varname PS pasgn   {emit(passgn);} //a=b=c=1
        | varname PS PROC    {
                               yyerror((char *)"Syntax error");

                               string msg="PROC used in an partial-assigment (':=') statement";
			       ReportErr(msg.c_str(),"###Error",ErrorObj::Fatal);
                             }
;

//
//-------------------------------------------------------------------
//
qstr: QSTRING                {
                               $1->fmt=DEFAULT_STRING_FMT;
			       emit2(vpush,(Instruction)$1);
			       $$=$1;
                             }
;
//comp: expr '.' GETVAL {emit(getval);}
//
//-------------------------------------------------------------------
//
expr:  symbtab_obj           {}
| TOD  '(' ')'               {emit(timeofday);}
| MJD  '(' ')'               {emit(mjd);}
| FMJD '(' ')'               {emit(fmjd);}
| LST  '(' ')'               {emit(lst);}
| GETDAY  '(' ')'            {emit(getday);}
| GETMONTH '(' ')'           {emit(getmonth);}
| GETYEAR '(' ')'            {emit(getyear);}
| SETLONG  '(' arglist ')'   {emit(setlong);}
| SETLAT   '(' arglist ')'   {emit(setlat);}
| SETGFMT  '(' qstr ')'      {emit(setgfmt);}
| BUILTIN '(' arglist ')'    {
                               if ($3 != 1) 
                                {
			          string msg=$1->name+" needs exactly 1 argument";
			          ReportErr(msg.c_str(),"###Error",ErrorObj::Recoverable);
			        }
                               emit2(bltin1, (Instruction)$1->otype.func1); 
                             }
| BUILTIN2 '(' arglist ')'   {
                               if ($3 != 2) 
                                {
                                  string msg=$1->name+" needs exactly 2 argument";
			          ReportErr(msg.c_str(),"###Error",ErrorObj::Recoverable);
			        }
			       emit2(bltin2, (Instruction)$1->otype.func2);
                             }
| expr '@' FMT               {ReportErr("Unit conversion not yet implemented","###Error",0);}
| expr '+' expr              {emit(add);}
| expr '-' expr              {emit(sub);}
| expr '*' expr              {emit(mul);}
| expr '/' expr              {emit(vdiv);}
| expr '%' expr              {emit(mod);}
| '-' expr %prec MINUS       {emit(uminus);}
| expr '^' expr              {emit(power);}
| expr POW expr              {emit(power);} // '**' operator
| expr EQ expr               {emit(eq);}
| expr NE expr               {emit(ne);}
| expr LT expr               {emit(lt);}
| expr GT expr               {emit(gt);}
| expr GE expr               {emit(ge);}
| expr LE expr               {emit(le);}
| expr OR expr               {emit(door);}
| expr AND expr              {emit(doand);}
| NOT  expr                  {emit(donot);}
| '(' expr ')'               { }
| expr FMT                   {
                               $2->units=0; //Change only on the stack
			       emit3(vpush,(Instruction)$2,setfmt);
			       //emit3(vpush,(Instruction)$2,setfmt);
                             }
| FUNC '(' arglist ')'       {funcrun($1,$3);}
| FSUC '(' arglist ')'       {funcrun($1,$3);}
| uops                       {}
| expr GETRMS                {emit(getrms);}
| expr GETVAL                {emit(getval);}
;
/*------------------------------------------------------------------*/
%%
#include <lex.yy.c>
#include <yyerror.c>
