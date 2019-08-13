// $Id$
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

    First version.  Functions for printing the VM program as 
    op-codes, and print the internal symbol tables.

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in

    Fixed the prtVM() function to correctly print the name of called
    built-in function.  It also now recognizes the start of the if,
    while or subprogram and print the following constants correctly.

    Changed the prtSymbTab() to print the PC (Program Counter) value
    for the FUNC_TYPE and PROC_TYPE symbols instead of the ID field
    (which is useless for such symbols)
 
               Sanjay Bhatnagar, Dec. 2003
               sbhatnag@aoc.nrao.edu

    Made changes to use the user settable default print format.
    Also added the help on setfmt fussy command.

               Sanjay Bhatangar, March 10, 2006

    Renamed it to prtFuncs.cc (since it contains much more than
    just prtVM() function now.
   
               Sanjay Bhatnagar, Oct. 12, 2006
******************************************************************/
// $Id$
#include <IDResource.h>
#include <calc.h>
#include <fussyparse.hh>
#include <list>
#include <cstring>
extern VMac Prog;
extern list<Calc_Symbol> SymbTab;
extern list<Calc_Symbol> ConstTab;
extern list<Calc_Symbol> LocalSymbTab;
extern IDResource       IDR;                    // The central ID resource
//
//-----------------------------------------------------------------
// Print a string value.
//
void prtString(const char *Str)
{
  int N;
  N=strlen(Str);
  cerr << " \"";
  for(int j=0;j<N;j++)
    {
      switch(Str[j])
	{
	case '\n': cerr <<"\\n"; break;
	case '\t': cerr <<"\\t"; break;
	default: cerr << Str[j];
	}
    }
  cerr<< "\"";
}
int getBuiltinFnName(ostream &os, int PC, void *Ptr)
{
  for(SymbTabType::iterator i=SymbTab.begin();i!=SymbTab.end();i++)
    if ((void *)(i->otype.func1) == Ptr)  
      {
	os << PC << " " << i->name << " " << endl;
	return 1;
      }
  os << PC << " builtin_fn " << endl;
  return 0;
}
//
//-----------------------------------------------------------------
// Print the list of IDs allocated in the central ID List resource.
//
int prtIDList()
{
  IDR.prtIDList();
  return 0;
}
//
//-----------------------------------------------------------------
// Print the virtul machine program.  A useful debugging tool as well
// as a good learning tool of how the program works.
//
void prtVM()
{
  unsigned int i;
  OPCode PrevInst=NULL;
  Calc_Symbol *s;
  string name;
  list<Calc_Symbol>::const_iterator CI;

  for (i=0;i<Prog.size();i++)
    {
      s=(Calc_Symbol *)Prog[i];
      cerr << i;
      if (Prog[i]==STOP) cerr << " STOP" << endl;
      //
      // "./vm.a" is automatically generated (using vmMake) from the
      // forward declerations in calc.h
      //
#include "./vm.a"
      else if ((unsigned long)Prog[i] < Prog.size()) 
	{
	  if (PrevInst == rvpush) cerr << " LSYM <TBD>" << endl;
	  else if ((PrevInst==ifcode) || (PrevInst==forcode))
	    {
	      cerr <<      " JMP " << (long)Prog[i] << endl;i++;
	      cerr << i << " JMP " << (long)Prog[i] << endl;i++;
	      cerr << i << " JMP " << (long)Prog[i] << endl;
	    }
	  else if (PrevInst==whilecode)
	    {
	      cerr <<      " JMP " << (long)Prog[i] << endl;i++;
	      cerr << i << " JMP " << (long)Prog[i] << endl;
	    }
	  else cerr << " CONST " << (long)Prog[i] << endl;
	}
      //      else if (ISSET(s->type,BUILTIN_TYPE))  {cerr << " bultin_fn" << endl;}
      else if (ISSET(s->type,QSTRING_TYPE))  {prtString(s->otype.qstr->c_str());cerr << endl;}
      else if (ISSET(s->type,FMT_TYPE))      {prtString(s->fmt.c_str());cerr << endl;}
      else if (ISSET(s->type,AUTOVAR_TYPE))  {cerr << s->name << endl;}
      else if (ISSET(s->type,PROC_TYPE) || ISSET(s->type,FUNC_TYPE)) 
	cerr << " " << s->name << endl;
      else if (ISSET(s->type,VAR_TYPE)      ||
	       ISSET(s->type,CONSTANT_TYPE) || 
	       ISSET(s->type,NUMBER_TYPE))
	{
	  name.resize(0);
	  //
	  // If it's a number, print it's value.
	  //
	  if (ISSET(s->type,NUMBER_TYPE))  cerr << " " <<  s->value << endl;
	  //
	  // It's either a CONSTANT, VAR, BUILTIN, or BUILTIN2.
	  // Located it in the symbol table and if found, use it's
	  // name for output.
	  //
	  else
	    {
	      for (CI=SymbTab.begin();
		   (CI!=SymbTab.end()) && (CI->name != s->name);
		   CI++); 
	      if (CI != SymbTab.end()) name=CI->name;
	      
	      if (name.size()) cerr << " " << name << endl;
	    }
	}
      else 
	{
	  name.resize(0);
	  for (CI=SymbTab.begin(); (CI!=SymbTab.end()); CI++) 
	    if (CI->otype.func1 == s->otype.func1) 
	      {
		cerr << CI->name << endl;
		break;
	      }

	  if (CI != SymbTab.end()) name=CI->name;

	  if (name.size()) cerr << " " << name << endl;
	  else cerr << " builin_fn" << endl;
	}
      PrevInst=Prog[i];
      //
      // If the instruction was bltin1 or bltin2, the next location
      // has the pointer to the builtin function to be called.  Look 
      // for it's name from the Symbol table.
      //
      if ((Prog[i]==bltin1) || (Prog[i]==bltin2))
	{
	  i++;s=(Calc_Symbol *)Prog[i];
	  getBuiltinFnName(cerr,i,(void *)(s));
	}

    }
}
//template<class T> void prtSymb(SymbTabType::iterator& CI)
template<class T> void prtSymb(T& CI)
{
      unsigned int type;
      type = gettype(CI->type);
      if ((type!=BUILTIN)
	  //	  && (type!=CONSTANT) 
	  && (type!=BUILTIN2))
	{
	  cerr << " Name= "<<CI->name;
	  if ((type==FUNC) || (type==PROC))
	    cerr << "\t  PC=" << CI->otype.FuncStartPC;
	  else
	    {
	      cerr << "\t  ID=";
	      if (ISSET(type,PARTIAL_VAR)) 
		for (IDList::iterator i=CI->IDL.begin(); i!=CI->IDL.end(); i++)
		  cerr  << (*i) << " ";
	      else cerr  << CI->ID;
	      cerr << "\t Value= ";
	      if (ISSET(CI->type,QSTRING_TYPE))
		prtString(CI->otype.qstr->c_str());
	      else if (ISSET(CI->type,FMT_TYPE))
		cerr << CI->fmt << " ";
	      else
		cerr << CI->value << "  ";
	    }
	  cerr << "\t Type=";
	  switch (type)
	    {
	      //
	      // "./type.a" is automatically generated (using typeMake) from
	      // y.tab.h which is generated by bison
	      //
#include "./type.a"
	    }
	}
}
//void  prtSymb<SymbTableType::iterator>;
//
//-----------------------------------------------------------------
// Print the global symbol table (SymbTab) on the stderr.
//
void prtSymbTab()
{
  list<Calc_Symbol>::const_iterator CI;

  for (CI=SymbTab.begin();(CI!=SymbTab.end());CI++)
      prtSymb<list<Calc_Symbol>::const_iterator>(CI);
}
//
//-----------------------------------------------------------------
// Print the table of symbols from the local symbol table (used to hold 
// symbols used in sub-program scopes (functions and procedures).
//
void prtLocSymbTab()
{
  list<Calc_Symbol>::const_iterator CI;

  for (CI=LocalSymbTab.begin(); (CI!=LocalSymbTab.end()); CI++)
    {
      cerr << CI->name << "  Type=";
      switch (CI->type)
	{
	  //
	  // "./type.a" is automatically generated (using typeMake) from
	  // y.tab.h which is generated by bison
	  //
#include "./type.a"
	}
    }
}
//
//-----------------------------------------------------------------
// Print the table of constants (ConstTab).
//
void prtCSymbTab()
{
  list<Calc_Symbol>::const_iterator CI;

  for (CI=ConstTab.begin(); (CI!=ConstTab.end()); CI++)
    {
      cerr << " Value = " ;
      if (ISSET(CI->type,NUMBER_TYPE))       cerr << CI->value << " ID = " << CI->ID << " ";
      else if (ISSET(CI->type,FMT_TYPE))     cerr  << CI->fmt << " ID = " << CI->ID << " ";
      else if (ISSET(CI->type,QSTRING_TYPE)) 
	{
	  prtString(CI->otype.qstr->c_str());
	  cerr << " ID = " << CI->ID << " ";
	}
      cerr << "  Type=";
      switch (gettype(CI->type))
	{
	  //
	  // "./type.a" is automatically generated (using typeMake) from
	  // y.tab.h generated by bison
	  //
#include "./type.a"
	}
#ifdef VERBOSE
      if (CI->type & PARTIALVAR_TYPE) cerr <<"Ah ha! Partial var!!" << endl;
#endif
    }
}
//
//-----------------------------------------------------------------
// Show copyright info.
//
void showCopyright(const char *Msg)
{
  cerr << "   fussy 2.0" << endl
       << 
#include "copyright.inc"
       << endl
       << "   This is free software with ABSOLUTELY NO WARRANTY." << endl;
  if (Msg) cerr << Msg << endl << endl;
}
//
//-----------------------------------------------------------------
// Show warranty info.
//
void showWarranty()
{
#include "./warranty.txt"  // This loads warrantyStr.txt string.

  showCopyright("");
 
  cerr << warrantyStr << endl;
}
//
//-----------------------------------------------------------------
// Show help
//
void showHelp()
{
  string INDENT;
  INDENT = "\t      ";

  cerr << "Variable/function/procedure names can be of any length starting with" << endl;
  cerr << "an alphabet or \'_\'. Compound statements/sub-program bodies are enclosed" << endl;
  cerr << "in \'{\' and \'}\' pairs.  User defined functions can be recursive."<<endl;
  cerr << "Sub-program units (functions/procedures) can be passed as arguments to" << endl;
  cerr << "other sub-program units." <<endl;

  cerr << endl << "Mathematical functions available:" << endl;
  cerr << "\t" << "sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh" << endl;
  cerr << "\t" << "asinh, acosh, atanh, exp, ln, log, fabs, fmod, sqrt, int" << endl;
  cerr << "\t" << "floor, ceil" << endl;

  cerr << endl << "Some useful constants:" << endl;
  cerr << "\t PI: The value of PI" << endl;
  cerr << "\t C: The speed of light in meter/second" << endl;
  cerr << "\t R2D: Factor to convert angles in degree to radian" << endl;
  cerr << "\t A2R: Factor to convert angles in arcsecond to radian" << endl;
  cerr << "\t kb: The Boltzmann constant" << endl;
  cerr << "\t PC2M: Factor to convert from Parsec to meter" << endl;
  cerr << "\t PC2LY: Factor to convert from Parsec to Lightyear" << endl;
  cerr << "\t AU2M: Factor to convert from Astronomical Unit to meter" << endl;
  cerr << "\t sigma: [NOT YET USED] Threshold to use for the result of logical" << endl
       << INDENT << "operators" << endl;
  cerr << endl << "System variables:" << endl;
  cerr << "\t LONGITUDE: Longitude used for astronomical computations" << endl;
  cerr << "\t LATITUDE: Latitude used for astronomical computations" << endl;
  cerr << "\t EPSILON: Upper bound for logical \"zero\" (values less than" << endl
       << INDENT << "this are considered equivalent to zero." << endl << endl;
  cerr << "\t These variables can be set only by special commands" << endl
       << "\t setlong, setlat and seteps (see below)." << endl;

  cerr << endl << "Astronomical functions available:" << endl;
  cerr << "\t" << "time():  returns the current time in the hms format." << endl;
  cerr << "\t" << "lst():   returns the Local Sidereal time in the hms format." << endl;
  cerr << "\t" << "day():   returns the current day." << endl;
  cerr << "\t" << "month(): returns the current month." << endl;
  cerr << "\t" << "year():  returns the current year." << endl;
  cerr << "\t" << "mjd(),fmjd(): returns the current MJD and fractional MJD." << endl;
  cerr << "\t" << "setlong(value): Sets the global variable LONGITUDE" << endl
       << INDENT << "to the given value." << endl;
  cerr << "\t" << "setlat(value): Sets the global variable LATITUDE to" << endl
       << INDENT << "the given value." << endl;
  
  cerr << endl << "Functions to set system variables:" << endl;
  cerr << "\t" << "setfmt(string): Sets the system variable \'fmt\' used as" << endl
       << INDENT << "the default printing format for numerical values to" << endl
       << INDENT << "the given format string." << endl;
  cerr << "\t" << "seteps(value): Sets the global variable EPSILON to" << endl
       << INDENT << "the given value." << endl;

  cerr << endl << "Arithmetic operators:" << endl;
  cerr << "\t" << "+, -, *, \\, ^, **, ++, --" << endl;

  cerr << endl << "Logical operators:" << endl;
  cerr << "\t" << "<, >, <=, >=, =, !=, ||, &&" << endl;

  cerr << endl << "Program control statements: " << endl;
  cerr << "\t " << "if, if-else, while, return, break, " << endl;
  cerr << "\t " << "for(assign;test;increment)"<<endl;

  cerr << endl << "Output:" << endl;
  cerr << "\t " << "print <expr>[,<expr>,...]" << endl
       << INDENT << "The result of <expr> is printed on the STDOUT using" << endl
       << INDENT << "the format associated with it (see below for format)." << endl
       << INDENT << "The result of an expression, if not assigned to another" << endl
       << INDENT << "variable or used in another expression/control statement," << endl
       << INDENT << "is automatically printed on the STDOUT." << endl
       << "\t " << "printn <expr>[,<expr>,...]" << endl
       << INDENT << "Same as print, with a newline appended automatically at the end." << endl
       << "\t " << "Strings:" << endl
       << INDENT << "Strings are defined as a sequence of characters enclosed in" << endl
       << INDENT << "double quotes.  Special characters are included as in" << endl
       << INDENT << "C-strings. E.g. \"This is a string\\n\" will be printed with" << endl
       << INDENT << "a NEWLINE at the end." << endl;

  cerr << endl << "Special operators:" << endl;
  cerr << "\t" << "\':=\': " << "For assignment of partial results/sub-expression " << endl
       << INDENT << "values." << endl;
  cerr << "\t" << "\'pm\': "  << "For associating an error with numerical values." << endl 
       << INDENT << "E.g 10 +/- 1.0 is expressed as 10pm1." << endl;
  cerr << "\t" << "<expr>.rms, <expr>.val: For extracting the associated error " << endl
       << INDENT << "and the value of the expression <expr>.  E.g. x.rms" << endl
       << INDENT << "is the error associated with x while x.val is the " << endl
       << INDENT << "value of x." << endl;
  cerr << "\t" << "<expr>%<format>: Sets the print format of the result of the" << endl
       << INDENT << "expression <expr> to the printf style format <format>. " << endl
       << INDENT << "E.g. x%10.5f will print the value of x as a float in a" << endl
       << INDENT << "10 character field with 5 places after decimal." << endl;
  cerr << "\t" << "<var>.: Operator to set the default print format of a variable." << endl
       << INDENT << "E.g. x.=%7.2f will replace the default printf format (%10.5f)" << endl
       << INDENT << "by %7.2f." << endl << endl;

  cerr << "General commands:" << endl;
  cerr << "\tquit/bye: " << "The only two ways to quit from the interpreter."<<endl
       << INDENT << "Politer (more civlized) of the two commands is more " << endl
       << INDENT << "recommended!" << endl
       << INDENT << "Typing Ctrl-D has no effect and typing Ctrl-C will" << endl
       << INDENT << "attempt to teach a thing or two about life!" << endl;
  cerr << "\tsetfmt: "<< "Set the system parameter \'fmt\' used as the default " << endl
       << INDENT << "format for printing numbers." <<endl;
  cerr << "\tshowvm: "<< "Prints the resident VM program as op-codes." <<endl;
  cerr << "\tshowsym: "<< "Prints the symbol table of variables." << endl;
  cerr << "\tshowcsym: "<< "Prints the symbol table of constants." << endl;
  cerr << "\tshowid: " << "Prints the list of allocated IDs." << endl;
  cerr << "\twarranty: "<< "Prints the warranty information." << endl;
}
