//$Id: vcode.cc,v 1.5 2006/08/07 23:03:52 sbhatnag Exp $
/******************************************************************
 * Copyright (c) 2000-2017, 2018 S.Bhatnagar
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

    LocalSymbTab is converted from a vector to a list.  This was
    necessary for auto variables to be handled properly - particularly
    when they are in nested function/procedure calls.

                    Sanjay Bhatnagar, Nov. 2001 
                    sanjay@ncra.tifr.res.in

    The basic machinery for error and ID propagation is in place.  A
    separate stack is maintained to hold the partial derivatives per
    independent variate in the expression.  Each symbol is tagged with
    a unique ID (and thus identifying it as an independent variate)
    and the IDs of the constituent variates is propagated on the VM
    stack.

                    Sanjay Bhatnagar, Dec. 2001
                    sanjay@ncra.tifr.res.in

    ID propagation and partial derivatives using the chain rule for
    multi-variate case is implemented.  A numeric constant is also
    treated as an independent variate.

                    Sanjay Bhatnagar, Dec. 2001 [It took RK to crack 
                       the earlier implementation in 10sec!]
                    sanjay@ncra.tifr.res.in

    Infrastructure for handling error propagation correctly using user
    defined sub-expression is in place.  All objects on the VM stack
    now potentially carry a list of IDs and associated
    MeasurementErrors and DS values.  vpush() then recreates the VM
    state by pushing the DS values onto appropriate DS stacks and
    filling in the ME values.  With this, the distinction between
    StackType and SymbolType has disappeared (though the difference
    exist in the code - but even that should be cleaned up).  In
    principle, the code can now become much cleaner (in the final
    analysis then, implementing this was a liberating experience after
    all! :)).

    The machinery for sub-program handling (the call() and ret()
    functions) still need modification.  The documentation also needs
    to be augmented - particularly for the fact that changing values
    of variables used earlier in sub-expressions can have unexpected
    results (the author, Yours Truly, himself went on a wild chase
    for a bug which was not there in the first place!).

                    Sanjay Bhatnagar, Oct. 2003
                    sbhatnag@aoc.nrao.edu

    Modified call(), ret(), assgn() and passgn() to use the ID-release
    algorithm which keeps track of the symbol IDs which are either out
    of scope or not in use by any other symbol.  Without this, there
    will be a memory leak via dangling IDs (and associated locations
    in the ME table).
 
                    Sanjay Bhatnagar, Nov. 2003
                    sbhatnag@aoc.nrao.edu

    All QSTRING_TYPE symbols have an ID of zero.  Since such symbols
    never participate in error propagation, they don't need a unique
    ID of their own.  Run time memory requirements with this reduces,
    particularly for programs which have a lot of strings in it.
    
                    Sanjay Bhatnagar, Nov. 2003
                    sbhatnag@aoc.nrao.edu

    All symbols for constants with zero error also have an ID of zero.
    This minimizes runtime memory requirements (further runtime
    performance improvement can be achieved by eliminating unnecessary
    stack operations).

    Added the prefix and postfix ++ and -- VM instructions.  Fixed the
    getval() and getrms() VM instructions.

    Fixed all the comparison operator VM instructions and checked that
    recursion works.  Checked-in test scripts (test.factorial and
    test.laguerre both of which use recursion).

    General cleanup.
    
                    Sanjay Bhatnagar, Dec. 2003
                    sbhatnag@aoc.nrao.edu

     
    sin(x:=0.4pm0.1) kind of statements were not allowed (they should
    be allowed since syntactically ":=" and "=" operaters are treated
    at par).  This was because passgn() was not leaving the LHS on the
    stack (it does now).  pop() had to be also changed to POP the DSs
    associated with a symbol.

                    Sanjay Bhatnagar, Feb 01, 2004 (while waiting at 
                      the airport for a delayed flight out of Phenix!).
                    sbhatnag@aoc.nrao.edu

    Added the setgfmt() VM instruction and associated code. This is 
    called from the fussy command "setfmt(QSTRING)" which sets the
    global numerical print format.  The print format associated with
    symbols takes precedence.

                     Sanjay Bhatnagar, Mar 10, 2006.
******************************************************************/
#include <fstream>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <stdio.h>
#include <calc.h>
#include <tables.h>
#include <fussyparse.hh>
#include <AngFmt.h>
#include <ErrorObj.h>
#include <string>
#include <defns.h>
#include <sys/time.h>
#include <IDResource.h>
#include <set>
#include <algorithm>

#ifdef VMDBGMODE
#define DBG(a)  {cerr << "\t Exe: " << a << " " << pc << endl;}
#else
#define DBG(a)  {}
#endif

#define DEFAULT_TYPE    VAR_TYPE  // Just a convenience
#define OUTSTREAM       stdout
extern ostream OUTPUT;
#define DEFAULT_RETURN  ({return 1;})
extern ofstream         ERROUT;
//
// All the various global symbol tables.
//
extern ConstTabType     ConstTab;       // Holds the constants of a VM program
extern LocalSymbTabType LocalSymbTab;   // Holds the local symbols in a VM scope
extern SymbTabType      SymbTab;        // Holds the global symbols.
extern TmpSymbTabType   TmpSymbTab;     // Holds the temp. symbols.
//
// VM related externs
//
extern FrameStackType   FrameStack;     // Holds the call-frame stack
extern unsigned int     ProgBase;       // Start of "user" mem. in the VM
extern int              GlobalFlag;     // Flag for user interrupt in the world of VM!
extern IDResource       IDR;                    // The central ID resource
//
// VM related global objects
//
DSType                  DS;             // The derivative stack (DS)
VMac                    Prog;           // The VM program
Stack                   stck;           // The VM stack (VMS)
long int            sp=0;           // Stack pointer register
long int            pc=0;           // Program counter register

vector<BASIC_NUM>       MeasurementError;       // The ME table

NUMTYPE                 GlobalLong,GlobalLat;   // Global longitude and latitude
IDType                  GlobalLongID;
NUMTYPE                 Result;                 // The final result of a VM program.
                                                // Also used in fussy.l
Calc_Symbol*            GlobalDefaultFmt;
//
// Some macros for better code readability
//
#define PUSH(s,Item)                   (s. push_back(Item))
#define POP(s)                         (s.pop_back())
#define INIT_SYMB(d,SYMB,FMT,UNITS)    ({d.symb=SYMB;d.fmt=FMT;d.units=UNITS;})
//
// Use this version till stack related bugs are removed!  
// A faster version would be 
//#define TOP(v)  (v.back())
//
template <class X> X& TOP(vector<X>& v) 
{
  if (!v.size()) 
    ReportErr("Illegal operation on stack or symbol table!\n            "
	      "If you did not pass a proc as an argument where a func was\n            " 
	      "expected, this is an internal error!","###Runtime",0);
  return v.back();
}
//
//-------------------------------------------------------------------
// A function to print some debugging information (the sizes of the
// value Derivative Stacks (DS)).
//
void prtIDs(StackType &d)
{
  IDList::iterator iterend=d.symb->IDL.end();
  //  for(IDList::iterator i=d.symb->IDL.begin();i!=d.symb->IDL.end();i++)
  for(IDList::iterator i=d.symb->IDL.begin();i!=iterend;++i)
    ERROUT << *i << " ";
  ERROUT << endl;
}
#ifdef VERBOSE
void prtDS()
{
  ERROUT << "DS size = " << DS.size() << endl;
  for (unsigned int i=0;i<DS.size();i++)  
    if (DS[i].size())
      {
	ERROUT << "DS[" << i << "].size= " << DS[i].size() << ": ";
	for (IDType j=0;j<DS[i].size();j++)
	  ERROUT << DS[i][j] << " ";
	ERROUT << endl;
      }
}
void prtTypes(Calc_Symbol *s)
{
  ERROUT << "A  P  R  C  N  F  Q" << endl;
  ERROUT << ISSET(s->type,AUTOVAR_TYPE) << "  "
	 << ISSET(s->type,PARTIALVAR_TYPE) << "  "
	 << ISSET(s->type,RETVAR_TYPE) << "  "
	 << ISSET(s->type,CONSTANT_TYPE) << "  "
	 << ISSET(s->type,NUMBER_TYPE) << "  "
	 << ISSET(s->type,FMT_TYPE) << "  "
	 << ISSET(s->type,QSTRING_TYPE) << "  "
	 << endl;
}
inline void prtTypes(StackType &d){prtTypes(d.symb);}
//
// Print the number of objects on the DS and the VMS for diagnostics
//
void prtStacks(const char *From=NULL)
{
  int N;
  N=DS.size();
  if (From) ERROUT << From << ":" << endl;
  for(int i=0;i<N;i++)
    ERROUT << setw(5) << i;
    ERROUT << endl;
  for(int i=0;i<N;i++)
    ERROUT << setw(5) << DS[i].size();
  ERROUT << endl;

  ERROUT << "VMS Size = " << stck.size() << endl;
}
//
//-----------------------------------------------------------------
// Function to print some debugging info.
//
void memCheck()
{
  ERROUT << "--------------MEM CHECK----------------" << endl;
  ERROUT << "DS.size = " << DS.size() << endl;
  for (IDType i=0;i<DS.size();i++) 
    ERROUT << " " << DS[i].size();  ERROUT << endl;
  ERROUT << "Stack size = " << stck.size() << endl;
  ERROUT << "ME size = " << MeasurementError.size() << endl;
  ERROUT << "---------------------------------------" << endl;
}
#endif
//
//-----------------------------------------------------------------
//
int prtStckSize()
{
#ifdef VERBOSE
  ERROUT << "VMS.size = "    << stck.size()  << " " << endl;
  for (unsigned int i=0;i<DS.size();i++)
    ERROUT << "DS[" << i << "]=" << DS[i].size() << endl;
#endif
  DEFAULT_RETURN;
}
//
//-------------------------------------------------------------------
// Function to clear the DS associated with a given StackType.
//
//   StackType&  d     The stack type who's DS is cleared.
//
void ClearDS(StackType &d) 
{
  IDList::iterator iterend=d.ID.end();
  //  for(IDList::iterator i=d.ID.begin();i!=d.ID.end();i++) 
  for(IDList::iterator i=d.ID.begin();i!=iterend;++i) 
  {
    if (DS[*i].size() > 0) 
      {
	POP(DS[*i]);
#ifdef VERBOSE
	ERROUT<<"poped " << *i << " ";
#endif
      };
#ifdef VERBOSE
    ERROUT<<endl;
#endif
  }
}
//
//-------------------------------------------------------------------
// Function to clear a given IDList.
//
//   IDList& d     The ID list to be cleared.
//
void ClearIDL(IDList &d) 
{
  d.clear();
}
//
//-------------------------------------------------------------------
//
// Algorithm for releasing resources used by the temporary variables
// once they go out of scope.  This function is called by the
// operators which become the terminal operators (the consumers) for
// the symbols returned from function sub-programs (namely, assgn(),
// printcode(), print(), passgn()), ifcode(), whilecode(), and
// forcode()).
//
// Search for each symbol associated with the ID list of the stack
// object in SymbTab, LocalSymbTab, ConstTab and TmpSymbTab.  First
// three are the tables to hold the persistent symbols.  If the ID was
// found in the TmpSymbTab, delete the associated symbol (it was
// created internally in ret() and hence is out of scope when this
// function is called).  If the ID was NOT found on the persistent
// tables, also release the associated ID (this indicates that the
// tmp. symbol was a "pure temp" symbol and not a partial variable
// constructed using persistent symbols).
//
//   StackType& d        The stack object who's ID(s) are to be released.
//   unsigned int Type   Not used.
//   int ReleaseID       If == 1 (default), then release the ID as well, if
//                       the symbol is not a PersistantVar.
//   int Check           If == 0, uninstall if the symbol is in the
//                       TmpSymbTab.
//                       If == 1, uninstall if it is in TmpSymbTab but not
//                       persistant (i.e. not in SymbTab or ConstTab)
//
void LetGoID(StackType &d, unsigned int TYPE, int ReleaseID=1, 
	     int Check=1, int UnInstall=1)
{
  Calc_Symbol *PersistantVar=NULL, *TmpVar=NULL;

  if (ISSET(d.symb->type,QSTRING_TYPE)) return;
  IDList::iterator iterend=d.ID.end();
  //  for(IDList::iterator i=d.ID.begin();i!=d.ID.end();i++)  
  for(IDList::iterator i=d.ID.begin();i!=iterend;++i)  
    {
      TmpVar = IsIDinTab(*i,1);
#ifdef VERBOSE
      if (TmpVar) 
	ERROUT << "LetGoID: The symbol " << TmpVar 
	       << " is a Tmp var. " << endl;
#endif

      if ((!Check) || TmpVar)
	{
	  PersistantVar = IsIDinTab(*i,0);
#ifdef VERBOSE
	  if (PersistantVar) 
	    ERROUT << "LetGoID: The symbol " << PersistantVar 
		   << " is a persistant var. " << endl;
#endif
	  if (UnInstall) uninstall(*i,TmpSymbTab);
	  if ((!PersistantVar) && ReleaseID) 
	    IDR.ReleaseID((unsigned int)(*i));
	}
      /*
      if (Check && (!TmpVar)) return;
      uninstall(*i,TmpSymbTab);
      if ((!PersistantVar) && ReleaseID) 
	IDR.ReleaseID((unsigned int)(*i));
      */
      /*
      if (Check)
	{
	  if (((TmpVar)))// && (!PersistantVar)))
	    {
#ifdef VERBOSE
	      ERROUT << "LetGoID: ";
#endif
	      uninstall(*i,TmpSymbTab);
	      if ((!PersistantVar) && ReleaseID) 
		IDR.ReleaseID((unsigned int)(*i));
	    }
	}
      else
	{
#ifdef VERBOSE
	      ERROUT << "LetGoID: ";
#endif
	  uninstall(*i,TmpSymbTab);
	  if ((!PersistantVar) && ReleaseID) 
	    IDR.ReleaseID((unsigned int)(*i));
	}
      */
    }
}
//
//-----------------------------------------------------------------
// Function to compute the propagated error in the given symbol
// from stack.
//
// StackType& d   The symbol from the stack for which the 
//                propagated error is computed.
//
BASIC_NUM PropagateError(StackType &d)
{
  BASIC_NUM tdx=0,dx;
#ifdef VERBOSE
  ERROUT << "PropagateError: ID dx ME: " << endl;
#endif
  IDList::iterator iterend=d.ID.end();
  //  for(IDList::iterator j=d.ID.begin();j!=d.ID.end();++j)
  for(IDList::iterator j=d.ID.begin();j!=iterend;++j)
    {
      dx = TOP(DS[*j]); POP(DS[*j]);
#ifdef VERBOSE      
      ERROUT << *j << "  " << dx << "   " << MeasurementError[*j] << endl;
#endif
      dx *= MeasurementError[*j];
      tdx += dx*dx;
    }
  return tdx;
}
//
//-----------------------------------------------------------------
// Function to resize the DS vector to a new length.
//
//   int n   The new length.
//
inline void ResizeDS(int n) {DS.resize(n);}
//
//-----------------------------------------------------------------
// Internal function for printing VM variables.
//
int fussy_fprintf(FILE *s, const char *f, double v)
{
  if (strlen(f)==0)    return fprintf(s,"%f",v);
  
  if (strstr(f,"d")||strstr(f,"x")||strstr(f,"X")||strstr(f,"o")||strstr(f,"O"))
    return fprintf(s,f,(int)v);
  else
    return fprintf(s,f,v);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// Function to print the extended-number associated with a given
// StackType.
//
//   StackType& v     The stack type who's value is printed.
//   ostream& ostr    The output stream to which the formatted
//                    value is printed.
//
int PrintENum(StackType& v, ostream& ostr)
{
  int RET=0;
  if (v.fmt.length() <= 0) RET=-1;
  else if (strstr(v.fmt.c_str(),"hms"))      
    {
      //
      // Print in the hour, min., sec. format (time)
      //
      NUMTYPE t=v.val*F_R2H;
      BASIC_NUM t0=24.0;

      if (t.val() > t0)  ostr << Fmt(t-t0,&v.fmt.c_str()[1]);
      else               ostr << Fmt(t,&v.fmt.c_str()[1]);
      RET=1;
    }
  else if (strstr(v.fmt.c_str(),"dms")) 
    {
      //
      // Print in the degree, min., sec. format (angle)
      //
      NUMTYPE t=v.val*F_R2D;
      BASIC_NUM t0=360.0;

      if (t.val() > t0)  ostr << Fmt(t-t0,"d'\"");
      else               ostr << Fmt(t,"d'\"");
      RET=1;
    }
  else if ((v.fmt=="%b")||(v.fmt=="%B"))
    {
      //
      // Print in bit format
      //
      ostr << Fmt(v.val,v.fmt.c_str());
      RET=1;
    }

  if (RET <= 0)
    {
      string fmt;
      //
      // If v.fmt string is not there, set to default.  Else set to
      // the fmt string attached to the symbol.  This effectively
      // means that the fmt of the symbol takes precedence over the
      // default format.  Constants are always printed in the default
      // format (since they get the default format at their birth and
      // can't be changed for the rest of their lives).
      //
      //      cout << GlobalDefaultFmt->fmt << " " << v.fmt << endl;
      if (RET==-1) fmt = DEFAULT_FMT;  
      else fmt=v.fmt;

      ostr << format(fmt.c_str()) << v.val.val();
      if (v.val.rms()) ostr << " +/- " << format(fmt.c_str()) <<v.val.rms();
    }

  ostr.flush();
  return RET;
}
//
//-----------------------------------------------------------------
// Boot the VM.  Essentially cleanup all stacks and set the program
// counter to the start of the "user" memory (ProgBase).
// 
int boot()
{
  //  ConstTab.erase(ConstTab.begin(),ConstTab.end());
  GlobalFlag=0;
  int N;

  CollectGarbage();
  Prog.resize(ProgBase);  pc=ProgBase; 

  //
  // Clear the VM stack
  //
  N=stck.size();     for (int i=0;i<N;i++) POP(stck);
  //
  // Clear the Derivative stack(s)
  //
  N=DS.size();     
  for (int i=0;i<N;i++) 
    for(IDType j=0;j<DS[i].size();j++) 
      POP(DS[i]);

#ifdef VERBOSE
  ERROUT <<"###Booting the VM DS=" << DS.size() << endl;
  for(size_t i=0;i<DS.size();i++) ERROUT << DS[i].size() << " ";ERROUT << endl;
  ERROUT <<"                  ME=" << MeasurementError.size() <<endl;
  for(size_t i=0;i<MeasurementError.size();i++) 
    ERROUT << MeasurementError[i] << " ";ERROUT << endl;
  ERROUT <<"                 LST=" << LocalSymbTab.size() <<endl;
#endif

  ResizeDS(0);
  MeasurementError.resize(0);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// Run the VM code starting from where the program counter is
// currently pointing.
//
//  VMac& P    The in-memory VM program.
//
NUMTYPE Run(VMac& P)
{
  Result=0;

  if (pc==ProgBase) 
    {
#ifdef VERBOSE
      ERROUT <<"###Starting the VM " << endl;
#endif
      GetNewID(0); // Only resize the DS and ME
    }

  try
    {
      while((P[pc] != STOP))
	{
	  if (GlobalFlag & FLAG_CTRL_C) ReportErr("Interrupted!","###Runtime",0);
	  //i=(*(*P[pc++]))(); // Execute the current instruction and increment the PC
	  (*(*P[pc++]))(); // Execute the current instruction and increment the PC
	}
    }
  catch(ReturnException& E)
    {
      throw(E);
    }

  return Result;
}
//
//-----------------------------------------------------------------
// Emit the ins instruction for the VM
//
//  Instruction ins     The VM instruction to be emitted.
//
Instruction emit(Instruction ins)
{
  Prog.push_back(ins);
  return Prog.back();
}
//
//-----------------------------------------------------------------
// VM instruction: Quit the interpretor
//
int quit()
{
  ExitException e;throw(e);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The pop operation on the VM stack. 
//
int pop()
{
  StackType d;
  DBG("pop");
  d=TOP(stck); POP(stck);
  for (IDList::iterator i=d.symb->IDL.begin();i!=d.symb->IDL.end();++i)
    POP(DS[*i]);
  //  POP(DS[*d.ID.begin()]);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
//
// VM instruction: The push instruction to push a constant on the VMS.
// Used in suitations where error propagation computations are not
// required (like in relational operators, a<b, a>b, etc.).
//
int fcpush()
{
  StackType d;
  Calc_Symbol *t;
  DBG("fcpush");

  t=((Calc_Symbol *)*Prog[pc]);
  d.val   = t->value;
  d.units = t->units;
  d.fmt   = t->fmt;
  pc++;

  d.symb=NULL;

  PUSH(stck,d);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The push instruction to put a constant.
//
int cpush()
{
  StackType d;
  Calc_Symbol *t;
  BASIC_NUM dx;

  DBG("cpush");

  t=((Calc_Symbol *)*Prog[pc]);
  d.val   = t->value;
  d.units = t->units;
  d.fmt   = t->fmt;
  pc++;

  d.symb=t;

  //  cout << "cpush: " << (int)t->name[0] << " " << t->value << endl;
#ifdef VERBOSE
  ERROUT << "cpush ID: "<<t->ID<< " " << d.val << endl;
#endif
  
//   IDType ID;
//   ID = GetNewID();

//   cout << ID << " " << t->value << endl;
//   d.ID.insert(d.ID.end(),ID);
//   d.symb->IDL.insert(d.symb->IDL.end(),ID);

//   MeasurementError[ID]=d.val.rms();
//   dx=1.0;
//   PUSH(DS[ID],dx);
//   PUSH(stck,d);

  d.ID.insert(d.ID.end(),t->ID);
  d.symb->IDL.insert(d.symb->IDL.end(),t->ID);

  MeasurementError[t->ID]=d.val.rms();
  dx=1.0;
  PUSH(DS[t->ID],dx);
  PUSH(stck,d);

#ifdef VERBOSE
      prtStacks("cpush:");
#endif

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
//
// Function push a symbol on the stack.  Emits the VM instructions
// rvpush or vpush followed by the symbol to be pushed.
//
//   Calc_Symbol* S    Pointer to the symbol to pushed on the stack.
//                     Emit rvpush if the symbol is an automatic
//                     variable.  Else emit vpush.
//
int mpush(Calc_Symbol* S)
{
  if (ISSET(S->type, AUTOVAR_TYPE))
    {emit(rvpush);emit((Instruction)(S->units));}
  else  
    {emit(vpush);emit((Instruction)S);}

#ifdef VERBOSE
      prtStacks("mpush:");
#endif

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
//
// VM instruction: To push a variable on the stack.  The pointer to
// the variable pushed on the stack is found in the next location in
// the VM program.
//
int vpush()
{
  StackType d;
  DBG("vpush");
  IDList::iterator i;
  int currentPC;

  currentPC = pc++;
  d.symb  = ((Calc_Symbol*)*Prog[currentPC]);
  d.val   = d.symb->value;
  d.units = d.symb->units;
  d.fmt   = d.symb->fmt;

#ifdef VERBOSE
  if (d.symb) ERROUT << "vpush Types: " << endl; prtTypes(d);
#endif

  ClearIDL(d.ID);

  if ((d.symb!=NULL) && ((!ISSET(d.symb->type, FUNC_TYPE)) && 
			 (!ISSET(d.symb->type, PROC_TYPE))))
    {
      unsigned int k=0;
      for (i=d.symb->IDL.begin();i!=d.symb->IDL.end();++i)
	{
	  d.ID.insert(d.ID.end(),(*i));

#ifdef VERBOSE
	  ERROUT << (*i) << " ";
	  if (d.symb->DSList.size()<k) 
	    ERROUT << d.symb->DSList[k] 
		   << " " << d.symb->dx[k];
	  ERROUT << " " << DS[*i].size() 
		 << endl;
#endif

	  if (d.symb->dx.size())
	    MeasurementError[(*i)]=d.symb->dx[k];//d.val.rms();
	  if (d.symb->DSList.size())
	    PUSH(DS[(*i)],d.symb->DSList[k]);
	  else
	    PUSH(DS[(*i)],0);
	  k++;
	}
    }

#ifdef VERBOSE
  else
    ERROUT <<"SubProg"<<endl;   
#endif

  //  d.ID.push_back(d.symb->ID);
  PUSH(stck,d);

#ifdef VERBOSE
      prtStacks("vpush:");
#endif

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
//
// VM instruction: To push a referenced variable on the stack.  The
// symbol to be pushed in this case is found at an offset in the Local
// Symbol Table.  The offset is in the next location of the VM
// program.  This is required for the automatic symbols (a sub-program
// scope).
//
int rvpush()
{
  StackType d;
  BASIC_NUM dx;
  LocalSymbTabType::const_iterator CI=LocalSymbTab.begin();
  long OffSet;

  DBG("rvpush");

  //OffSet=((int)*Prog[pc++]);
  OffSet=((long)*Prog[pc++]);

  for(unsigned int i=0;i<OffSet+sp;i++) CI++;
  d.symb=(Calc_Symbol *)&(*CI);
  //  d.symb=(Calc_Symbol *)&(*CI++);

  d.val   = d.symb->value;
  d.units = d.symb->units;
  d.fmt   = d.symb->fmt;

#ifdef VERBOSE
  if (ISSET(d.symb->type,PARTIALVAR_TYPE)) 
    ERROUT << "rvpush: Got a PARTIALVAR!" << endl;
  ERROUT << "rvpush ID DS ME: ";
#endif

  OffSet=0;
  if (ISSET(d.symb->type,PARTIALVAR_TYPE))
    {
      for (IDList::iterator i=d.symb->IDL.begin();i!=d.symb->IDL.end();i++)
	{
	  d.ID.insert(d.ID.end(),*i);
	  PUSH(DS[*i],d.symb->DSList[OffSet]);
	  MeasurementError[*i] = d.symb->dx[OffSet];

#ifdef VERBOSE
	  ERROUT << *i << " " << d.symb->DSList[OffSet] 
		 << " " << MeasurementError[*i] << endl;
#endif

	  OffSet++;
	}
    }
  else if ((!ISSET(d.symb->type, FUNC_TYPE)) & (!ISSET(d.symb->type, PROC_TYPE))) 
    {

#ifdef VERBOSE
      ERROUT << "!P "  << d.symb->ID << d.val << endl;
#endif

      d.ID.insert(d.ID.end(),d.symb->ID);
      d.symb->IDL.insert(d.symb->IDL.end(),d.symb->ID);

      MeasurementError[d.symb->ID]=d.val.rms();
      dx=1.0; 
      PUSH(DS[d.symb->ID],dx);
    }

#ifdef VERBOSE
  else 
    ERROUT <<"SubProg"<<endl;   

  ERROUT << endl;
#endif

  PUSH(stck,d);

#ifdef VERBOSE
  if (ISSET(d.symb->type,PARTIALVAR_TYPE)) ERROUT << "rvpush: Got a PARTIALVAR!" << endl;
#endif

#ifdef VERBOSE
      prtStacks("rvpush:");
#endif
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The equal-to operator ("==")
//
int eq()
{
  StackType d1, d2;
  DBG("eq");
  
#ifdef VERBOSE
      prtStacks("eq IN");
#endif

  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);

  //
  // POP one value from DSs corresponding to the ID set for LHS
  // (d1==d2)
  //
  //  if (!ISPERMANENT(d2.symb->type)) 
  ClearDS(d2);  
  //  if (!ISPERMANENT(d1.symb->type)) 
  ClearDS(d1);

  d1.val = (d1.val.val() == d2.val.val());

  IDList::iterator iterend=d1.symb->IDL.end();
  //  for(IDList::iterator i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();i++)
  for(IDList::iterator i=d1.symb->IDL.begin();i!=iterend;++i)
    PUSH(DS[*(i)],0);
    //  PUSH(DS[*(d1.ID.begin())],0);
  PUSH(stck,d1);

#ifdef VERBOSE
      prtStacks("eq OUT");
#endif

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The not-equal-to operator ("!=")
//
int ne()
{
  StackType d1, d2;
  DBG("ne");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);

  //
  // POP one value from DSs corresponding to the ID set for LHS
  // (d1!=d2).
  //
  //  if (!ISPERMANENT(d2.symb->type)) 
  ClearDS(d2); 
  //  if (!ISPERMANENT(d1.symb->type)) 
  ClearDS(d1);

  d1.val = (d1.val.val() != d2.val.val());

  IDList::iterator iterend=d1.symb->IDL.end();
  //  for(IDList::iterator i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();i++)
  for(IDList::iterator i=d1.symb->IDL.begin();i!=iterend;++i)
    PUSH(DS[*(i)],0);
  //  PUSH(DS[*(d1.ID.begin())],0);
  PUSH(stck,d1);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The greater-than operator (">")
//
int gt()
{
  StackType d1, d2;
  DBG("gt");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);

  //
  // POP one value from all the DSs corresponding to all the IDs of
  // the ID set of the LHS (d1>d2)
  //
  //  if (!ISPERMANENT(d2.symb->type)) 
  ClearDS(d2); 
  // if (!ISPERMANENT(d1.symb->type)) 
  ClearDS(d1);

  d1.val = (d1.val.val() > d2.val.val());

  IDList::iterator iterend=d1.symb->IDL.end();
  //  for(IDList::iterator i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();i++)
  for(IDList::iterator i=d1.symb->IDL.begin();i!=iterend;++i)
    PUSH(DS[*(i)],0);
  //  PUSH(DS[*(d1.ID.begin())],0);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The less-than operator ("<")
//
int lt()
{
  StackType d1, d2;
  DBG("lt");

  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);

  //
  // POP one value from all the DSs corresponding to all the IDs in
  // the ID set of the LHS (d1<d2)
  //

#ifdef VERBOSE
  if (d1.symb) 
    ERROUT << "lt(): LHS is permanent =" 
	   << ISPERMANENT(d1.symb->type) << endl;
  if (d2.symb) 
    ERROUT << "lt(): RHS is permanent =" 
	   << ISPERMANENT(d2.symb->type) << endl;
#endif

  //  if (!ISPERMANENT(d2.symb->type)) 
  ClearDS(d2); 
  //  if (!ISPERMANENT(d1.symb->type)) 
  ClearDS(d1);

  d1.val = (d1.val.val() < d2.val.val());

  IDList::iterator iterend=d1.symb->IDL.end();
  //  for(IDList::iterator i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();i++)
  for(IDList::iterator i=d1.symb->IDL.begin();i!=iterend;++i)
    PUSH(DS[*(i)],0);
  //  PUSH(DS[*(d1.ID.begin())],0);
  PUSH(stck,d1);
  //  PUSH(DS[*(d1.ID.begin())],d1.val.rms());
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The greater-than-or-equal-to operator (">=") 
//
int ge()
{
  StackType d1, d2;
  DBG("ge");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);

  //
  // POP one value from all the DSs corresponding to all the IDs of
  // the ID set of the LHS (d1>=d2)
  //
  //  if (!ISPERMANENT(d2.symb->type)) 
  ClearDS(d2); 
  //  if (!ISPERMANENT(d1.symb->type)) 
  ClearDS(d1);

  d1.val = (d1.val.val() >= d2.val.val());

  IDList::iterator iterend=d1.symb->IDL.end();
  //  for(IDList::iterator i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();i++)
  for(IDList::iterator i=d1.symb->IDL.begin();i!=iterend;++i)
    PUSH(DS[*(i)],0);
  //  PUSH(DS[*(d1.ID.begin())],0);
  PUSH(stck,d1);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The less-than-or-equal-to operator ("<=") 
//
int le()
{
  StackType d1, d2;
  DBG("le");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);

  //
  // POP one value from all the DSs corresponding to all the IDs of
  // the ID set of the LHS (d1<=d2)
  //
  //  if (!ISPERMANENT(d2.symb->type)) 
  ClearDS(d2); 
  //  if (!ISPERMANENT(d1.symb->type)) 
  ClearDS(d1);

  d1.val = (d1.val.val() <= d2.val.val());

  IDList::iterator iterend=d1.symb->IDL.end();
  //  for(IDList::iterator i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();i++)
  for(IDList::iterator i=d1.symb->IDL.begin();i!=iterend;++i)
    PUSH(DS[*(i)],0);
  //  PUSH(DS[*(d1.ID.begin())],0);
  PUSH(stck,d1);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The OR operator ("|")
//
int door()
{
  StackType d1, d2;
  DBG("or");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);

  d1.val = ((int)d1.val.val() | (int)(d2.val.val()));
  ClearDS(d2);

  PUSH(stck,d1);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The AND operator ("&")
//
int doand()
{
  StackType d1, d2;
  DBG("and");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);

  d1.val = ((int)d1.val.val() & (int)d2.val.val());
  ClearDS(d2);

  PUSH(stck,d1);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The negation operator ("!")
//
int donot()
{
  StackType d1;
  DBG("not");
  
  d1 = TOP(stck);    POP(stck);
  SETVAL(d1.val,(!d1.val.val()),0);

  PUSH(stck,d1);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The pre-INCREMENT operator ("++x")
//
int pre_incr()
{
  DBG("pre_incr");

  TOP(stck).val = TOP(stck).symb->value += 1;

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The post-INCREMENT operator ("x++")
//
int post_incr()
{
  DBG("post_incr");

  TOP(stck).symb->value += 1;

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The pre-DECREMENT operator ("--x")
//
int pre_decr()
{
  DBG("pre_decr");

  TOP(stck).val = TOP(stck).symb->value -= 1;

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The post-DECREMENT operator ("x--")
//
int post_decr()
{
  DBG("post_decr");

  TOP(stck).symb->value -= 1;

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The ADD operator ("+")
//
int add()
{
  StackType d1, d2;
  IDList::iterator i;
  IDList res;
  BASIC_NUM dx;
  DBG("add");

#ifdef VERBOSE
  ERROUT << "add()" << endl;
#endif

  d2 = TOP(stck);    POP(stck); 
  d1 = TOP(stck);    POP(stck); 

#ifdef VERBOSE
  if (d1.symb) ERROUT << "add() LHS Types: " << endl; prtTypes(d1);
  if (d2.symb) ERROUT << "add() RHS Types: " << endl; prtTypes(d2);
#endif

//   for(i=d1.ID.begin();i!=d1.ID.end();++i) cout << *i << endl;
//   for(i=d2.ID.begin();i!=d2.ID.end();++i) cout << *i << endl;
  //
  // Make set of common IDs
  //
  set_intersection(d1.ID.begin(),d1.ID.end(),
		   d2.ID.begin(),d2.ID.end(),
		   inserter(res, res.end()));
  //
  // Computation of partial derivatives w.r.t. independent variates
  //
  IDList::iterator iterend=res.end();
  //  for(i=res.begin();i!=res.end();++i)
  for(i=res.begin();i!=iterend;++i)
    {
      //
      // Remove this checking once it's all debugged!
      //
      if (DS[*i].size()>1)
	{
	  //	  dx=0;
	  dx = TOP(DS[*i]);   POP(DS[*i]);
	  //	  dx+= TOP(DS[*i]);   POP(DS[*i]);
	  TOP(DS[*i]) += dx;
	  //	  PUSH(DS[*i],dx);
	}
      else
	ReportErr("Internal error in operator+","###Error",0);
    }


  res.clear();
  set_union(d1.ID.begin(),d1.ID.end(),
	    d2.ID.begin(),d2.ID.end(),
	    inserter(res, res.end()));

  d1.ID=res;

  d1.val += d2.val;

  PUSH(stck,d1);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The SUBTRACT operator ("-")
//
int sub()
{
  StackType d1, d2;
  IDList::iterator i;
  IDList res;
  BASIC_NUM dx;
  DBG("sub");

  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);
  //
  // Make set of common IDs
  //
  set_intersection(d1.ID.begin(),d1.ID.end(),
		   d2.ID.begin(),d2.ID.end(),
		   inserter(res, res.end()));
  //
  // Computation of partial derivatives w.r.t. independent variates
  //
  IDList::iterator iterend=res.end();
  //  for(i=res.begin();i!=res.end();++i)
  for(i=res.begin();i!=iterend;++i)
    {
      //
      // Remove this checking once it's all debugged!
      //
      if (DS[*i].size()>1)
	{
	  dx=0;
	  dx-= TOP(DS[*i]);   POP(DS[*i]);
	  //	  dx+= TOP(DS[*i]);   POP(DS[*i]);
	  //	  PUSH(DS[*i],dx);
	  TOP(DS[*i]) += dx;
	}
      else
	ReportErr("Internal error in operator-","###Error",0);
    }
  //
  // Construct a set of unique IDs among the two operands
  //  
  res.clear();
  set_symmetric_difference(d1.ID.begin(), d1.ID.end(),
			   d2.ID.begin(), d2.ID.end(),
			   inserter(res,res.end()));
  for (i=res.begin();i!=res.end();++i) 
    if (binary_search(d2.ID.begin(),d2.ID.end(),*i)) 
      {
	TOP(DS[*i]) *= -1;
	//	dx = -TOP(DS[(*i)]);   POP(DS[(*i)]);
	//	PUSH(DS[*i],dx);
      }
  //
  // Set the ID list of the result to the union of the constituent IDs
  //
  res.clear();
  set_union(d1.ID.begin(),d1.ID.end(),
	    d2.ID.begin(),d2.ID.end(),
	    inserter(res, res.end()));
  d1.ID=res;

  d1.val -= d2.val;
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The unary minus operator ("-")
//
int uminus()
{
  StackType d;
  IDList::iterator i;

  DBG("uminus");

  d =  TOP(stck);     POP(stck);
  IDList::iterator iterend=d.ID.end();
  //  for(i=d.ID.begin();i!=d.ID.end();++i)
  for(i=d.ID.begin();i!=iterend;++i)
    {
      TOP(DS[*i]) *= -1;
      //      BASIC_NUM dx;
      //      dx = TOP(DS[*i]);   POP(DS[*i]);
      //      PUSH(DS[*i],-dx);
    }
  d.val = -d.val;
  PUSH(stck,d);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The multiplication operator ("*")
//
int mul()
{
  StackType d1, d2;
  BASIC_NUM d1x,d2x;
  IDList res;
  IDList::iterator i;

  DBG("mul");
#ifdef VERBOSE
  prtStacks("mul in");
  ERROUT << "mul() " << endl;
#endif
  d2  = TOP(stck);    POP(stck);
  d1  = TOP(stck);    POP(stck);


#ifdef VERBOSE
  if ((d2.symb!=NULL) && (d1.symb!=NULL))
    {
      ERROUT << "mul: d1.val, d2.val: "<<d1.val<< " " << d2.val << endl;
      ERROUT << "mul: LHS Types: " << endl; prtTypes(d1);
      ERROUT << "mul: RHS Types: " << endl; prtTypes(d2);
    }
  ERROUT << "D1 IDs: ";
  for (i=d1.ID.begin();i!=d1.ID.end();++i) ERROUT << *i << " ";
  ERROUT << endl;
  ERROUT << "D2 IDs: ";
  for (i=d2.ID.begin();i!=d2.ID.end();++i) ERROUT << *i << " ";
  ERROUT << endl;
#endif

  //NEW

  //
  // Construct a set of IDs common to both the operands
  //
  set_intersection(d1.ID.begin(), d1.ID.end(),
		   d2.ID.begin(), d2.ID.end(),
		   inserter(res,res.end()));
  //
  // POP a value from all the DSs corresponding to IDs in the common
  // set.
  //

#ifdef VERBOSE
  prtStacks("mul common");
  //  cerr << "Common IDS: ";
#endif

  for (i=res.begin();i!=res.end();++i) 
    {
      //      cerr << setw(3) << *i;
      d2x = TOP(DS[(*i)]);   POP(DS[(*i)]);
      d1x = TOP(DS[(*i)]);  // POP(DS[(*i)]);
      //
      // Push the derivative on all the DSs corresponding to IDs in
      // the common set.
      //
      //      PUSH(DS[(*i)],d1x*d2.val.val()+d2x*d1.val.val());
      TOP(DS[*i]) = d1x*d2.val.val()+d2x*d1.val.val();
    }

#ifdef VERBOSE
  ERROUT << endl;
#endif

  //
  // Construct a set of unique IDs between the two operands
  //  
  res.clear();
  set_symmetric_difference(d1.ID.begin(), d1.ID.end(),
			   d2.ID.begin(), d2.ID.end(),
			   inserter(res,res.end()));
  //
  // Push the corresponding derivatives on all the DSs corresponding
  // to all the IDs in the unique ID set.
  //
#ifdef VERBOSE
  prtStacks("mul unique");
  ERROUT << "Unique IDs: ";
#endif

  for (i=res.begin();i!=res.end();++i)
    {
#ifdef VERBOSE
      ERROUT << setw(3) << *i;
#endif
      d1x = TOP(DS[(*i)]);   //POP(DS[(*i)]);
      if (binary_search(d1.ID.begin(),d1.ID.end(),*i)) 
	//	PUSH(DS[*i],d1x*d2.val.val()); //d1x*d2
	TOP(DS[*i])=d1x*d2.val.val(); //d1x*d2
      else
	//	PUSH(DS[*i],d1x*d1.val.val()); //d2x*d1
	TOP(DS[*i]) = d1x*d1.val.val(); //d2x*d1
    }

#ifdef VERBOSE
  ERROUT << endl;
#endif

  //
  // Make a list of the union of the ID sets of both the operands and
  // set the resultant as the ID set of the result (d1).
  //
  res.clear();
  set_union(d1.ID.begin(), d1.ID.end(),
	    d2.ID.begin(), d2.ID.end(),
	    inserter(res,res.end()));

#ifdef VERBOSE
  for (i=res.begin(); i!=res.end(); ++i) 
    ERROUT << "All: " << (*i);  ERROUT << endl;
#endif

  d1.ID=res;

#ifdef VERBOSE
  for (unsigned int j=0;j<DS.size();j++)
    if (DS[j].size() > 0) ERROUT << j << ": " << TOP(DS[j]) << endl;
#endif
  
  d1.val *= d2.val;

#ifdef VERBOSE
  ERROUT << "mul: " << d1.val << endl;
#endif

  PUSH(stck,d1);

#ifdef VERBOSE
  prtStacks("mul out");
#endif

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The division operator ("/")
//
int vdiv()
{
  StackType d1, d2;
  BASIC_NUM d1x, d2x;
  IDList res;
  IDList::iterator i;

  DBG("div");

  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);
  if (d2.val.val()==0) 
    ReportErr("Division by zero","###Runtime error",0);
  //NEW
  //
  // Construct a set of IDs common to both the operands
  //
  set_intersection(d1.ID.begin(), d1.ID.end(),
		   d2.ID.begin(), d2.ID.end(),
		   inserter(res,res.end()));
  //
  // POP a value from all the DSs corresponding to IDs in the common
  // set.
  //
  for (i=res.begin();i!=res.end();++i) 
    {
      d2x = TOP(DS[(*i)]);   POP(DS[(*i)]);
      d1x = TOP(DS[(*i)]);   //POP(DS[(*i)]);
      //
      // Push the derivative on all the DSs corresponding to IDs in
      // the common set.
      //
      // dx = (R*dxL - L*dxR)/(R*R)
      //
      //      PUSH(DS[(*i)],(d2.val.val()*d1x-d1.val.val()*d2x)/(d2.val.val()*d2.val.val()));
      TOP(DS[(*i)]) = (d2.val.val()*d1x-d1.val.val()*d2x)/(d2.val.val()*d2.val.val());
    }
  //
  // Construct a set of unique IDs among the two operands
  //  
  res.clear();
  set_symmetric_difference(d1.ID.begin(), d1.ID.end(),
			   d2.ID.begin(), d2.ID.end(),
			   inserter(res,res.end()));
  //
  // Push the corresponding derivatives on all the DSs corresponding
  // to all the IDs in the unique ID set.
  //
  for (i=res.begin();i!=res.end();++i)
    {
      d1x = TOP(DS[(*i)]);   
      //POP(DS[(*i)]);
      if (binary_search(d1.ID.begin(),d1.ID.end(),*i)) 
	//	PUSH(DS[*i],d1x/d2.val.val()); //dxL/R
	TOP(DS[*i])=d1x/d2.val.val(); //dxL/R
      else
	//	PUSH(DS[*i],-(d1.val.val()*d1x)/
	//	     (d2.val.val()*d2.val.val())); //-L*dxR/(R*R)
	TOP(DS[*i]) = -(d1.val.val()*d1x)/(d2.val.val()*d2.val.val());
    }
  //
  // Make a list of the union of the ID sets of both the operands and
  // set the resultant as the ID set of the result (d1).
  //
  res.clear();
  set_union(d1.ID.begin(), d1.ID.end(),
	    d2.ID.begin(), d2.ID.end(),
	    inserter(res,res.end()));
  d1.ID=res;
  //
  // Push the value on the VM stack
  //
  d1.val /= d2.val;
  PUSH(stck,d1);
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The to-the-power operator ("^")
//
int power()
{
  StackType d1, d2;
  BASIC_NUM d1x,d2x,t;
  IDList res;
  IDList::iterator i;
  int Sign;

  DBG("power");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);
  

  if (!(d1.val.val()==0))
    {    
      //NEW
      //
      // Construct a set of IDs common to both the operands
      //
      set_intersection(d1.ID.begin(), d1.ID.end(),
		       d2.ID.begin(), d2.ID.end(),
		       inserter(res,res.end()));
      //
      // POP a value from all the DSs corresponding to IDs in the
      // common set.
      //
      t= pow(d1.val.val(),d2.val.val());
      Sign = (t < 0)?-1:1;

      for (i=res.begin();i!=res.end();++i) 
	{
	  d2x = TOP(DS[(*i)]);   POP(DS[(*i)]);
	  d1x = TOP(DS[(*i)]);   //POP(DS[(*i)]);
	  //    }
	  //
	  // Push the derivative on all the DSs corresponding to all
	  // the IDs in the common set.
	  //
	  // dx = L^R*((R/L)*dxL+log(L)*dxR)
	  //
	  //	  PUSH(DS[(*i)],
	  //	       t*((d2.val.val()/d1.val.val())*d1x + 
	  //	       Sign*log(fabs(d1.val.val()))*d2x));
	  TOP(DS[(*i)]) = t*((d2.val.val()/d1.val.val())*d1x + 
			     Sign*log(fabs(d1.val.val()))*d2x);
	}
      //
      // Construct a set of unique IDs among the two operands
      //  
      res.clear();
      set_symmetric_difference(d1.ID.begin(), d1.ID.end(),
			       d2.ID.begin(), d2.ID.end(),
			       inserter(res,res.end()));
      //
      // Push the corresponding derivatives on all the DSs
      // corresponding to all the IDs in the unique ID set.
      //
      for (i=res.begin();i!=res.end();++i)
	{
	  d1x = TOP(DS[(*i)]);   //POP(DS[(*i)]);
	  if (binary_search(d1.ID.begin(),d1.ID.end(),*i)) 
	    //	    PUSH(DS[*i],t*(d2.val.val()/d1.val.val())*d1x);  //dxL*(L^R)*(R/L)
	    TOP(DS[*i])=t*(d2.val.val()/d1.val.val())*d1x;  //dxL*(L^R)*(R/L)
	  else
	    //	    PUSH(DS[*i],t*Sign*log(fabs(d1.val.val()))*d1x); //(L^R)*log(L)*dxR
	    TOP(DS[*i])=t*Sign*log(fabs(d1.val.val()))*d1x; //(L^R)*log(L)*dxR
	}
      //
      // Make a list of the union of the ID sets of both the operands
      // and set the resultant as the ID set of the result (d1).
      //
      res.clear();
      set_union(d1.ID.begin(), d1.ID.end(),
		d2.ID.begin(), d2.ID.end(),
		inserter(res,res.end()));
      d1.ID=res;
      //
      // Push the value on the VM stack
      //
      d1.val = pow((NUMTYPE)d1.val,(NUMTYPE)d2.val);
      PUSH(stck,d1);
    }
  else
    ReportErr("Partial derivative undefined for 0^x!","###Informational",0);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The integer modouls operator ("%")
//
int mod()
{
  StackType d1, d2;
  
  DBG("mod");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);
  SETVAL(d1.val,((int)d1.val.val() % (int)d2.val.val()),0);
  ClearDS(d2);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Get time of the day
//
int getday()
{
  StackType d1;

  struct timeval tv;
  //  struct timezone tz;
  struct tm *tm;
  time_t t;
  
  DBG("day");
  INIT_SYMB(d1,NULL,"%f",U_DAY);
  
  if (gettimeofday(&tv,NULL)==-1)
    ReportErr("Could not get time of the day","###Error",0);
  time(&t);  tm = localtime(&t);
  d1.val = tm->tm_mday;

  IDType i;
  d1.symb = makeTmpSymb(1,RETVAR_TYPE);
  i=*d1.symb->IDL.begin();
  d1.ID.insert(d1.ID.end(),i);
  d1.type=RETVAR_TYPE;

  PUSH(DS[i],1.0);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Get the current month
//
int getmonth()
{
  StackType d1;

  struct timeval tv;
  struct tm *tm;
  time_t t;
  
  DBG("month");
  INIT_SYMB(d1,NULL,"%f",U_MONTH);
  
  if (gettimeofday(&tv,NULL)==-1)
    ReportErr("Could not get time of the day","###Error",0);
  time(&t);  tm = localtime(&t);
  d1.val = tm->tm_mon+1;
  
  IDType i;
  d1.symb = makeTmpSymb(1,RETVAR_TYPE);
  i=*d1.symb->IDL.begin();
  d1.ID.insert(d1.ID.end(),i);
  d1.type=RETVAR_TYPE;

  PUSH(DS[i],1.0);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Get the current year
//
int getyear()
{
  StackType d1;

  struct timeval tv;
  struct tm *tm;
  time_t t;
  
  DBG("year");
  INIT_SYMB(d1,NULL,"%f",U_YEAR);
  
  if (gettimeofday(&tv,NULL)==-1)
    ReportErr("Could not get time of the day","###Error",0);
  time(&t);  tm = localtime(&t);
  d1.val = tm->tm_year+1900;
  
  IDType i;
  d1.symb = makeTmpSymb(1,RETVAR_TYPE);
  i=*d1.symb->IDL.begin();
  d1.ID.insert(d1.ID.end(),i);
  d1.type=RETVAR_TYPE;

  PUSH(DS[i],1.0);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Get time-of-the-day.
//
int timeofday()
{
  StackType d1;
  struct timeval tv;

  //  struct timezone tz;
  struct tm *tm;
  time_t t;
  
  DBG("timeofday");
  INIT_SYMB(d1,NULL,"%hms",U_RADIAN);
  
  //  if (gettimeofday(&tv,&tz)==-1)
  if (gettimeofday(&tv,NULL)==-1)
    ReportErr("Could not get time of the day","###Error",0);
  time(&t);  tm = localtime(&t);
  
  d1.val.setval((((tm->tm_sec+tv.tv_usec*1E-6)/60.0+tm->tm_min)/60.0 +
		 tm->tm_hour)*(F_H2R),0.0);
  
  IDType i;
  d1.symb = makeTmpSymb(1,RETVAR_TYPE);
  i=*d1.symb->IDL.begin();
  d1.ID.insert(d1.ID.end(),i);
  d1.type=RETVAR_TYPE;

  PUSH(DS[i],1.0);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Get the Modified Julian Day (MJD).
//
int mjd()
{
  StackType d1;
  int stat;
  double mjd;
  struct timeval tv;
  struct tm *tm;
  time_t t;
  
  DBG("mjd");
  
  INIT_SYMB(d1,NULL,"%f",U_DAY);
  
  if (gettimeofday(&tv,NULL)==-1)
    ReportErr("Could not get time of the day","###Error",0);
  
  time(&t);  tm = localtime(&t);
  
  tm->tm_year+=1900;  tm->tm_mon++;
  
  slacaldj_(&(tm->tm_year), &(tm->tm_mon), &(tm->tm_mday), &mjd, &stat);
  
  d1.val = mjd;
  IDType i;
  d1.symb = makeTmpSymb(1,RETVAR_TYPE);
  i=*d1.symb->IDL.begin();
  d1.ID.insert(d1.ID.end(),i);
  d1.type=RETVAR_TYPE;
  
  PUSH(DS[i],1.0);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Get the fractional MJD
//
int fmjd()
{
  StackType d1;
  int stat;
  double mjd,fmjd;
  struct timeval tv;
  struct tm *tm;
  time_t t;
  
  DBG("fmjd");
  
  INIT_SYMB(d1,NULL,"%f",U_DAY);
  
  if (gettimeofday(&tv,NULL)==-1)
    ReportErr("Could not get time of the day","###Error",0);
  
  time(&t);  tm = localtime(&t);
  
  fmjd = tm->tm_sec+(tm->tm_min+tm->tm_hour*60.0)*60.0+tv.tv_usec*1E-6;
  
  tm->tm_year+=1900;  tm->tm_mon++;
  
  slacaldj_(&(tm->tm_year), &(tm->tm_mon), &(tm->tm_mday), &mjd, &stat);
  
  d1.val = (mjd+fmjd*0.0000115740740);

  IDType i;
  d1.symb = makeTmpSymb(1,RETVAR_TYPE);
  i=*d1.symb->IDL.begin();
  d1.ID.insert(d1.ID.end(),i);
  d1.type=RETVAR_TYPE;
  
  PUSH(DS[i],1.0);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Get the Local Siderial Time (LST).
//
int lst()
{
  StackType d1;

  int stat;
  double mjd,fmjd;
  struct timeval tv;
  struct timezone tz;
  struct tm *tm;
  time_t t;
  
  DBG("lst");
  
  INIT_SYMB(d1,NULL,"%hms",U_RADIAN);
  
  if (gettimeofday(&tv,&tz)==-1)
    ReportErr("Could not get time of the day","###Error",0);
  
  time(&t);  tm=gmtime(&t);
  
  fmjd = tm->tm_sec+(tm->tm_min+tm->tm_hour*60.0)*60.0+tv.tv_usec*1E-6;
  
  tm->tm_year+=1900;  tm->tm_mon++;
  
  slacaldj_(&(tm->tm_year), &(tm->tm_mon), &(tm->tm_mday), &mjd, &stat);
  fmjd=(mjd+fmjd*0.0000115740740);
  d1.val = slagmst_(&fmjd);
  d1.val = d1.val+GlobalLong;
  //  d1.type = 0;
  //  SETBIT(d1.type,RETVAR_TYPE);

  //
  // Construct a new symbol of type RETVAR_TYPE
  //
  IDType i;
  d1.symb = makeTmpSymb(0,PARTIALVAR_TYPE);
  //  d1.type = d1.symb->type=PARTIALVAR_TYPE;
  d1.symb->IDL.insert(d1.symb->IDL.end(),GlobalLongID);
  i=*d1.symb->IDL.begin();
  d1.ID.insert(d1.ID.end(),i);
  d1.symb->dx.resize(1); 
  MeasurementError[i]=d1.symb->dx[0] = d1.val.rms();
  d1.symb->DSList.resize(1); d1.symb->DSList[0] = 1.0;
  
  d1.type=RETVAR_TYPE;

  PUSH(DS[i],1.0);
  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
//
// VM instruction: Set the internal value of Longitude.  The user
// supplied argument is on the stack (in radians).  So set it's value
// in GlobalLong as well as in the symbol named "LONGITUDE" in the
// symbol table.
//
int setlong()
{
  /*
  StackType d;
  Calc_Symbol *s;
  
  d=TOP(stck);
  // POP(stck); // This is not required since setlat() instruction
                // returns the value it has set (which is left on 
                // the stack.

  //
  // Anyway...so get the argument from the top of the stack.  Get the
  // pointer to the SYS_VAR "LONGITUDE".  Set it, and a global variable
  // to the user supplied arg.  And that's it!
  //
  s=calcgetSymb("LONGITUDE");
  GlobalLong = d.val.val();
  s->value.setval(GlobalLong,0);
  */
  //  calcgetSymb("LONGITUDE")->value.setval(GlobalLong=TOP(stck).val.val(),0);
  Calc_Symbol *s;
  s=calcgetSymb("LONGITUDE");s->value=GlobalLong=TOP(stck).val;
  s->dx[0]=s->value.rms();

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
//
// VM instruction: Set the internal value of Latitude.  The user
// supplied argument is on the stack (in radians).  So set it's value
// in GlobalLat as well as in the symbol named "LATITUDE" in the
// symbol table.
//
int setlat()
{
  //
  // Get the argument from the top of the stack.  Get the pointer to
  // the SYS_VAR "LATITUDE".  Set it, and a global variable to the
  // user supplied arg.  And that's it!
  //
  Calc_Symbol *s;
  s=calcgetSymb("LATITUDE");s->value=GlobalLat=TOP(stck).val;
  s->dx[0]=s->value.rms();

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
//
// VM instruction: Set the internal value of the default print format
// numerical values.  The user supplied argument is on the stack.  So
// set it's value in GlobalDefaultFmt.  This is pointer to the symbol
// named "fmt" in the symbol table.
//
int setgfmt()
{
  GlobalDefaultFmt->fmt = *(TOP(stck).symb->otype.qstr);
  //
  // Setting the return value to a message which then gets printed by
  // printcode().
  //
  *(TOP(stck).symb->otype.qstr)=string("###Informational: Default Format = ")
    +string(*(TOP(stck).symb->otype.qstr))+string("\n");;
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
//
int printEngine(const int doNewLine)
{
  vector<StackType> d;
  int n;

  DBG("print");

#ifdef VERBOSE
  ERROUT << "print:" << endl;
#endif

  //
  // The top of the VM stack has the number of arguments to the print
  // command.
  //
  d.resize(1);
  d[0]=TOP(stck);    POP(stck);
  n=(int)d[0].val.val();
  d.resize(n);
  //
  // The arguments to the print statement are sitting on the stack in
  // the reverse order!  So pop them all into an array of StackType
  // first.
  //
  // Since print is also a terminal point in parsing, compute the
  // final error from the partial derivatives on the appropriate DS
  // stacks and the MeasurementError table.
  //
  for (int i=0;i<n;i++)
    {
      d[i]=TOP(stck);   POP(stck);
      d[i].val.setval(d[i].val.val(),sqrt(PropagateError(d[i])));
    }
  //
  // Now print the poped objects in the reverse order.
  //
  for (int i=n-1;i>=0;i--)
    {
      //
      // Print a QSTRING
      //

#ifdef VERBOSE
      if (d[i].symb) 
	ERROUT << "print: STRINGTYPE=" 
	       << ISSET(d[i].symb->type,QSTRING_TYPE) << endl;
#endif

      if (d[i].symb && (ISSET(d[i].symb->type,QSTRING_TYPE)))
	{
	  OUTPUT << d[i].symb->otype.qstr->c_str();
	  if (d[i].symb->name.size()==0) uninstall(d[i].symb);
	}
      /*
      else if (d[i].symb && (ISSET(d[i].symb->type,FMT_TYPE)))
	{
	  OUTPUT << d[i].symb->fmt;
	}
      */
      else //!(d[i].symb->type==QSTRING))
	{
	  PrintENum(d[i],OUTPUT);
	  LetGoID(d[i],RETVAR_TYPE,1,0);
	}
    }
  if (doNewLine) OUTPUT << endl;

  return 1;
}
//
// VM instruction: The VM print instruction.
//
// print works by collecting the arglist from the stack into a
// temporary array and then printing the array elements in the
// reverse order.  This has to be done since the arglist exists
// in the reverse order on the stack.   Top of the stack will have
// the number of items on the stack to be poped for print.
//
// This should be improved to operate from the stack WITHOUT copying
// the stack elements into a temp. array.  In case of large argument
// list, this will be inefficient.
//
int print()
{
  return printEngine(0);
}
//
// VM instruction: The VM printn instruction.
//
//-----------------------------------------------------------------
int printn()
{
  return printEngine(1);
}
//
//-----------------------------------------------------------------
// VM instruction: VM instruction to call a built-in function with 
// one argument.
//
int bltin1()
{
  StackType d;
  BASIC_NUM dx;
  DBG("bltin1");
  
  d = TOP(stck);   POP(stck);
  d.val = (*(NUMTYPE (*)(NUMTYPE))(Prog[pc++]))(d.val);

  for (IDList::iterator i=d.ID.begin();i!=d.ID.end();++i)
    {
      dx  = TOP(DS[*i]); POP(DS[*i]); 
      dx *= d.val.rms();
      PUSH(DS[*i],dx);
    }

  d.fmt=DEFAULT_FMT;
  PUSH(stck,d);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: VM instruction to call a built-in function with
// two arguments.
//
int bltin2()
{
  StackType d1,d2;
  BASIC_NUM dx;
  DBG("bltin2");
  
  d2 = TOP(stck);    POP(stck);
  d1 = TOP(stck);    POP(stck);
  d1.val = (*(NUMTYPE (*)(NUMTYPE,NUMTYPE))(Prog[pc++]))(d1.val,d2.val);
  for (IDList::iterator i=d1.ID.begin();i!=d1.ID.end();++i)
    {
      dx  = TOP(DS[*i]); POP(DS[*i]); 
      dx *= d1.val.rms();
      PUSH(DS[*i],dx);
    }
  for (IDList::iterator i=d2.ID.begin();i!=d2.ID.end();++i)
    {
      dx  = TOP(DS[*i]); POP(DS[*i]); 
      dx *= d2.val.rms();
      PUSH(DS[*i],dx);
    }

  IDList res;
  set_union(d1.ID.begin(), d1.ID.end(),
	    d2.ID.begin(), d2.ID.end(),
	    inserter(res,res.end()));
  d1.ID = res;
  d1.fmt=DEFAULT_FMT;

  PUSH(stck,d1);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction:  VM instruction to return just the value of a 
// symbol on the stack.
//
int getval()
{
  StackType d,r;
  Calc_Symbol *s;
  BASIC_NUM v;
  IDType ID;

  DBG("getval");
  
  d=TOP(stck);   POP(stck);

  r.fmt=d.fmt;
  r.units = d.units;

  v=d.val.val();

//   if ((d.symb!=NULL) && (ISSET(d.symb->type,UNDEF_TYPE)))
//     ReportErr("undefined symbol ","###Error",0);
//   else
    {
      s = makeTmpSymb(1,RETVAR_TYPE);
      ClearDS(d);

      r.val.setval(v,0.0);
      s->value.setval(v,0.0);
      r.symb=s;
      r.symb->dx.resize(1);
      r.symb->DSList.resize(1);
      r.ID.insert(r.ID.end(),ID=*(r.symb->IDL.begin()));
      r.type=r.symb->type=RETVAR_TYPE;

      PUSH(DS[ID],0);
      PUSH(stck,r);
    }

  LetGoID(d,RETVAR_TYPE,1,0);
  DEFAULT_RETURN;
}   
//
//-----------------------------------------------------------------
// VM instruction: VM instruction to return just the error associated
// with a symbol on the stack.
//
int getrms()
{
  StackType d,r;
  Calc_Symbol *s;
  BASIC_NUM v;
  IDType ID;

  DBG("getrms");
  
  d=TOP(stck);   POP(stck);

  r.fmt=d.fmt;
  r.units = d.units;

  v=sqrt(PropagateError(d));

//   if ((d.symb!=NULL) && (ISSET(d.symb->type,UNDEF_TYPE)))
//     ReportErr("undefined symbol ","###Error",0);
//   else
    {
      s = makeTmpSymb(1,RETVAR_TYPE);
      //      ClearDS(d);

      r.val.setval(v,0.0);
      s->value.setval(v,0.0);

      r.symb=s;
      r.symb->dx.resize(1);
      r.symb->DSList.resize(1);
      r.ID.insert(r.ID.end(),ID=*(r.symb->IDL.begin()));
      r.type=r.symb->type=RETVAR_TYPE;

      PUSH(DS[ID],0);
      PUSH(stck,r);
    }

  LetGoID(d,RETVAR_TYPE,1,0);
  DEFAULT_RETURN;
}   
//
//-----------------------------------------------------------------
// VM instruction: To set the format string of a VM variable.
//
int setfmt()
{
  StackType d,fmt;
  DBG("setfmt");

#ifdef VERBOSE
  ERROUT << "setfmt" << endl;
#endif

  fmt = TOP(stck);   POP(stck);
  d   = TOP(stck);   POP(stck);
  
  if ((d.fmt=="%dms") & (fmt.fmt=="%rad"))
    {
      d.units=U_RADIAN;
      fmt.fmt=d.fmt=DEFAULT_FMT;
      d.val *= (M_PI/180.0)/3600;
    }
  
  d.fmt=fmt.fmt;
  if (fmt.units==FMTCOPY) d.symb->fmt=fmt.fmt;

  //  POP(DS[*fmt.ID.begin()]);
  //  ClearDS(fmt);

#ifdef VERBOSE
  ERROUT << "setfmt: Stack size = " << stck.size() << endl;
#endif

  PUSH(stck,d);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Instruction for the assignment operator
//
int assgn()
{
  StackType d1,d2;
  BASIC_NUM tdx=0;

  DBG("assgn");

#ifdef VERBOSE
  ERROUT << "assgn" << endl;
#endif
  
  d2=TOP(stck);    POP(stck);
  d1=TOP(stck);    POP(stck);

  tdx = PropagateError(d2);
  ClearDS(d1);

#ifdef VERBOSE
  IDList::iterator i;
  ERROUT << "D1 IDs: ";
  for(i=d1.ID.begin();i!=d1.ID.end();++i)  
    ERROUT << *i << " " << DS[*i].size()<<"|";
  ERROUT << endl;
  ERROUT << "D2 IDs: ";
  for(i=d2.ID.begin();i!=d2.ID.end();++i)  
    ERROUT << *i << " " << DS[*i].size()<<"|";
  ERROUT << endl;
#endif

  d1.symb->value.setval(d2.val.val(),sqrt(tdx));
  d1.val.setval(d1.symb->value.val(),sqrt(tdx));
  d1.symb->fmt   = d2.fmt;
  d1.symb->units = d2.units;
  
  //
  // Since the RHS will be over-written, if the RHS was of
  // PARTIALVAR_TYPE, release all the IDs associated with it.  Else
  // release the single ID that was associated with it.
  //
  if (ISSET(d1.symb->type,PARTIALVAR_TYPE))
    LetGoID(d1,RETVAR_TYPE,1,0,0);
  //  else if (ISSET(d1.symb->type,VAR_TYPE|AUTOVAR_TYPE|QSTRING_TYPE))
  else
    //  if ((ISSET(d1.symb->type,VAR_TYPE) || ISSET(d1.symb->type, AUTOVAR_TYPE)
    //       || ISSET(d1.symb->type, QSTRING_TYPE)))
    {
#ifdef VERBOSE
      ERROUT << "assgn LHS types: " << endl; prtTypes(d1);
#endif
      IDR.ReleaseID((unsigned int)*(d1.symb->IDL.begin()));
    }
  //  d1.symb->DSList.resize(1);
  //  d1.symb->dx.resize(1);
  ClearIDL(d1.symb->IDL);
  ClearIDL(d1.ID);

  //
  // Next, assign a new ID to the RHS and set it's ME value to 1.0.
  // Also resize the DSList and the dx arrays associated with it to a
  // size of one and write 1.0 and the propagated error for the LHS
  // into them respectively.  
  //
#ifdef VERBOSE
  if (d1.symb) ERROUT << "assgn: LHS Types:" << endl;prtTypes(d1);
  if (d2.symb) ERROUT << "assgn: RHS Types:" << endl;prtTypes(d2);
#endif
  {
    IDType ID;

    ID=GetNewID();
    d1.symb->IDL.insert(d1.symb->IDL.end(),ID);
    d1.ID.insert(d1.ID.end(),ID);
    d1.symb->ID=ID;
    MeasurementError[ID]=1;
  }
  d1.symb->dx.resize(1);
  d1.symb->DSList.resize(1);
  d1.symb->dx[0]=sqrt(tdx);
  d1.symb->DSList[0]=1;
#ifdef VERBOSE
  ERROUT << "assgn: ME: " << *d1.symb->IDL.begin() << " " 
	 << MeasurementError[*d1.symb->IDL.begin()] 
	 << " " << d1.symb->dx[0] << endl;
#endif

  MeasurementError[*d1.symb->IDL.begin()]=d1.symb->dx[0];
  //  if ((Type!=VAR) && (Type!=SYS_VAR) && (Type != PARTIALVAR_TYPE)
  //  && ((Type&AUTOVAR_TYPE)!=AUTOVAR_TYPE))
  //
  // Since the RHS is now a normal variable, switch off the
  // PARTIALVAR_TYPE bit and switch on the DEFAULT_TYPE (VAR) bit.
  //
  RESETBIT(d1.symb->type, PARTIALVAR_TYPE);
  RESETBIT(d1.symb->type, QSTRING_TYPE);
  SETBIT(d1.symb->type, DEFAULT_TYPE);
  RESETBIT(d1.type, PARTIALVAR_TYPE);
  RESETBIT(d1.symb->type, PARTIALVAR_TYPE);
  RESETBIT(d1.type, QSTRING_TYPE);
  SETBIT(d1.type, DEFAULT_TYPE);

  if (d2.symb) d2.type = d2.symb->type;
  if (d1.symb) d1.type = d1.symb->type;

  //
  // If the LHS was a symbol from the symbol table and was of a quoted
  // string, set the type of the RHS to QSTRING_TYPE.  Also, allocated
  // the string object in the data structure of the RHS and copy the
  // LHS string to it.
  //
  if (d2.symb) 
    {
      if (ISSET(d2.symb->type,QSTRING_TYPE))
	{
#ifdef VERBOSE
	  ERROUT << "D2 is QString type" << endl;
#endif
	  //
	  // symb->type.qstr is expected to be empty (==NULL).  If d2
	  // is a new symbol, install() should have set it to NULL.
	  // If d2 is an existing VAR from the Symbol table of type
	  // QSTRING, the grammar rule
	  //
	  //    QSTRING '=' expr 
	  //
	  // will delete any existing qstr.
	  //
	  // If a non-QSTRING variable is getting converted to
	  // QSTRING, it will already have qstr=NULL (since it was
	  // created by install()).  Hopefully, this should take care
	  // of any mem-leak.  Hopefully!
	  //
	  //	  SETBIT(d1.symb->type,QSTRING_TYPE);
	  //	  SETBIT(d1.type, QSTRING_TYPE);
	  d1.type = d1.symb->type = QSTRING_TYPE;
	  
	  //	  if (d2.symb->otype.qstr) delete d2.symb->otype.qstr;
	  if (!d1.symb->otype.qstr) 
	    d1.symb->otype.qstr=new string;
	  *(d1.symb->otype.qstr) = *(d2.symb->otype.qstr);
	}
    }
  //
  // Push the LHS back on the stack.  This allows f(x=y) and x=y=z
  // kind of expressions.  The parser recognizes atomic assignment
  // statements and emits a pop() instruction to consume the value on
  // the top of the stack is cleared.
  //
  PUSH(stck,d1);
  PUSH(DS[*(d1.ID.begin())],1.0);

#ifdef VERBOSE
  if (d2.symb) ERROUT << "assgn(): RHS types:" << endl;prtTypes(d2);
#endif
  //
  // Finally, clear the ID list of the LHS as well.
  //
  LetGoID(d2,RETVAR_TYPE,1,0);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The partial assign operator.
//
int passgn()
{
  StackType d1,d2;
  BASIC_NUM dx2,tdx=0;
  IDList::iterator i;
  IDList res,tmp;

  DBG("passgn");
  
  d2=TOP(stck);    POP(stck);
  d1=TOP(stck);    POP(stck);
  //
  // Record the IDs of LHS symbol
  //
  for (i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();++i) tmp.insert(tmp.end(),*i);

  ClearIDL(d1.symb->IDL);
  ClearIDL(d1.ID);

#ifdef VERBOSE
  ERROUT << "passgn: LHS types :" << endl; prtTypes(d1);
  ERROUT << "passgn: RHS types :" << endl; prtTypes(d2);
  ERROUT << "passgn: LHS ID = " << d1.symb->ID << endl;
  ERROUT << "passgn: RHS ID = " << d2.symb->ID << endl;
#endif

  int k=0;
  for (i=d2.ID.begin();i!=d2.ID.end();++i) 
    {
      tdx=TOP(DS[*i]);POP(DS[*i]);
      dx2=MeasurementError[*i];
      d1.ID.insert(d1.ID.end(),*i);
      d1.symb->IDL.insert(d1.symb->IDL.end(),*i);
      d1.symb->DSList.resize(k+1);d1.symb->DSList[k]=tdx;
      d1.symb->dx.resize(k+1);d1.symb->dx[k]=dx2;
      k++;
    }

#ifdef VERBOSE
  ERROUT << "passgn: LHS IDs = ";
  for(i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();i++)
    ERROUT << *i << " ";
  ERROUT << endl;

  prtDS();
#endif

  for (i=tmp.begin();i!=tmp.end();++i)  
    {
#ifdef VERBOSE
      ERROUT << *i << " " << DS[*i].size() << endl;
#endif

      if (DS[*i].size()) POP(DS[*i]);

#ifdef VERBOSE
      ERROUT << *i << " " << DS[*i].size() << endl;
#endif
    }

#ifdef VERBOSE
  prtDS();
#endif

  //
  // Release the ID associated with the RHS since it's now a partial
  // variable (a sub-tree) and hence does not have an identity of it's
  // own.
  //
  if (d1.symb) if (!(ISSET(d1.symb->type,PARTIALVAR_TYPE))) IDR.ReleaseID(d1.symb->ID);
  
  d1.symb->value.setval(d2.val.val(),d2.val.rms());
  d1.val.setval(d1.symb->value.val(),d2.val.rms());
  d1.symb->fmt   = d2.fmt;
  d1.symb->units = d2.units;

  RESETBIT(d1.symb->type,VAR_TYPE);
  RESETBIT(d1.type,VAR_TYPE);
  SETBIT(d1.symb->type, PARTIALVAR_TYPE);
  SETBIT(d1.type, PARTIALVAR_TYPE);

  /*
  //
  // Erase all history of previous ID's the partial variable was
  // carrying.  However do not release the IDs from the IDR(esource).
  // The IDs may be pointing to other variables in use elsewhere in
  // the VM code.
  //

  ClearIDL(d1.symb->IDL);
  ClearIDL(d1.ID);

  for(i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();++i) 
      d1.symb->IDL.erase(i);

  for(i=d2.ID.begin();i!=d2.ID.end();++i) 
    d1.symb->IDL.insert(d1.symb->IDL.end(),(*i));
  */

  if (d2.symb && ISSET(d2.symb->type,QSTRING_TYPE)) 
    {
      //      if (ISSET(d2.symb->type,QSTRING_TYPE))
	{
	  //
	  // symb->type.qstr is expected to be empty (==NULL).  If d2
	  // is a new symbol, install() should have set it to NULL.
	  // If d2 is an existing VAR from the Symbol table of type
	  // QSTRING, the grammar rule
	  //
	  //    QSTRING '=' expr 
	  //
	  // will delete any existing qstr.
	  //
	  // If a non-QSTRING variable is getting converted to
	  // QSTRING, it will already have qstr=NULL (since it was
	  // created by install()).  Hopefully, this should take care
	  // of any mem-leak.  Hopefully!
	  //
	  SETBIT(d1.symb->type,QSTRING_TYPE);
	  
	  //	  if (d2.symb->otype.qstr) delete d2.symb->otype.qstr;
	  if (!d1.symb->otype.qstr) 
	    d1.symb->otype.qstr=new string;
	  *(d1.symb->otype.qstr) = *(d2.symb->otype.qstr);
	}
    }
  //
  k=0;
  PUSH(stck,d1);
  for (i=d1.symb->IDL.begin();i!=d1.symb->IDL.end();++i)
    PUSH(DS[(*i)],d1.symb->DSList[k++]);

  LetGoID(d2,RETVAR_TYPE,1,0);
  
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The instruction to execute an if-loop.
//
int ifcode()
{
  int bodyPC=pc-1;

  DBG("ifcode");

#ifdef VERBOSE
  ERROUT << "ifcode()" << endl;
#endif

  StackType d;
  
  pc += 3;
  Run(Prog);
  //
  // POP the result of the condition code
  //
  d=TOP(stck);   POP(stck);
  //
  // POP the derivatives from the all the DSs corresponding to the IDs
  // associated with the result of the condition code.
  //
  ClearDS(d);
  LetGoID(d,RETVAR_TYPE);
  
  if (d.val.val()) pc=(long) Prog[bodyPC+1];
  else             pc=(long) Prog[bodyPC+2];
  Run(Prog);
  pc=(long) Prog[bodyPC+3];
  
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The instruction to execute a while-loop.
//
int whilecode()
{
  int bodyPC=pc-1,CondPC=pc+2;

  DBG("whilecode");
  StackType d;
  
  pc = CondPC;                        // Condition code
  Run(Prog);
  d=TOP(stck);   POP(stck);
  if (ISSET(d.symb->type, RETVAR_TYPE)) 
    IDR.ReleaseID((unsigned int)*(d.ID.begin()));
  ClearDS(d);                        // POP the DS for result 
                                     // of the condition code
  
  try
    {
      while (d.val.val())
	{
#ifdef VERBOSE
	  memCheck();
#endif
	  pc=(long)Prog[bodyPC+1];     // Body code - it's part of the PROG
	  Run(Prog);
	  //d=TOP(stck);   	  POP(stck);
	  //	  IDR.ReleaseID((unsigned int)*(d.ID.begin()));
	  //	  ClearDS(d);                 // POP the DS for result 

	  pc=CondPC;                  // Condition code
	  Run(Prog);
	  d=TOP(stck);   POP(stck);
	  if (ISSET(d.symb->type,RETVAR_TYPE)) 
	    IDR.ReleaseID((unsigned int)*(d.ID.begin()));
	  ClearDS(d);                 // POP the DS for result 
                                      // of the condition code
	  LetGoID(d,RETVAR_TYPE);

	}
      //      pc=(int)Prog[bodyPC+2]; // End-of-while
    }
  catch (BreakException& x)
    {
    }      
  pc=(long)Prog[bodyPC+2];             // End-of-while
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Instruction to execute the for-loop.
//
int forcode()
{
  //
  // Execution of the for(INIT;CONDITION;PREDICATE) BODY; loop
  //
  int bodyPC=pc-1, CondPC=(long)Prog[bodyPC+1];

  StackType d;

  DBG("forcode");

#ifdef VERBOSE
  ERROUT << "forcode()" << endl;
#endif

  pc += 4;                        // The init code
  Run(Prog);
  d=TOP(stck);   POP(stck);
  ClearDS(d);
  LetGoID(d,RETVAR_TYPE);

  pc=CondPC;                      // Condition code
  Run(Prog);
  d=TOP(stck);   POP(stck);
  ClearDS(d);
  LetGoID(d,RETVAR_TYPE);
  
  try
    {
      while(d.val.val())
	{

#ifdef VERBOSE
	  memCheck();
#endif

	  pc=(long)Prog[bodyPC+2]; // Body code
	  Run(Prog);

	  pc=(long)Prog[bodyPC+3]; // Predicate code
	  Run(Prog);

 	  d=TOP(stck);   
 	  POP(stck);
 	  ClearDS(d);
	  LetGoID(d,RETVAR_TYPE);

	  pc=CondPC;              // Condition code
	  Run(Prog);
	  d=TOP(stck);  
	  POP(stck);
	  ClearDS(d);
	  LetGoID(d,RETVAR_TYPE);
	}
    }
  catch (BreakException& x)
    {
      //pc=(long)Prog[bodyPC+4];   // End-of-while
    }
  pc=(long)Prog[bodyPC+4];         // End-of-while
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: Instruction to automatically print the top of
// the VM stack.
//
int printcode()
{
  StackType d;

  DBG("printcode");
  
  d  = TOP(stck);  POP(stck);

#ifdef VERBOSE
  ERROUT << "ID List (printcode): " <<endl;
  prtTypes(d);
#endif
  //
  // Print a QSTRING
  //
  if (d.symb && ISSET(d.symb->type,QSTRING_TYPE)) 
    {
      OUTPUT << format(d.fmt.c_str()) << d.symb->otype.qstr->c_str();
      //	fprintf(OUTSTREAM,d.fmt.c_str(),d.symb->otype.qstr->c_str());
      return 1;
    }
  else if (d.symb && ISSET(d.symb->type,FMT_TYPE))
    {
      OUTPUT << d.fmt.c_str() << endl;
      return 1;
    }
  
  //
  // Print a number...  Fill-in the result in the global variable
  // Result (which is returned to the caller of top level API to the
  // parser as the final result).
  //
  Result=d.val;
  Result.setval(d.val.val(),sqrt(PropagateError(d)));
  d.val=Result;
  OUTPUT << "\t";
  PrintENum(d,OUTPUT);
  //  fprintf(OUTSTREAM,"\n");
  OUTPUT << endl;

#ifdef VERBOSE
  ERROUT << "printcode: Types:" << endl;prtTypes(d);
  for (IDList::iterator i=d.ID.begin();i!=d.ID.end();i++)
    ERROUT << "IDs: " << *i << " ";ERROUT << endl;
#endif
  //
  // Release everything in the TmpSymbTab irrespective of whether it
  // is in a permanent symb. table or not.
  //
  LetGoID(d,RETVAR_TYPE,1,0);

  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The break instruction.
//
int break_code()
{
  DBG("break_code");
  BreakException E;
  throw(E);
}
//
//-----------------------------------------------------------------
// VM instruction: Instruction to run a sub-program
//
int call()
{
  FrameType Frame;

  StackType d,ArgsOnStack;
  int NArgs=0, NAutos=0;
  long N=0;
  LocalSymbTabType::iterator CI;
  int i,M;
  
  DBG("call");

#ifdef VERBOSE
  ERROUT << "call()" << endl;
#endif

  try
    {

#ifdef VERBOSE
      prtStacks("Call in");
#endif
/*
      if (FrameStack.size() > FRAME_STACK_DEPTH)
	ReportErr("Function call nested too deep!", "###Error",0);
*/

      //
      // Set the values of the program counter (pc) and stack pointer
      // (sp) in a frame.  These values will be reloaded upon return
      // from the sub-program branch to continue the execution.
      //
      Frame.RetPC=pc;
      Frame.SP=sp;
      sp=LocalSymbTab.size();
      //      
      // This was a internally generated const - for the number of
      // arguments passed to the function.  When it was vpush()'ed on
      // the stack, it's associated error was pushed on the
      // appropriate DS stack as well (a useless operation!).  So POP
      // the DS as well!
      //
      d=TOP(stck);POP(stck);      
      POP(DS[*d.ID.begin()]);

      N=(long)d.val.val();  // No. of arguments on the stack

      d=TOP(stck);POP(stck); // Pointer to the start of the sub-prog. code
      
      pc=d.symb->otype.FuncStartPC;
      if (pc > (int)Prog.size() || pc < 0)
	ReportErr("Program conuter gets invalid value for a function call",
		  "###Runtime",0);
      //
      // Next 2 locations of the VM program have the no. of autos and
      // arguments declared in the sub-prog. code respectively.
      //
      NArgs  = (long)Prog[pc++]; 
      NAutos = (long)Prog[pc++];
      
      if (N > NArgs)
	ReportErr("Too many arguments in function call!","###Runtime",0);
      //
      // Push the current call frame on the Frame Stack and make space
      // on the Local Symbol Table to hold the formal arguments to the
      // sub-program and any auto variables declared in the
      // sub-program.
      //
      FrameStack.push_back(Frame);
      MkSpaceOnLocalSymbTab(NArgs+NAutos);
      //      N=LocalSymbTab.size()-1;
      
      //      for(i=sp+NArgs-1;i>=(long)sp;--i)
      //
      // Fill in the values of the arguments into temp. variables on
      // Local Symb. tab.  These temp. symbols are at the end of the
      // table.
      //

      CI=LocalSymbTab.begin();

      //
      // Compute the no. of slots, from the current LocalSymbTab
      // Pointer (SP) to skip over.  The reason one needs to skip over
      // is because the arguments are stored on the stack in the
      // reverse order (e.g. for f(x,y), stack has (y x)).  However
      // the VM code needs it in the right order.  So we set CI at the
      // last location for holding the passed arguments on the local
      // symbol table, and start copying values from the stack from
      // the back.  Therefore, starting from the beginning, we skip
      // over SP+NArgs-1 locations on the LocalSymbTab
      //
      // M=((NArgs=sp+N-1)>0)?NArgs:0;

      M=(sp+N-1);      M=M>0?M:0;
      advance(CI,M);
      //      CI = CI+M; // This does not work!  Should make this work.
      //      for (i=0;i<M;i++) CI++; // CI points to the (SP+N) th. symbol

      //
      // Now start copying the arguments from the stack into
      // LocalSymbTab, back-wards.
      //
      for(i=0;i<N;i++) 
	{
	  int N=0;
	  d=TOP(stck);POP(stck);
	  
	  (*CI).value = d.val;
	  (*CI).units = d.units;
	  (*CI).fmt   = d.fmt;

	  N=d.ID.size();
	  (*CI).DSList.resize(N);
	  (*CI).dx.resize(N);
	  N=0;
	  for (IDList::iterator i=d.ID.begin();i!=d.ID.end();i++)  
	    {
	      (*CI).IDL.insert((*CI).IDL.end(),*i);
	      (*CI).DSList[N] = TOP(DS[*i]); POP(DS[*i]);
	      (*CI).dx[N]     = MeasurementError[*i];

#ifdef VERBOSE
	      ERROUT << "call: ID DS ME " 
		     << *i << " " 
		     << (*CI).DSList[N] << " " 
		     << (*CI).dx[N] << endl;
#endif

	      N++;
	    }
	  (*CI).type=0;

	  SETBIT((*CI).type, VAR_TYPE|PARTIALVAR_TYPE);      
	  //	  SETBIT((*CI).type, PARTIALVAR_TYPE);
	  //
	  // Since a new ID has been allocated, resize the DS stack,
	  // transfer the DS value from the old ID to the new ID, and
	  // pop the old DS.
	  //
	  // The checks on the N <= HighestID and DS[N] having
	  // something in there are required since if one of the
	  // arguments passed is a sub-program, the corresponding DS
	  // will have nothing.  Ideally it is not even necessary to
	  // make space for this object on the DS - but not sure if
	  // the rest of logic would recognize that.  So it makes the
	  // space, but copies only if its not a pointer to a
	  // subprogram.
	  //
	  // EmptyLocalSymbTab() also does these checks.
	  //
	  DS.resize(IDR.HighestID()+1);
	  N=*d.ID.begin();

	  if (d.symb) 
	    {
	      (*CI).otype.FuncStartPC=d.symb->otype.FuncStartPC;
	      //	      SETBIT((*CI).type,d.symb->type);
	    }
	  CI--;
	}
      //
      // Now set up the auto variables in the local scope on the
      // LocalSymbTab. Why is this also done backwards?  I don't know! 
      // :) It does not make a difference anyway!!
      //
      CI=LocalSymbTab.end();
      CI--;

      for(i=0;i<NAutos;i++) 
	{
	  (*CI).ID=GetNewID();
	  (*CI).type=0;
	  SETBIT((*CI).type,AUTOVAR_TYPE);

	  (*CI).otype.qstr=NULL;
	  //	  (*CI).value.setval(0,0);
	  //	  MeasurementError[(*CI).ID-1] = 0.0;
	  CI--;
	}

#ifdef VERBOSE
      prtStacks("Call Run:");
#endif
      //
      // PC already points to the location of the code to be executed.
      // 
      Run(Prog);
    }
  catch (ReturnException x)
    {
      //
      // Execution will reach here when the sub-program returns (via
      // the return() instruction).
      //

#ifdef VERBOSE
      prtStacks("Call out:");
#endif

      //
      // Remove the symbols local to the current scope (the formal
      // arguments and the auto vaiables).  The formal arguments were
      // copied into variables on the local symbol table before the
      // sub-program execution began.
      //
      EmptyLocalSymbTab(NArgs,NAutos);

#ifdef VERBOSE
      ERROUT << "call(): " << "DS.size()=" 
	     << DS.size() << " Err.size()=" 
	     << MeasurementError.size() << endl;
#endif

      //
      // Pop the Frame Stack and recover the values of the program
      // counter (pc) and the stack pointer (sp) that were there
      // before branching to the sub-program.
      //
      Frame = TOP(FrameStack); POP(FrameStack);

      pc=Frame.RetPC;    // End-of-function
      sp=Frame.SP;       // Load the local symb. table pointer from the
                         // frame stack.
    }
  DEFAULT_RETURN;
}
//
//-----------------------------------------------------------------
// VM instruction: The return instruction.
//
int ret()
{
  //
  // If there is something on the VM stack, a value is being returned
  // from a function sub-program.  So create a symbol on the
  // TmpSymbTab, pop the VMS, package its contents in the new symbol,
  // and push it back on the VMS.  This re-packaging is required since
  // the returned value could have been constructed out of auto
  // variables, all of which will disappear immediately after the
  // return instruction is executed and hence not valid for
  // consumption outside the scope of the sub-program.
  //
  // Pop the VM stack.  Set the ID of the poped symbol to the ID of a
  // locally declared StackType.  Resize the DS to the HighestID
  // value.  Set the measured error for the new symbol to 1.0.  Copy
  // the values from the top of the stack to the the new symbol.  Put
  // the symbol on the stack and set the DS[ID] to the RMS value.
  // Then throw the ReturnExecption.
  //
  DBG("ret");
  StackType d,r;
  IDType NewID;
  SETVAL(r.val,0,0);

#ifdef VERBOSE
  prtStacks("Ret in");
  ERROUT << "Ret " << endl;
#endif

  if (stck.size() > 0)
    {
      //
      // The top of the stack has the object which was the argument of
      // the "return" command.  Since this argument can be a local
      // (auto) variable, replace the object on the top of the VM
      // stack by a new object which has the same value but a new ID.
      // The ID associated with the variable which is on the top of
      // the stack will be released in the clean-up process in call()
      // (the ReturnExecption takes the execution exactly there).
      //
      d = TOP(stck);      POP(stck);
      
#ifdef VERBOSE
      prtTypes(d);
      ERROUT << "Ret in: " << *(d.ID.begin()) <<  " " << stck.size() << endl;
#endif
      //
      // Allocated a new ID.  And resize the DS and MeasurementError
      // arrays to accommodate the new ID.
      //
      int N=0;

#ifdef VERBOSE
      ERROUT << "Ret: ID DS ME: ";
#endif

      r.symb=NULL;
      if (d.symb)  
	{

#ifdef VERBOSE
	  ERROUT << endl << "TmpSymbTab.size() = " << TmpSymbTab.size() << endl;
#endif

	  r.symb = makeTmpSymb(0);

#ifdef VERBOSE
	  ERROUT << "RET: TmpSymb " << r.symb << endl;
	  ERROUT << "Types from ret():";
	  prtTypes(d);
#endif

	  r.symb->type = d.symb->type;
	  r.type = d.type;
	  r.fmt = d.fmt;
	  r.units = d.units;
	}


      if ((d.symb!=NULL))
	{
	  N=d.ID.size();
	  r.symb->DSList.resize(N);
	  r.symb->dx.resize(N);
	  N=0;

	  IDList::iterator iterend=d.ID.end();
	  //	  for(IDList::iterator i=d.ID.begin();i!=d.ID.end();i++) 
	  for(IDList::iterator i=d.ID.begin();i!=iterend;++i) 
	    {
	      r.ID.insert(r.ID.end(),*i);
	      r.symb->IDL.insert(r.symb->IDL.end(),*i);
	      r.symb->DSList[N] = TOP(DS[*i]); POP(DS[*i]);
	      r.symb->dx[N] = MeasurementError[*i];

#ifdef VERBOSE
	      ERROUT << "Ret: ID: " 
		     << *r.ID.end()/* *i*/ << " " 
		     << r.symb->DSList[N]  << " " 
		     << r.symb->dx[N] << " " << endl;
	      ERROUT << "Ret Obj. IDs: ";
	      IDList::iterator iterend=r.ID.end();
	      for(IDList::iterator i=r.ID.begin();i!=iterend;++i) 
		ERROUT  << *i << " ";
	      ERROUT << endl;
#endif

	      N++;
	    }

	  if (d.symb) 
	    {
	      r.symb->type = 0;
	      SETBIT(r.symb->type,RETVAR_TYPE|PARTIALVAR_TYPE);
	      r.symb->units = d.symb->units;
	    }
	  r.type = 0;
	  SETBIT(r.type,RETVAR_TYPE|PARTIALVAR_TYPE);

	  r.val.setval(d.val.val(),d.val.rms());
	}
      else
	{
	  ClearIDL(r.ID);
	  r.ID.insert(r.ID.begin(),(NewID=GetNewID()));

#ifdef VERBOSE
	  ERROUT << "Ret: ID: " << *r.ID.end() << endl;
#endif
	  
	  r.type=0;
	  SETBIT(r.type, RETVAR_TYPE);
	  //
	  // Initialize the MeasurementError and DS for this new object.
	  // 
	  r.val.setval(d.val.val(),sqrt(PropagateError(d)));

	  if (d.symb)
	    {
	      r.symb->DSList.resize(1);
	      r.symb->dx.resize(1);
	      ClearIDL(r.symb->IDL);
	      r.symb->IDL.insert(r.symb->IDL.begin(),NewID);
	      SETBIT(r.symb->type, RETVAR_TYPE);
	      SETBIT(r.symb->type, PARTIALVAR_TYPE);
	      r.symb->dx[0]=MeasurementError[NewID]=r.val.rms();
	      r.symb->DSList[0]=1.0;//sqrt(tdx);
	    }

	}
      //
      // Finally, push the variable and the corresponding DS on the VM
      // and the appropriate Derivative Stacks.
      //

      int k=0;
      //      if(1)
	for (IDList::iterator ii=r.symb->IDL.begin();ii!=r.symb->IDL.end();++ii)
	  {

#ifdef VERBOSE
	    ERROUT << "Onto DS stack: " << *ii << " " 
		   << r.symb->DSList[k] << " " 
		   << r.symb->dx[k] << " " 
		   << endl;
#endif

	    MeasurementError[*ii]=r.symb->dx[k];//d.val.rms();
	    PUSH(DS[*ii],r.symb->DSList[k]);
	    k++;
	  }
	//      else
	//	PUSH(DS[NewID],1.0);

#ifdef VERBOSE
      prtTypes(d);
#endif

      if (ISSET(d.symb->type,AUTOVAR_TYPE)) SETBIT(r.symb->type,AUTOVAR_TYPE);
      if (ISSET(d.symb->type,RETVAR_TYPE))        LetGoID(d,RETVAR_TYPE,0,0);
      else if (!ISSET(d.symb->type,AUTOVAR_TYPE)) LetGoID(d,RETVAR_TYPE);
#ifdef VERBOSE
      ERROUT << "Onto stack types: " << endl; prtTypes(r);
#endif
      PUSH(stck,r);
    }
  //
  // Throw a ReturnException.  This is only caught in call() where the
  // local symbol table is cleaned, the FrameStack is poped and the
  // SP, PC pointers are loaded from the Frame stack from where the
  // execution continues.
  //
#ifdef VERBOSE
  prtStacks("Ret out");
#endif

  ReturnException E;
  throw(E);

  DEFAULT_RETURN;
}
//template <class X> inline X& TOP(vector<X>& v) {if (!v.size()) ReportErr("Illegal operation on stack or symbol table!\n            If you did not pass a proc instead of a func as an argument,\n            this is an internal error!","###Runtime",0);return v.back();}
