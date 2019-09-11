/* $Id: tables.cc,v 1.5 2006/08/07 23:03:52 sbhatnag Exp $ */
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
    All tables related operations are done here. 

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in

    installConst: New constants are made and put in the ConstTab
    here.  So get a ID from the IDResource and set the ID.
   
    cleanupSymbTab: ConstTab and SymbTab are both cleaned up 
    using this.  Release the IDs associated with the erased 
    symbols here.

               Sanjay Bhatnagar, Dec 2001
               sanjay@ncra.tifr.res.in

    Defined the GlobalDefaultFmt as an extern.
               Sanjay Bhatnagar, March 10,2006.

    Modified the logic in calcgetConst() and installCont()
    so that (1) all constants are treated as indep. variates
    - i.e., 10pm1+10pm1 = 20+/-1.414213 rather than 20+/2, and
    (2) existing entries on ConstTab are re-used if appropriate
    (see comments for calcgetConst() and installConst()).  This also
    optimizes the ConstTab for NUMBER_TYPEs.  Similar optimization 
    can done for other types.
            
                Sanjay Bhatnagar, Aug 6, 2006
                (60th. anniversary of the first Holocaust using
                 atom-bomb)
******************************************************************/

#include <tables.h>
#include <fussyparse.hh>
#include <emath.h>
#include <calc.h>
#include <IDResource.h>
#include <fstream>
#include <iomanip>

extern ofstream            ERROUT;
extern SymbTabType         SymbTab;
extern ConstTabType        ConstTab;
extern LocalSymbTabType    LocalSymbTab;
extern TmpSymbTabType      TmpSymbTab;
extern int                 InFuncDefn;
extern unsigned int        sp;
extern FrameStackType      FrameStack;
extern IDResource          IDR;
extern DSType              DS;
extern vector<BASIC_NUM>   MeasurementError;     // The ME table
extern Calc_Symbol*        GlobalDefaultFmt;
//
//-----------------------------------------------------------------
// Function to get a new ID from the central IDResource and resize
// the DS and MeasurementError arrays to be of the size of the
// highest allocated ID.
//
//  int Allocate       If == 0, only resize the DS
//                     and MeasurementError arrays.  
//                     if == -1 only allocate a new ID
//                     else allocate a new ID and also resize
//                     DS and MeasuremenetError
//
unsigned int GetNewID(int Allocate)
{
  unsigned int ID=0,i;
  if (Allocate!=0) ID=IDR.GetNewID();
  if (Allocate!=-1)
    {
      i=IDR.HighestID()+1;
      DS.resize(i);
      MeasurementError.resize(i);
      MeasurementError[ID]=0.0;
    }
  return ID;
}
//
//-------------------------------------------------------------------
//
// Check if a symbol is dependent on the ID.
//   SymbTabType::const_iterator CI  The symbol (just convenient to
//                                   pass the iterator here!)
//   IDType ID                       The symbol ID to be matched.
//
//   If matched, return the pointer to the symbol.  Else return NULL.
//
Calc_Symbol *CheckIDList(SymbTabType::const_iterator CI, IDType ID)
{
  for(IDList::iterator tCI=(*CI).IDL.begin();
      tCI!=(*CI).IDL.end();tCI++)
    {
#ifdef VERBOSE
      ERROUT << *tCI << " ";
#endif
      if (*tCI == ID) return (Calc_Symbol *)&(*CI);
    }
  return NULL;
}
//
//-------------------------------------------------------------------
//
Calc_Symbol *IsIDinGivenTab(IDType ID, SymbTabType& tab,const char *name)
{
  SymbTabType::const_iterator CI,Start,Stop;
  Start=tab.begin(); Stop=tab.end();

#ifdef VERBOSE
  ERROUT << name << " CheckList: " << endl;
#endif

  for(CI=Start;CI!=Stop;CI++)
    //	if ((ISSET((*CI).type,VAR_TYPE|PARTIALVAR_TYPE|SYS_VAR_TYPE)))
    {
      if (CheckIDList(CI,ID))
	{
#ifdef VERBOSE
	  ERROUT << " Found!" << endl;
#endif
	  return (Calc_Symbol *)&(*CI);
	}
      //	    if ((*CI->IDL.begin() == ID)) return (Calc_Symbol *)&(*CI);
    }
  return NULL;
}
//
//-------------------------------------------------------------------
//
// Return a symbol with ID from SymbTab, ConstTab or LocalSymbTab.
// Else return NULL.
//
//   int ID     The ID to look for in the SymbTab, ConstTab or
//              LocalSymbTab.  If the 2nd argument is non-zero,
//              look in TmpSymbTab as well.
//   int Which  If non-zero, also search in TmpSymbTab.
//
Calc_Symbol *IsIDinTab(IDType ID,int Which=0)
{
  SymbTabType::const_iterator CI,Start,Stop;

#ifdef VERBOSE
  ERROUT << "IsIDin: Checking ID: "<<ID<<endl;
#endif

  if (Which==0)
    {
      Calc_Symbol *symb=IsIDinGivenTab(ID,SymbTab,"SymbTab");
      if (symb != NULL)   return symb;

      symb=IsIDinGivenTab(ID,ConstTab,"ConstTab");
      if (symb != NULL)   return symb;
      
      symb=IsIDinGivenTab(ID,LocalSymbTab,"LocalSymbTab");
      if (symb != NULL)   return symb;
    }
  else
    {
      Calc_Symbol *symb=IsIDinGivenTab(ID,TmpSymbTab,"TmpSymbTab");
      if (symb != NULL)   return symb;
    }
#ifdef VERBOSE
  ERROUT << "Not in table";
  ERROUT << endl;
#endif
  return NULL;
}
#undef VERBOSE
//
//-------------------------------------------------------------------
//
// Function to return a pointer to a symbol with a given name.  If the
// symbol is not found in LocalSymbTab or SymbTab, return NULL.
// Symbols are first searched in the LocalSymbTab and then in the
// SymbTab (the global symbol table).
//
// const char *Name     String identifying the name of symbol
//                      being searched for.
//
Calc_Symbol *calcgetSymb(const char *Name)
{
  SymbTabType::const_iterator CI;
  Calc_Symbol *s=NULL;

  if (((InFuncDefn != 0) && ~(PROCRET)) && (s=LocalSymbGet(Name))) 
    return s;
  else
    for(CI=SymbTab.begin();CI!=SymbTab.end();CI++)
      if ((CI->name == Name)) 
	return (Calc_Symbol *)&(*CI);

  return NULL;
}
//
//-------------------------------------------------------------------
//
// Function to find a symbol in the ConstTab matching the value given
// by the symbol passed as an argument.  The value of the symbol is
// check depending upon the type of the symbol passed as an argument.
//
//    Calc_Symbol &v         The value of the symbol to look for.
//
Calc_Symbol *calcgetConst(Calc_Symbol& v)
{
  ConstTabType::iterator CI;
  const Calc_Symbol *s;

  if (ISSET(v.type,NUMBER_TYPE))
    {
      // 
      // If in sub-prog. code definition and the supplied symbol will
      // NOT participate in error propagation (i.e. it has no error),
      // then proceed to search for in ConstTab.
      //
      if ((InFuncDefn & 1) && (v.value.rms()==0))
	for (CI=ConstTab.begin(); CI!=ConstTab.end(); CI++) 
	  {
	    //
	    // If the ConstTab entry is a referenced entry, and
	    //  it is of type NUMBER_TYPE, and
	    //  it's value (including RMS) matches,
	    // then return the pointer to this entry.
	    //
	    if (
		(((int)(*CI).name[0]) == 1)     &&
		(ISSET((*CI).type,NUMBER_TYPE)) &&
		((*CI).value == v.value)
		)
	      {
		//		cout << (*CI).value << endl;
		break;
	      }
	  }
      else return NULL;
    }
  else if (ISSET(v.type,FMT_TYPE))
    {
      for (CI=ConstTab.begin(); CI!=ConstTab.end(); CI++) 
	if ((*CI).fmt == v.fmt)
	  break;
    }
  else if (ISSET(v.type,QSTRING_TYPE))
    {
      for (CI=ConstTab.begin(); CI!=ConstTab.end(); CI++) 
	{
	  s=&(*CI);
	  if (ISSET(s->type,QSTRING_TYPE) && s->otype.qstr && 
	      *(s->otype.qstr) == *(v.otype.qstr))
	    break;
	}
    }

  if (CI != ConstTab.end()) return (Calc_Symbol *)&(*CI);
  else return NULL;
}
//
//-------------------------------------------------------------------
//
// Function to add a symbol to the global symbol table SymbTab.
//
//   Calc_Symbol e     The new symbol to be added to SymbTab.
//
void calcput(Calc_Symbol e){SymbTab.push_back(e);}
//
//-------------------------------------------------------------------
// Function to put a new symbol with a given name.
//
//   string name       Name of the new symbol.
//
void calcput(string &name)
{
  Calc_Symbol t;

  t.type = 0;
  t.value=0;
  t.otype.qstr=NULL;
  t.name=name;

  calcput(t);
}
//
//-------------------------------------------------------------------
//
// Function to clean up a symbol table.
//
// SymbTabType& Tab    The table to cleanup.
// int OP              The operation to be done.
//                       OP==OP_ON_REF  Remove all symbol which
//                                      are non-persistent
//                       OP==OP_ON_TYPE Remove all symbols with
//                                      a matching type.
// int Type            The type of symbol to be removed.
//
void cleanupSymbTab(SymbTabType& Tab, int OP,TypeType Type=UNDEF_TYPE)
{
  SymbTabType::iterator CI;

 DONE:
  for(CI=Tab.begin();CI!=Tab.end();CI++)
    {
      switch(OP)
	{
	case OP_ON_REF:
	  // If we are going to remove an ID without checking the Type
	  // information, make sure that the ID slated to be removed
	  // is not referenced by another symbol on the Symbol Table.
	  if ((CI->ID > 0) && IsIDinGivenTab(CI->ID,SymbTab,"SymbTab"))  break;

	  if (ISSET(CI->type,NUMBER_TYPE) && (!((int)CI->name[0])))
	  //	  if (((int)CI->type == NUMBER_TYPE) && (!((int)CI->name[0])))
	    {
#ifdef VERBOSE
	      unsigned int ID;
	      ID=((Calc_Symbol *)&(*CI))->ID;
	      ERROUT << "Cleanup:REF " << (int)ID<<endl;
#endif
	      IDR.ReleaseID(((Calc_Symbol *)&(*CI))->ID);
	      Tab.erase(CI);
	      goto DONE;
	    }
	  break;
	case OP_ON_TYPE: 
	  if (ISSET(CI->type,Type))
	    {
#ifdef VERBOSE
	      unsigned int ID;
	      ID=((Calc_Symbol *)&(*CI))->ID;
	      ERROUT << "Cleanup:TYPE " << (int)ID<<endl;
#endif
	      IDR.ReleaseID(((Calc_Symbol *)&(*CI))->ID);
	      Tab.erase(CI);
	      //	      goto DONE;
	      if ((Type == FSUC_TYPE) || (Type == PSUC_TYPE)) return;
	    }
	  else break;
	}
    }
}
//
//-------------------------------------------------------------------
//
// Function to remove the named symbol from the global symbol table.
//
//  string& name     Name of the symbol to be removed.
//
void uninstall(string& name)
{
  SymbTabType::iterator CI;
  for(CI=SymbTab.begin();CI!=SymbTab.end();CI++)
    if ((*CI).name == name) 
      {
	SymbTab.erase(CI);
	break;
      }
}
//
//-------------------------------------------------------------------
// Function to remove a symbol from the global symbol table.
//
//   Calc_Symbol *e       The pointer to the symbol to be removed.
//
void uninstall(Calc_Symbol *e)
{
  Calc_Symbol *s;
  SymbTabType::iterator CI;
  for(CI=SymbTab.begin();CI!=SymbTab.end();CI++)
    {
      s=(Calc_Symbol *)&(*CI);

      if (s == e) 
	{
	  SymbTab.erase(CI);
	  break;
	}
    }
}
//
//-------------------------------------------------------------------
//
// Function remove a symbol from a table with a matching ID.
//
//    IDType  i              The ID to be matched.
//    TmpSymbTabType tab     The table from which to remove the symbol.
//                           This is defaulted to TmpSymbTab.
//
void uninstall(IDType i, TmpSymbTabType& tab)
{
  TmpSymbTabType::iterator CI;
  for(CI=tab.begin();CI!=tab.end();CI++)
    {
      //      Calc_Symbol *s=(Calc_Symbol *)&(*CI);
#ifdef VERBOSE
      ERROUT << "Attempt uninstall symb " << &(*CI) << " with ID " << i 
	     << ((&tab==&TmpSymbTab)?" from TmpSymbTab ":"")<< endl;
#endif
      if (*(CI->IDL.begin()) == i)
	{
	  SymbTab.erase(CI);
#ifdef VERBOSE
	  ERROUT << "unistalling " << i << endl;
#endif
	  break;
	}
    }
}
//
//-------------------------------------------------------------------
//
// Function to install a symbol in the local symbol table of a
// specified name and type.
//
//    const char *Name     The name of the symbol.
//    int N                NOT USED
//    int Type             The type of the new symbol.
//
void LocalSymbInstall(const char *Name,int N,int Type=VAR_TYPE)
{
  LocalSymbTabType::const_iterator CI;
  int Found=0;

  for (CI=LocalSymbTab.begin();
       (CI!=LocalSymbTab.end());
       CI++)
    if ((Found = (CI->name == Name))) 
      {
	string msg="Redeclaration of local variable ";
	msg += CI->name;

	ReportErr(msg.c_str(),"###Error",0);
      }

  if (!Found) 
    {
      Calc_Symbol s;

      s.name  = Name;
      //
      // When passing arguments to subprograms, this is used by
      // rvpush() as an index in the LocalSymbTable.....don't
      // understand it well now! :-|
      //
      // In any case, if this is called during sub-program
      // construction (InFuncDefn != 0), then do not allocate a new ID
      // for the auto variables.  Those are anyway released when the
      // sub-program construction finishes and are allocated at
      // runtime again.  This effectively better ensures that the ID
      // allocation is contiguous - i.e. the IDs permanents vars. and
      // constants are contiguous leading to optimal memory usage
      // since the size of the ME and DS arrays is equal to the
      // highest allocated ID.
      //
      s.units = LocalSymbTab.size();
      s.type  = Type|AUTOVAR_TYPE;

      if (!InFuncDefn) 
	s.ID    = GetNewID();
      SETVAL(s.value,1,0);

#ifdef VERBOSE
      ERROUT <<"AUTO "<<Name<<" "<<s.ID<<" InFuncDefn=" << (InFuncDefn&1) << endl;
#endif
      //      s.value.setval(1,0);
      LocalSymbTab.push_back(s);
    }
}
//
//-------------------------------------------------------------------
//
// Empty the local symbol table of the the symbols created in the
// local scope.  The formal arguments are at the end of the table and
// nothing needs to be done to release them except to move the end of
// the symbol table up by NArgs.  After the formal arguments are the
// local (auto) symbols.  The IDs associated with these arguments also
// have to be released (this MUST not be done for the temp. symbols in
// which the formal arguments are copied!).
//
//    int NArgs          The number of formal arguments to a sub-
//                       program to be removed from the local symbol
//                       table.
//    int NAutos         The number of automatic variables of a sub-
//                       program to be removed from the local symbol
//                       table.
//
void EmptyLocalSymbTab(int NArgs, int NAutos)
{
  LocalSymbTabType::const_iterator CI;
  int C,i,m;
  IDType tID;

#ifdef VERBOSE
  ERROUT << "###Emptying Local Symb tab (" << NArgs << "," << NAutos << ")" << endl;
#endif
  C = (NAutos>0?NAutos:0) + (NArgs>0?NArgs:0);
  m=LocalSymbTab.size()-C;


#ifdef VERBOSE
  int L;
  L=LocalSymbTab.size()-1;
  ERROUT << "LST IDs: " ;
  for (CI=LocalSymbTab.begin();CI!=LocalSymbTab.end();CI++)
    ERROUT << (*CI).ID << " (" << (ISSET((*CI).type,PARTIALVAR_TYPE)==1?'P':'V') << ") ";
  ERROUT << endl;
#endif

  CI=LocalSymbTab.end();
  CI--;

  for (i=0;i<NAutos;i++)
    {
      if (!(ISSET((*CI).type,PARTIALVAR_TYPE)))
	{
	  tID=(*CI).ID;
	  if ((tID <= IDR.HighestID()) && (!IsIDinTab(tID,1)))
	    {
	      IDR.ReleaseID(tID);
	      DS[tID].resize(0);
	    }
	}
      if ((*CI).otype.qstr) delete (*CI).otype.qstr;
#ifdef VERBOSE
      else
	ERROUT << "###EmptyLocalSymb: not releasing because PARTIALVAR " << (*CI).ID << endl;
#endif
      CI--;
    }
  for (i=0;i<NArgs;i++) 
    {
      //      int isAuto=ISSET((*CI).type,AUTOVAR_TYPE);
      if (ISSET((*CI).type,AUTOVAR_TYPE))
	{
	  tID=(*CI).ID;
	  if ((tID <= IDR.HighestID()))
	    {
	      IDR.ReleaseID(tID);
	      DS[tID].resize(0);
	    }
	}
#ifdef VERBOSE
      else ERROUT << "###EmptyLocalSymb: not releasing because !AUTOVAR  " << (*CI).ID << " " << endl;
#endif
      CI--;
    }

  m = m>0?m:0;
  if (C==0) 
    {
      for(CI=LocalSymbTab.begin();CI!=LocalSymbTab.end();CI++)
	{
	  unsigned int N = (IDType)(*CI).ID;
	  if (N <= IDR.HighestID()) IDR.ReleaseID(N);
	  if (DS.size() > N) DS[N].resize(0);
	}
      LocalSymbTab.resize(0);
    }
  else LocalSymbTab.resize(m);
}
//
//-------------------------------------------------------------------
//
// Function to return the named symbol from the local symbol table.
// If the symbol is not found, return NULL.
//
//    const char *Name     The name of the symbol.
//
Calc_Symbol *LocalSymbGet(const char *Name)
{
  LocalSymbTabType::const_iterator CI=LocalSymbTab.begin();

  //
  // Start the search after the SP th. entry in the table.
  //

  for (unsigned int i=0;i<sp;i++) CI++;
  for (;
       (CI!=LocalSymbTab.end());
       CI++)
    if (CI->name==Name)
      return (Calc_Symbol *)&(*CI);

  return NULL;
}
//
//-------------------------------------------------------------------
//
// Function to make a new symbol on the temporary symbol table
// (TmpSymbTab) of given type.  If makeNewID is non-zero, allocate a
// new ID for the new symbol.
//
//    int makeNewID        Allocated a new ID if this is non-zero.
//    IDType  Type         Type of the new symbol.
//
Calc_Symbol *makeTmpSymb(int makeNewID,IDType Type)
{
  Calc_Symbol c,*p;
  IDType i;
  TmpSymbTab.push_back(c);
  p = (Calc_Symbol *)&(TmpSymbTab.back());

  if (makeNewID) 
    {
      //      p->IDL.insert(p->IDL.end(),(i=IDR.GetNewID()));
      p->IDL.insert(p->IDL.end(),(i=GetNewID()));
      
      //      DS.resize(IDR.HighestID()+1);
      //      MeasurementError.resize(IDR.HighestID()+1);
      //      MeasurementError[i] = 0.0;
    }

  p->type = Type;
  p->type |= RETVAR_TYPE;

  return p;
}
//
//-------------------------------------------------------------------
//
// Function to create new space on the local symbol table.
//
//   int N     The number of new symbols to be created on the
//             local symbol table.
//
void MkSpaceOnLocalSymbTab(int N)
{
  //
  // Add N new elements on the Local Symb Tab and initialize the
  // the new elements to 0
  //
  int m=LocalSymbTab.size();
  LocalSymbTabType::iterator CI=LocalSymbTab.end();
  LocalSymbTab.resize(m+N);

  for (;
       (CI!=LocalSymbTab.end());
       CI++)
    {
      SETVAL((*CI).value,0,0);
      (*CI).type=VAR_TYPE;
      (*CI).type=AUTOVAR_TYPE;
      (*CI).fmt=DEFAULT_FMT;
      (*CI).name = "AutoVar";
    }
}
//-----------------------------------------------------------------
//
// Function to install a new symbol of specified name, type and value.
//
//  char *Name         Name of the symbol.
//  int type           The type of the new symbol.
//  double v           The value of the new symbol.
//  double e           The error associated with the new symbol.
//
Calc_Symbol *install(const char *Name, int type, double v, double e)
{
  Calc_Symbol *s=new Calc_Symbol;
  //Calc_Symbol *s=(Calc_Symbol *)calloc(1,sizeof(Calc_Symbol));
  s->type = type;
  SETVAL(s->value,v,e);

  s->fmt=DEFAULT_FMT;
  s->otype.qstr=NULL;
  return s;
}
//
//-----------------------------------------------------------------
//
// Function to install a new constant in the ConstSymbTab.  If the
// constant already exists in the ConstSymbTab, just return a pointer
// to that constant.  Else construct one, add it to the ConstSymbTab
// and return the pointer. If a new constant is created and NewID>-1,
// assign the value of NewID as the ID of the new constant.  Else get
// a new ID from the central ID resource.
//
//    Calc_Symbol &d         The new symbol to be installed.
//    int NewID(default=-1)  The ID to be assigned to a new constant
//
//
// The strategy of managing the table of constants is the following now:
//   For NUMBER_TYPE, look on ConsTab if (1) installing a number for
//     sub-program code (InFuncDefn==True), and (2) the symbol being 
//     installed has no error (Symbol.rms()==0).  Unless these two 
//     conditions are met in calcgetConst(), it does not search in ConsTab.
//
//     If ConsTab is searched, return an entry if (1) it is an entry
//     referenced by some sub-program code - i.e., it is permanent, and
//     (2) its value (including RMS) matches.
//     
//   In case a new symbol is added to the ConstTab, since its always
//   pushed at the end, pick up the pointer to the latest constant
//   from the end (instead of searching in ConsTab again!).
//   
//   Finally, if in function/procedure compilation set symb->name[0] to 1
//   
// Cleanup() cleans up the ConstTab of all symbols which are not referenced. 
//  i.e. for which name[0] != 1.
//
// This essentially makes ConstTab a permanent storage for constants used in
// sub-program code and temp. store for constants used in vm code for the interactive 
// session and/or parameters (all temps).
// Functionally, this then treats all constants, as indep. variates, even if their
// values are the same.  Same name symbols on the SymbTab.
//
//                                                     S.Bhatnagar, 4Aug, 2006
//
Calc_Symbol *installConst(Calc_Symbol&d,int NewID)
{
  Calc_Symbol *t;

  if ((t=calcgetConst(d))==NULL)
    {
      //
      // The constant does not exist on the ConstTab.
      // So install one.
      //
      d.name.resize(sizeof(int));
      ConstTab.push_back(d);
      //      t=calcgetConst(d);
      ConstTabType::const_iterator CI=ConstTab.end();      
      CI--;
      t=(Calc_Symbol *)&(*(CI));
      //      cout << t->value << endl;
      //
      // Since this is a new CONSTANT, get new ID for it.
      //
      if (NewID > -1) t->ID = NewID;
      else            t->ID=GetNewID();

      //      t->ID=GetNewID();
    }
  (t->name[0])=(int)((int)(t->name[0])==0?(InFuncDefn & 1):(int)(t->name[0]));

  return t;
}
