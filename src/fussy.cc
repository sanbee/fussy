// $Id: fussy.cc,v 1.3 2006/03/10 21:38:37 sbhatnag Exp $
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

    Loads all files listed on the commandline (though if a file is
    specified more than once, it could be a problem - redefinition of
    procs/funcs is not allowed in the syntax).

               Sanjay Bhatnagar, Nov,2001                   
               sanjay@ncra.tifr.res.in

    The VM is booted before the files specified on the commandline
    are loaded.  This was a bug - without this, there was a seg.
    fault if any of the fussy commands was in the included files.

               Sanjay Bhatnagar, March 10, 2006.
******************************************************************/

#include <iostream>
#include <fstream>
//#include <string>
#include <cstring>
#include <stdio.h>
#include <calc.h>
#include <emath.h>
#include <fussyparse.hh>
#include <ErrorObjStr.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include "namespace.h"
#include "config.h"
#include <sstream>

extern int   calc_debug;
extern FILE  *rl_instream;
extern int FUSSY_IGNOREEOF;

ofstream     ERROUT;  // The global stream where debugging messages
		      // are written
void         ExitMsg(ostream& o, string file="exit.dat");
unsigned int ProgBase=0;
ostream OUTPUT(cout.rdbuf());
bool VMState_Quit=false;
//
// The following are used by ExitMsg()
//
int Persistance=0;
int Tolerance=10000;
pid_t Seed=getpid();
//
//---------------------------------------------------------------------
//
void handler(int sig)
{
  try
    {
      if (sig==SIGINT)
	{
	  boot();
	  if (VMState_Quit) exit(0);
	  else ExitMsg(cerr);
	}
      else if (sig==SIGSEGV) 
	{
	  boot();
	  ReportErr("Detected a segementaion fault.","###Error",0);
	}
    }
  catch(ExitException& e) {exit(0);}
  signal(SIGINT,handler);
  signal(SIGSEGV,handler);
}
//
//---------------------------------------------------------------------
//
const char *basename(const char *name)
{
  const char *t=&name[strlen(name)-1];
  while ((*t != '/') && (t > name)) t--;
  if (*t == '/') t++;
  return (char *)t;
}
//
//---------------------------------------------------------------------
//
void LoadFile(const char *Name)
{
  NUMTYPE ans;
  FILE *rc=NULL, *outfile=NULL;
  int i;
  if ((rc=fopen(Name,"r")))
    {
      outfile=fopen("/dev/null","w");
      while(!feof(rc))
	{
	  boot();
	  if ((i=calc(NULL,&ans,rc,outfile))==END_OF_INPUT) break;
	}
      fclose(rc);rc=NULL;
    }
  else	
    ReportErr(strerror(errno),"###Informational",ErrorObj::Informational);
}
//
//---------------------------------------------------------------------
//
ErrorObj MsgStream; // The global error message output stream

int main(int argc, char *argv[])
{
  string Name(getenv("HOME"));
  char *tt=getenv("FUSSY_IGNOREEOF");
  if (tt) 
    {
      stringstream ss(tt);
      ss >> FUSSY_IGNOREEOF; 
      FUSSY_IGNOREEOF=(FUSSY_IGNOREEOF <= 0?1e6:FUSSY_IGNOREEOF+1);
    }
  else FUSSY_IGNOREEOF=1;
  int n;
  bool beQuiet=false;
#ifdef YYDEBUG
  calc_debug=0;
#endif
  //
  // A command line argument of "-d", set the ERROUT (where all
  // debugging messages are written) to "/dev/tty". Else set it to
  // "/dev/null".  If it is "-t N" and N can be converted to an
  // interger, set Tolerance to N.
  //
  // Rest of the arguments are treated as filenames to be loaded
  // before starting the interactive session.  If no file is given to
  // be loaded, try to load ~/.fussy file.  Else keep quiet.
  //

  try
    {
      ERROUT.open("/dev/null");
      n=1;
      if (argc>=2)
	for(int i=1;i<argc;i++)
	  {
	    if (!strcmp(argv[i],"-q"))
	      {
		beQuiet=true;
		n++;
	      }
	    else if (!strcmp(argv[i],"-d"))
              {
                if (ERROUT) ERROUT.close();
                ERROUT.open("/dev/tty");
                if (!ERROUT) cerr << "Could not open error device!" << endl;
                n++;
              }
            else if (!strcmp(argv[i],"-t"))
              {
                int T=Tolerance;
                i++;
                sscanf(argv[i],"%d",&T);
                Tolerance=T;
                n+=2;
              }
            else if ((!strcmp(argv[i],"-h")) || (!strcmp(argv[i],"--help")))
              {
                MsgStream << "Usage: " << argv[0] 
                          << " [-h|--help] [-q] [-d] [-t N] [prog1,prog2,...]" 
                          << endl
                          << " \"-h or --help\": Gives this help" << endl
                          << " \"-q\":   Do not print the copyright information" << endl
                          << " \"-d\":   Sets the debugging mode (meant for developers)" << endl
                          << " \"-t N\": N is the number of Ctrl-C trials after which the interpreter" << endl
                          << "         gives up preaching good behavior and quits" << endl
                          << " Use the interpreter \"help\" command to get more help about the language syntax" 
                          << endl;
                n++;
                exit(0);
              }
            else if ((!strcmp(argv[i], "-v")) || (!strcmp(argv[i], "--version"))) {
              MsgStream << PACKAGE_NAME << ", version " << VERSION << endl;
#include "./warranty.txt"
	      // warranty.txt instantiates the copyright and warranty notices as a std::string object. 
	      MsgStream << warrantyStr << endl;
              exit(0);
            }
	  }
      //
      // Initialize internal tables etc.
      //
      InitFussy(); // Initialze all internal tables
      if (!beQuiet) showCopyright("   For details type `warranty'.");
      boot();      // Boot the virtual machine
      if (n>=argc)
	{
	  Name += "/."; Name += basename(argv[0]);
	  try
	    {
	      LoadFile(Name.c_str());
	      
	      // 	      MsgStream << "###Informational: Loaded file \"" << Name
	      // 			<< "\"..." << endl;
	    }
	  catch(ErrorObj&x) 
	    {
	      // Don't act up too much if the user doesn't even care
	      // to have the ~/.fussy file!
	    };
	}
      else
	for (int j=n;j<argc;j++) 
	  try
	    {
	      MsgStream << "###Informational: Loading file \"" << argv[j]
			<< "\"..." << endl;
	      LoadFile(argv[j]);
	    }
	  catch (ErrorObj& x) {x<< x.what() << endl;}
    }
  catch (ErrorObj& e)        {e << e.what() << endl;}
  catch (BreakException& x)  {}
  catch (ReturnException& x) {}
  catch (ExitException& x)   {exit(0);}
  //
  // Read input from the standard input and run the VM.  This is the
  // infinite loop which is terminated by the execution of the "quit"
  // instruction which throws the ExitException.
  //
  
  while(!VMState_Quit)
    try
      {
	Persistance=0;
	signal(SIGINT,handler);
	signal(SIGSEGV,handler);
	boot();
	calc(NULL,cout,NULL, NULL);
	//calc(NULL,&ans,NULL,NULL);
      }
    catch (ErrorObj& e)        {e << e.what() << endl;}
    catch (BreakException& x)  {}
    catch (ReturnException& x) {}
    catch (ExitException& x)   {ErrorObj tt; tt << "Goodbye!" << endl;exit(0);}
    catch (...) {MsgStream << "Caught an unknown exception" << endl;}
  if (VMState_Quit)  {ErrorObj tt; tt << "Goodbye!" << endl;exit(0);}
}

