/* $Id: func.cc,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $ */
/******************************************************************

 * Copyright (c) 2000-2019, 2020 S.Bhatnagar
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

******************************************************************/

#include <stdio.h>
#include <iostream>
#include <map>
#include <calc.h>
#include <string>
#include <func.h>
#include <tables.h>

LocalSymbTabType LocalSymbTab;
FrameStackType FrameStack;
extern VMac Prog;
extern int pc;
extern int InFuncDefn;
//
//-------------------------------------------------------------------
//
int funcInit(int NArgs)
{
  int n=LocalSymbTab.size();
  return n-NArgs;
}
//
//-------------------------------------------------------------------
//
int funcstart(int /*NArgs*/)
{
  emit(STOP);emit(STOP); // 2 locations for No. of arguments and No. of
                         // local variables used
  return 1;
}
//
//-------------------------------------------------------------------
//
int funcrun(Calc_Symbol *S,int Args)
{
  Calc_Symbol NArgs;
  BASIC_NUM ExpectedArgs = S->value.val();

  if (!((InFuncDefn & ~(PROCRET))) & (ExpectedArgs != Args))
    {
      char v[16];
      string msg=S->name;
      sprintf(v,"%d%c",(int)ExpectedArgs,'\0');
      msg += " needs exactly ";
      msg += v; msg += " arguments";
      ReportErr(msg.c_str(),"###Error",0);
    }

  MakeANumber(NArgs,Args,0);

  mpush(S);
  emit(cpush);emit((Instruction)installConst(NArgs,0));
  emit(call);
  return 1;
}
