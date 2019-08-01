/* $Id: tables.h,v 1.3 2006/08/05 20:07:41 sbhatnag Exp $ */
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

******************************************************************/

#if !defined(TABLES_H)
#define TABLES_H
#include <math.h>
#include <string>
#include <list>
#include <defns.h>

#define RESET_STACK(s) ((s).resize(0))

#define CHECK_SYMBTAB                 0x80000000
#define CHECK_CONSTTAB               0x40000000
#define CHECK_LOCALSYMBTAB      0x20000000
#define CHECK_ALLNONTMP            0x10000000
#define CHECK_TMPSYMBTAB          0x08000000
#define CHECK_ALLTAB                    0x04000000
/*
  Following is not defined with -ansi -pedantic options of gcc!
*/

Calc_Symbol   *calcgetSymb(const char *);
Calc_Symbol   *calcgetConst(Calc_Symbol&);
Calc_Symbol   *IsIDinTab(IDType,int);
Calc_Symbol    *IsIDinGivenTab(IDType ID, SymbTabType& tab,const char *name);
Calc_Symbol   *LocalSymbGet(const char *Name);
Calc_Symbol   *install(const char *Name, int type, double v, double e=0);
Calc_Symbol   *installConst(Calc_Symbol&d, int NewID=-1);
Calc_Symbol   *makeTmpSymb(int makeNewID=0,IDType Type=RETVAR_TYPE);
void           calcput(string &);
void           cleanupSymbTab(SymbTabType&, int OP, TypeType Type);
void           uninstall(string&);
void           uninstall(Calc_Symbol *);
void           uninstall(IDType, TmpSymbTabType&);
void           EmptyLocalSymbTab(int NArgs=-1,int NAutos=-1);
void           MkSpaceOnLocalSymbTab(int N);
void           LocalSymbInstall(const char *,int,int Type);
unsigned int   GetNewID(int Allocate=1);

#endif
