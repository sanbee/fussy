/* $Id: calcinit.h,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $ */
/******************************************************************
 * Copyright (c) 2000-2006, 2007 S.Bhatnagar
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

#if !defined(CALCINIT_H)
/* $Id: calcinit.h,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $ */
#define CALCINIT_H
#include <values.h>
#include <math.h>
#include <calc.h>
#include <string>
#include <list>
#include <tables.h>
#include <IDResource.h>
//
//  Following is not defined with -ansi -pedantic options of gcc!
//
//extern double rint(double x); 

//
// The global resources
//
SymbTabType    SymbTab;    // The Symbol table
ConstTabType   ConstTab;   // The symbol table for constatns
TmpSymbTabType TmpSymbTab; // The symbol table for temps.
IDResource     IDR;        // The ID resource

#endif
