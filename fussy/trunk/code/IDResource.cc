// $Id: IDResource.cc,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $
/******************************************************************
 * Copyright (c) 2000-2007, 2008 S.Bhatnagar
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

               Sanjay Bhatnagar, Dec 2001
               sanjay@ncra.tifr.res.in

******************************************************************/

#include "IDResource.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <iostream>
#include "BitField.h"
#include <fstream>

extern ofstream ERROUT;
//
// Method to allocate and return a new ID.
//
unsigned int IDResource::GetNewID()
{
  int N=IDList.length()*IDList.BYTELEN,Count=-1;
  for (int i=0;i<N;i++) if (!IDList(i)) {Count=i;break;}
  if (Count == -1) Count=N;
  IDList.set(Count);
#ifdef VERBOSE
  ERROUT <<"\t"<< "Allocating "<< Count<<endl;
#endif
  return (unsigned int)Count;
}
//
// Method to return the hightest allocated ID.
//
unsigned int IDResource::HighestID()
{
  unsigned int N=IDList.length()*IDList.BYTELEN,i,j=0;
  for (i=0;i<N;i++) if (IDList(i)) j=i;
  return j;
}
//
// Method to release an ID from the list of allocated IDs.  
// The zeroth ID is never released (it's for internal reuse).
//
//   int N       The ID to be released.
//
void IDResource::ReleaseID(unsigned int N)
{
  if (N)
    {
#ifdef VERBOSE
      ERROUT << "\t" << "Releasing (" << IDList(N) << ")" << N << endl;
#endif
      if (IDList(N))  IDList.toggle(N);
    }
}
//
// Print the list of allocated IDs on the standard error output 
// stream.
//
void IDResource::prtIDList()
{
  IDList.fprtBits(stderr);fprintf(stderr,"\n");
}

