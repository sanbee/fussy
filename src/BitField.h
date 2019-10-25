//-*-C++-*-
// $Id: BitField.h,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $
/*****************************************************************
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

               Sanjay Bhatnagar, Dec 2001
               sanjay@ncra.tifr.res.in

******************************************************************/
//
// A bit container array.  Individual bits can be manipulated and the
// internal storage is automatically resized to accomodate new bits.
//
#ifndef BITFIELD_H
#define BITFIELD_H

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <iomanip>
#include "namespace.h"

class BitField{
public:
  BitField() {Size=0;Bits=0;CurPos=0;DefBitVal=0;BYTELEN=sizeof(char);}
  BitField(BitField& /*b*/) {};
  BitField(short int Def) 
  {Size=0;Bits=0;CurPos=0;DefBitVal=Def;BYTELEN=sizeof(char);}
  BitField(int NBits);
  
  ~BitField() {Size=0;if(Bits) free(Bits);Bits=0;}

  void setdefault(short int d) {DefBitVal=d;}
  void setIterator(int N=0)    {CurPos=N;};
  void clear()                 {if (Bits) free(Bits);Bits=NULL;Size=0;}
  void toggle(int N);
  void set(int N);
  void reset(int N);
  int  resize(int N);
  int  length()                {return Size;};
  int  count();
  int  next();
  int  next(int);
  int  operator()(int N);

  char *getBuf()               {return Bits;}
  void fprtBits(FILE *);

  BitField& operator=(BitField& b);
  BitField& operator|(BitField& b);
  BitField& operator&(BitField& b);

  friend ostream& operator<<(ostream& os, BitField& m);

  unsigned int BYTELEN;


private:
  char *Bits;
  int Size;
  unsigned int CurPos;
  short int DefBitVal;
};

#endif
