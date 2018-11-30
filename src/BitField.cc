// $Id: BitField.cc,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $
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

    First version

               Sanjay Bhatnagar, Dec 2001
               sanjay@ncra.tifr.res.in

******************************************************************/

#include <BitField.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <iostream>
//
//-----------------------------------------------------------------
// The constructor.
//   int N     Length of the array as number of bits
//
BitField::BitField(int N)
{
  CurPos = 0;
  BYTELEN=8*sizeof(char);
  if (Size*BYTELEN < (unsigned int)N)  resize(N);
}
//
//-----------------------------------------------------------------
// Method to resize the bit array
//    int N    The new size as number of bits.
//
int BitField::resize(int N)
{
  int S=length();
  Size = (int)ceil(double(N+1)/BYTELEN);
  if ((Bits=(char *)realloc(Bits,sizeof(char)*length()))==NULL)
    {
      perror("###Fatal error in BitField::resize: ");
      exit(-1);
    }
  for (int i=S;i<length();i++) Bits[i]=(char)NULL;

  return length();
}
//
//-----------------------------------------------------------------
// Method to goggle a bit.
//  int N      The bit to be toggled, counting from 0 as the
//             first bit.
//
void BitField::toggle(int N)
{
  if ((N==0) && (Size==0)) resize(1);

  if ((unsigned int)N >= Size*BYTELEN) resize(N);
  
  Bits[N/BYTELEN] ^= 1<<(N%BYTELEN);
}
//
//-----------------------------------------------------------------
// Method to set a bit to 1.
//   int N       The bit to be set to 1.
//
void BitField::set(int N)
{
  if ((N==0) && (Size==0)) resize(1);

  if ((unsigned int)N >= length()*BYTELEN) resize(N);
  
  Bits[N/BYTELEN] |= 1<<(N%BYTELEN);
}
//
//-----------------------------------------------------------------
// Method to reset the bit to 0.
//   int N      The bit to set to 0.
//
void BitField::reset(int N)
{
  if ((N==0) && (Size==0)) resize(1);

  if ((unsigned int)N >= length()*BYTELEN) resize(N);
  
  Bits[N/BYTELEN] |= 1<<(N%BYTELEN);
}
//
//-----------------------------------------------------------------
// Method to read the value of a perticular bit.
//   int N      The bit to be read.
//
int BitField::operator()(int N)
{
  if (!Bits || ((unsigned int)N > length()*BYTELEN)) return DefBitVal;
  return (Bits[(N/BYTELEN)] & (1<<(N%BYTELEN)))>0;
}
//
//-----------------------------------------------------------------
// Method count the number of bits which are set (to 1).
//  
int BitField::count()
{
  unsigned int i,N=0;
  for (i=0;i<length()*BYTELEN;i++)
    if (operator()(i)) 
      N++;
  return (int)N;
}
//
//-----------------------------------------------------------------
// Iterator to get the next bit value from the current position.
//
int BitField::next()
{
  int N=length()*BYTELEN,R=-1;
  
  if (CurPos < (unsigned int)N)
    for (unsigned int i=CurPos;i<(unsigned int)N;i++)
      {
	CurPos++;
	if (operator()(i)) 
	  {R=CurPos-1;break;}
      }
  else  CurPos = 0;

  return R;
}
//
//-----------------------------------------------------------------
// Iterator to return a perticular bit value and set the cursor to
// that location.
//   int m       The bit location to be returned.
//
int BitField::next(int m)
{
  int N=length()*BYTELEN,R=-1;
  
  CurPos=m;
  
  if (CurPos < (unsigned int)N)
    for (unsigned int i=CurPos;i<(unsigned int)N;i++)
      {
	CurPos++;
	if (operator()(i)) 
	  {R=CurPos-1;break;}
      }
  else  CurPos = 0;

  return R;
}
//
//-----------------------------------------------------------------
// Method to write the bit values to a file descriptor.
//  FILE *fd         A valid file descriptor for output.
//
void BitField::fprtBits(FILE *fd)
{
  int i,N=length()*BYTELEN;

  for(i=0;i<N;i++)
    if (operator()(i)) fprintf(fd,"%d ",i);
}
//
//-----------------------------------------------------------------
// The bit-wise OR operator.
//
BitField& BitField::operator|(BitField& b)
{
  int N;

  N=Size<b.length()?Size:b.length();

  DefBitVal=b.DefBitVal;
  for(int i=0;i<N;i++)             Bits[i] |= b.Bits[i];
  
  return *this;
}
//
//-----------------------------------------------------------------
// The bit-wise AND operator.
//
BitField& BitField::operator&(BitField& b)
{
  int N;

  N=Size<b.length()?Size:b.length();

  DefBitVal=b.DefBitVal;
  for(int i=0;i<N;i++)             Bits[i] &= b.Bits[i];
  
  return *this;
}
//
//-----------------------------------------------------------------
// The copy operator.
//
BitField& BitField::operator=(BitField& b)
{
  DefBitVal = b.DefBitVal;

  resize(b.length()*BitField::BYTELEN);
  for (int i=0;i<Size;i++) Bits[i]=b.Bits[i];
  return *this;
}
//
//-----------------------------------------------------------------
// The ouput operator.
//   ostream os    The output stream.
//   Bitfield &m   The BitField object to be written to the output
//                 stream.
//
ostream& operator<<(ostream& os, BitField& m)
{
  int i,N;
  N=m.length()*m.BYTELEN;
  for (i=0;i<N;i++)
    if (m(i)) os << "1"; 
    else os << "0";
  return os;
}


