//-*-C++-*-
//$Id: AngFmt.h,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $
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

//
// Methods for overloading the operator<< to print a numerical value
// in time, angle or bit-formats.
//
#if !defined(ANGFMT_H)
#define ANGFMT_H

#include <iostream>
#include <iomanip>
#include <emath.h>

template <class T> ostream& FmtFunc(ostream& ostr, T const &Val, char const *Sep=NULL);

template <class T> struct xosmanip {
  ostream& (*f) (ostream&, const T&, const char *);
  const T Val;
  const char *Sep;

  xosmanip(ostream& (*ff)(ostream&, const T&, const char *),
	   const T &V, const char *S=NULL):f(ff),Val(V),Sep(S)
  {};

  template <class X> friend ostream& operator<<(ostream& os, const xosmanip<X>& m);
};

template <class T> inline xosmanip<T> Fmt(const T &Val, const char *Sep=NULL)
{
  return xosmanip<T>(FmtFunc,Val,Sep);
}

template <class T> ostream& operator<<(ostream& os, const xosmanip<T>& m)
{
  return m.f(os,m.Val,m.Sep);
}
//
//----------------------------------------------------------------------------------
//
ostream& PrintF(ostream& ostr, const char *Sep=NULL);

struct stream_format {
  ostream& (*print_fmt)(ostream&, const char *);
  const char *Format;

  stream_format(ostream& (*formatter)(ostream&, const char *),
	   const char *F=NULL):print_fmt(formatter),Format(F)
  {};

  friend ostream& operator<<(ostream& os, const stream_format& f);
};

inline stream_format format(const char *Sep)
{
  return stream_format(PrintF,Sep);
}

ostream& operator<<(ostream& os, const stream_format& m)
{
  return m.print_fmt(os,m.Format);
}

#include <AngFmt.cc>
#endif
