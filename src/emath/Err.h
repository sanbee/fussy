//-*-C++-*-
#if !defined(ERR_H)
#define ERR_H

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
#include <iostream>
#include <math.h>
using namespace std;

template<class T> class Err
{
private:
  T x,dx;
public:
  //template <class X> Err<T>(){x=dx=0;};
  //Err<T>(T v=0,T e=0) {x=v;dx=e;}

  template <class X> Err<T>& operator=(const Err<X>& l) 
    {x=l.val();dx=l.rms();return *this;};
  template <class X> Err<T>& operator=(const X& l) 
    {x=l;dx=0;return *this;};
  inline T val() const {return x;}
  inline T rms() const {return dx;}
  inline void setval(const double v=0, const double e=0) {x=v;dx=e;}
  template<class X> inline int operator==(Err<X>& l)
  {
    return ((l.x==x) & (l.dx==dx));
  }

  template<class X> inline int operator==(X& l)
  {
    return (l==x);
  }

  //
  //--------------------Addition-------------------------------------------------
  //      
  template <class X> inline Err<T> operator+=(const Err<X>& l)
    { x += l.x; dx = sqrt(l.rms()*l.rms() + dx*dx); return *this;}

  inline Err<T> operator+=(const double& l) { x +=l; return *this;}
  //
  //--------------------Multiplication-------------------------------------------
  //      
  template <class X> inline Err<T> operator*=(const Err<X>& l) 
    {
      dx=sqrt(l.x*l.x*dx*dx + x*x*l.rms()*l.rms()); x *= l.x; 
      return *this;
    }

  inline Err operator*=(const double& l) {x *= l; dx *= l;return *this;}
  //
  //--------------------Subtraction----------------------------------------------
  //      
  template <class X> inline Err<T> operator-=(const Err<X>& l) 
    {dx=sqrt(dx*dx + l.rms()*l.rms());x -= l.x; return *this;}

  inline Err operator-=(const double& l) {x -= l; return *this;}
  //
  //---------------------Division------------------------------------------------
  //      
  template <class X> inline Err<T> operator/=(const Err<X>& l) 
    {
      if (l.x) 
      {
	T t=l.x*l.x;
	dx=sqrt((dx*dx)/t + (-x*(-x))*l.rms()*l.rms()/(t*t));
	x /= l.x; 
      }
      return *this;
    };

  inline Err operator/=(const double& l) 
    {
      if (l) {dx /= l;  x /= l;}
      return *this;
    };

  //  template<class X> friend ostream& operator<<(ostream& o, Err<X>& l);
};
//
//---------------------------Non-member operators--------------------------------
//
//
//-------------------------------------------------------------------------------
//
template<class T> ostream& operator<<(ostream& o, const Err<T>& l)
{ o << "(" << l.val() << " +/- " << fabs(l.rms()) << ")"; return o;}
//
//----------------------------Addition-------------------------------------------
//
template<class T> inline Err<T> operator+(const Err<T>& r, const Err<T>& l)
{Err<T> t=r; return t+=l;}

template<class T> inline Err<T> operator+(const double& r, const Err<T>& l)
{Err<T> t=r; return t+=l;}

template<class T> inline Err<T> operator+(const Err<T>& r, const double& l)
{Err<T> t=r; return t+=l;}
//
//----------------------------Multiplication-------------------------------------
//
template<class T> inline Err<T> operator*(const Err<T>& r, const Err<T>& l)
{Err<T> t=r; return t*=l;}

template<class T> inline Err<T> operator*(const double& r, const Err<T>& l)
{Err<T> t;t.setval(r); return t*=l;}

template<class T> inline Err<T> operator*(const Err<T>& r, const double& l)
{Err<T> t=r; return t*=l;}
//
//----------------------------Subtraction----------------------------------------
//
template<class T> inline Err<T> operator-(const Err<T>& r, const Err<T>& l)
{Err<T> t=r; return t-=l;}

template<class T> inline Err<T> operator-(const double& r, const Err<T>& l)
{Err<T> t=r; return t-=l;}

template<class T> inline Err<T> operator-(const Err<T>& r, const double& l)
{Err<T> t=r; return t-=l;}
//
// Negation operator
//
template<class T> inline Err<T> operator-(Err<T>& v) {return -1.0*v;}
//
//----------------------------Division-------------------------------------------
//
template<class T> inline Err<T> operator/(const Err<T>& r, const Err<T>& l)
{Err<T> t=r; return t/=l;}

template<class T> inline Err<T> operator/(const double& r, const Err<T>& l)
{Err<T> t=r; return t/=l;}

template<class T> inline Err<T> operator/(const Err<T>& r, const double& l)
{Err<T> t=r; return t/=l;}
//
//-------------------------------------------------------------------------------
//
#endif
