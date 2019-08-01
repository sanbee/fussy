//-*-C++-*-
#if !defined(EMATH_H)
#define EMATH_H

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
#include <Err.h>
void          ReportErr(const char *Msg, const char *ErrType, const int& ErrLevel);
//
//-------------------Math functions for extended numbers-------------------------
//
template<class T> inline Err<T> pow(const Err<T>& v0, const Err<T>& v1)
{
  // if ((v0.val() < 0.0) && (v1.val() < 1.0))
  //   ReportErr("Value out of range in pow","###MathError",0);

  T x,dx;
  Err<T> tmp;
  x  = (T)pow((double)v0.val(),(double)v1.val());
  dx = log((double)v0.val());
  dx = (T)sqrt((double)(x*x*dx*dx*v1.rms()*v1.rms() + 
  			(v1.val()*v1.val()*pow(v0.val(),2*(v1.val()-1)))*
  			(v0.rms()*v0.rms())));	
  //NEW
  //  dx = (T)(double)(v1.val()*pow((double)v0.val(),v1.val()-1));

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> pow(const Err<T>& v0, const double& v1)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)pow((double)v0.val(),v1);
  dx = (T)sqrt((double)((v1*v1*pow(v0.val(),2*(v1-1)))*(v0.rms()*v0.rms())));	
  // NEW
  //  dx = x;

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> pow(const double& v0,const Err<T>& v1)
{
  // if ((v0 < 0.0) && (v1.val < 1.0))
  //   ReportErr("Value out of range in pow","###MathError",0);

  T x,dx;
  Err<T> tmp;
  x  = (T)pow((double)v0,v1);
  dx = log((double)v0);
  dx = (T)sqrt((double)(x*x*dx*dx*v1.rms()*v1.rms()));
  //NEW
  //  dx = (T)(double)(v1.val()*pow((double)v0.val(),v1.val()-1));

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> exp(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)exp((double)v0.val());
  //  dx = (T)x*v0.rms();
  // NEW
  dx=(T)x;

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> log(Err<T> v0)
{
  if (v0.val()<0)
    ReportErr("Value out of range in log","###MathError",0);

  T x,dx;
  Err<T> tmp;
    {
      x  = (T)log((double)v0.val());
      //      dx = v0.rms()/v0.val();
      // NEW
      dx = 1.0/v0.val();
      tmp.setval(x,dx);
    }

  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> sin(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)sin((double)v0.val());
  //  dx = v0.rms()*sqrt(1-x*x);  // delta(x)*cos(x)
  //NEW
  dx = (T)cos((double)v0.val()); // cos(x)

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> cos(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)cos((double)v0.val());
  //  dx = v0.rms()*sqrt(1-x*x);  // delta(x)*sin(x)
  //NEW
  dx = -(T)sin((double)v0.val());  // -sin(x)
  // This is not safe --> -sqrt(1-x*x);
  
  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> tan(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)tan((double)v0.val());
  dx = (T)cos((double)v0.val());
  //  dx = v0.rms()/(dx*dx); // deltaX*sec(X)
  // NEW
  dx = 1/(dx*dx); // sec(X)

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> asin(Err<T> v0)
{
  if (fabs(v0.val()) > 1.0)
    ReportErr("Value out of range in asin","###MathError",0);
  T x,dx;
  Err<T> tmp;

  x  = (T)asin((double)v0.val());
  dx = 1.0-v0.val()*v0.val();
  
  //  dx=(dx>0.0)?v0.rms()/sqrt(dx):0.0;
  // NEW
  dx = (dx>0.0)?1/sqrt(dx):0.0;

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> acos(Err<T> v0)
{
  if (fabs(v0.val()) > 1.0)
    ReportErr("Value out of range in acos","###MathError",0);
  T x,dx;
  Err<T> tmp;

  x  = (T)acos((double)v0.val());
  dx = 1-v0.val()*v0.val();
  // dx=(dx>0.0)?v0.rms()/sqrt(dx):0.0;
  // NEW
  dx=(dx>0.0)?1/-sqrt(dx):0.0;

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> atan(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)atan((double)v0.val());
  //dx = v0.rms()/(1+v0.val()*v0.val());
  // NEW
  dx = 1/(1+(v0.val()*v0.val()));

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> atan2(Err<T> v0,Err<T> v1)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)atan2((double)v0.val(),(double)v1.val());
  //  dx = v0.rms()/(1+v0.val()*v0.val());
  // NEW
  dx = 1.0/(1+v0.val()*v0.val());

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> sinh(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)sinh((double)v0.val());
  //  dx = v0.rms()*sqrt(1+x*x);
  // NEW
  //  dx = sqrt(1+x*x);
  dx = (T)cosh((double)v0.val());

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> cosh(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)cosh((double)v0.val());
  //  dx = v0.rms()*sqrt(x*x-1);
  // NEW
  //  dx = sqrt(x*x-1);
  dx = (T)sinh((double)v0.val());

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> tanh(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)tanh((double)v0.val());
  dx = cosh(v0.val());
  //  if (dx) dx = v0.rms()/dx;
  // NEW
  if (dx) dx = 1.0/(dx*dx);
  else ReportErr("Division by zero in d(tanh)/dx","###MathError",0);
  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> asinh(Err<T> v0)
{
  T x,dx;
  Err<T> tmp;
  x  = (T)asinh((double)v0.val());
  //  dx = v0.rms()/sqrt(1+v0.val()*v0.val());
  // NEW
  dx = 1.0/sqrt(1+v0.val()*v0.val());

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> acosh(Err<T> v0)
{
  if (fabs(v0.val()) < 1.0)
    ReportErr("Value out of range in acosh","###MathError",0);
  T x,dx;
  Err<T> tmp;
  x  = (T)acosh((double)v0.val());
  dx = (v0.val()*v0.val()-1.0);
  //  if (dx>0.0) dx = v0.rms()/sqrt(dx);
  // NEW
  dx = 1.0/sqrt(dx);

  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> atanh(Err<T> v0)
{
  if (fabs(v0.val()) > 1.0)
    ReportErr("Value out of range in atanh","###MathError",0);
  T x,dx;
  Err<T> tmp;
  x  = (T)atanh((double)v0.val());
  dx = (1.0-v0.val()*v0.val());
  //  if (dx) dx = v0.rms()/dx;
  // NEW
  //  if (dx)
    dx = 1.0/dx;
    //  else ReportErr("Division by zero in d(atanh)/dx","###MathError",0);
  tmp.setval(x,dx);
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> sqrt(Err<T> v0)
{
  if (v0.val() < 0)
    ReportErr("Value out of range in sqrt","###MathError",0);
  T x;
  Err<T> tmp;
  x  = (T)sqrt((double)v0.val());
  // NEW
  //  dx = 1.0/(2*x); // 1/(2*sqrt(X))
  tmp.setval(x,1.0/(2*x));
  return tmp;
}
//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> fabs(Err<T> v0)
{
  Err<T> tmp;
  tmp.setval(fabs(v0.val()),v0.rms());
  return tmp;
}

//
//-------------------------------------------------------------------------------
//
template<class T> inline Err<T> fmod(Err<T> v0,Err<T> v1)
{
  Err<T> tmp;
  tmp.setval(fmod((double)v0.val(),(double)v1.val()),v0.rms());
  return tmp;
}

typedef Err<double> edouble;
typedef Err<float>  efloat;
typedef Err<int>    eint;

#endif




