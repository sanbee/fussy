// $Id: AngFmt.cc,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $
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

    First version.

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in

    Added the PrtBits function.  Also used the enumerations from
    units.h.
               Sanjay Bhatnagar, Oct 2003
               sbhatnag@aoc.nrao.edu
                            
******************************************************************/

#include <AngFmt.h>
#include <iostream>
#include <math.h>
#include <units.h>
#include <string.h>
#include <ctype.h>
//---------------------------------------------------------------
//
// Method to print the indivitual bits an output stream.
//
//     ostr           The output stream
//     Format         The output format.  A value of 
//                    U_BITSX will insert a blank in
//                    output stream after 8 bits.
//
template<class T> void PrtBits(ostream& ostr, T v,int Format)
{
  //
  // Print in the bit format
  //
  T Mask;
  int N=sizeof(T)*8;

  Mask = (T)pow((double)2,(N-1));

  for(int i=0;i<N;i++)
    {
      if ((Format==U_BITSX) && (i%8 == 0)) ostr << " ";
      ostr << ((v & (Mask>>i))!=0);
    }
}
//---------------------------------------------------------------
//
// Method to print a value on the output stream in a format determined
// by the seperator string.
//
//    ostr        The output stream
//    Val         The value to be printed
//    Sep         The seperater string.  
//                  'h'"' will print in the angle format (02h30'10")
//                  'dms' will print in the time format (02h30m10s)
//                  '%b' will print in the bit format (001010110)
//                  '%B' same as %b except will insert a blank after 
//                       8 bits
//
template <class T> ostream& FmtFunc(ostream& ostr, T const &Val, char const *Sep)
{
  double t,s;
  int d,m;
  double v,e;
  char fillchar;
  int format=U_DMSFORMAT,N=0, M,Precision=2;

  v=Val.val();
  e=Val.rms();

  //
  // In case a the format was of type "%Ndms" or "%Nhms" (where N is
  // used to specify the precision), skip till a non-digit character
  // is found.
  //
  while (isdigit(Sep[N])) N++;

  // 
  // Convert as many leading characters in Sep to integer to extract
  // the precision specification for hms or dms formats.  Not a very
  // robust conversion!
  //
  sscanf(Sep,"%d",&Precision);

  if ((Sep[N]=='h') || (Sep[N]=='m') || (Sep[N]=='s')) format=U_HMSFORMAT;
  else if ((Sep[N]=='d') || (Sep[N]=='\'') || (Sep[N]=='"')) format=U_DMSFORMAT;
  else if (!strcmp(Sep,"%b")) format=U_BITS;
  else if (!strcmp(Sep,"%B")) format=U_BITSX;

  if (format==U_HMSFORMAT) {v=fmod(v,24.0); e=fmod(e,24.0);} 
  else if (format==U_DMSFORMAT) {v=fmod(v,360.0); e=fmod(e,360.0);} 

  if(e) ostr << "(";

  d=(int)v;
  if ((d==0) && (v<0)) ostr << "-";
  t=fabs(v-d);
  m=(int)(t*60.0);
  t=t-m/60.0;
  s=t*3600.0;

  //  cerr << v << " " <<  d << " " << m << " " << s << endl;
  M=N;
  if ((format==U_BITS)||(format==U_BITSX)) 
    {
      PrtBits<unsigned long>(ostr, (unsigned long)v,format);
    }
  else
    if (Sep && (strlen(Sep)>2))
      {
	ostr << setw(3) << d << Sep[M++] ;
	fillchar = ostr.fill('0');
	ostr << setw(2) << m << Sep[M++];
	ostr << setprecision(Precision) << setw(2) << s << Sep[M++];
	ostr.fill(fillchar);
      }
    else
      {
	ostr << setw(2) << d << ' ';
	fillchar = ostr.fill('0');
	ostr << setw(2) << m << ' ';
	ostr << setprecision(Precision) << setw(2) << s;
	ostr.fill(fillchar);
      }

  if (e)
    {
      M=N;
      ostr << " +/- ";
      d=(int)e;
      if ((d==0) && (v<0)) ostr << "-";
      t=fabs(e-d);
      m=(int)(t*60.0);
      t=t-m/60.0;
      s=t*3600.0;

      if ((format==U_BITS) || (format==U_BITSX))
	PrtBits<unsigned long>(ostr,(unsigned long)e,format);
      else
	if (Sep && (strlen(Sep)>2))
	  {
	    ostr << setw(3) << d << Sep[M++] ;
	    fillchar = ostr.fill('0');
	    ostr << setw(2) << m << Sep[M++];
	    ostr << setprecision(Precision) << setw(2) << s << Sep[M++];
	    ostr.fill(fillchar);
	  }
	else
	  {
	    ostr << setw(2) << d << ' ';
	    fillchar = ostr.fill('0');
	    ostr << setw(2) << m << ' ';
	    ostr << setprecision(Precision) << setw(5) << s;
	    ostr.fill(fillchar);
	  }
      ostr << ")";
    }

  return ostr;
}

typedef long int Bool;
const Bool TRUE=1;
const Bool FALSE=0;
 
ostream& PrintF(ostream& os, const char *fmt)
{
  int i = 0;
  while (fmt[i] != 0)
    { 
      if (fmt[i] != '%') 
	{
	  os << fmt[i]; i++; 
	}
      else
	{ 
	  i++;
	  if (fmt[i] == '%') { os << fmt[i]; i++; }
	  else
	    { 
	      Bool ok = TRUE;
	      int istart = i;
	      Bool more = TRUE;
	      int width = 0;
	      int precision = 6;
	      std::ios::fmtflags flags = 0;
	      char fill = ' ';
	      Bool alternate = FALSE;
	      while (more)
		{ 
		  switch (fmt[i])
		    { 
		    case '+':
		      flags |= ios::showpos;
		      break;
		    case '-':
		      flags |= ios::left;
		      break;
		    case '0':
		      flags |= ios::internal;
		      fill = '0';
		      break;
		    case '#':
		      alternate = TRUE;
		      break;
		    case ' ':
		      break;
		    default:
		      more = FALSE;
		      break;
		    }
		  if (more) i++;
		}
	      if (isdigit(fmt[i])) 
		{ 
		  width = atoi(fmt+i); 
		  do i++; while (isdigit(fmt[i]));
		}
	      if (fmt[i] == '.')
		{ 
		  i++;
		  precision = atoi(fmt+i); 
		  while (isdigit(fmt[i])) i++;
		}
	      switch (fmt[i])
		{ 
		case 's': break;
		case 'S': break;
		case 'd':
		  flags |= ios::dec;
		  break;
		case 'x':
		  flags |= ios::hex;
		  if (alternate) flags |= ios::showbase;
		  break;
		case 'X':
		  flags |= ios::hex | ios::uppercase;
		  if (alternate) flags |= ios::showbase;
		  break;
		case 'o':
		  flags |= ios::hex;
		  if (alternate) flags |= ios::showbase;
		  break;
		case 'f':
		  flags |= ios::fixed;
		  if (alternate) flags |= ios::showpoint;
		  break;
		case 'e':
		  flags |= ios::scientific;
		  if (alternate) flags |= ios::showpoint;
		  break;
		case 'E':
		  flags |= ios::scientific | ios::uppercase;
		  if (alternate) flags |= ios::showpoint;
		  break;
		case 'g':
		  if (alternate) flags |= ios::showpoint;
		  break;
		case 'G':
		  flags |= ios::uppercase;
		  if (alternate) flags |= ios::showpoint;
		  break;
		default:
		  ok = FALSE;
		  break;
		}
	      i++;
	      if (fmt[i] != 0) ok = FALSE;
	      if (ok)
		{ 
		  os.unsetf(ios::adjustfield | ios::basefield | 
			    ios::floatfield);
		  os.setf(flags);
		  os.width(width);
		  os.precision(precision);
		  os.fill(fill);
		}
	      else i = istart;
	    }
	}
    }

  return os;
}
