// $Id: ErrorObj.cc,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $
#include <ErrorObj.h>
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

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in

******************************************************************/
//
// Constructor for the ErrorObj type.
//
//   const char *m    The NULL terminated error message string
//   const char *i    The NULL terminated error identifier string.
//   int l            The severity level of the error.
//
ErrorObj::ErrorObj(const char *m, const char *i, int l)
  //:Id(i,strlen(i)),Msg(m,strlen(m))
{
  if (i)  {Id = new char [strlen(i)+1];  strcpy(Id,i);}
  if (m)  {Msg = new char [strlen(m)+1]; strcpy(Msg,m);}
  Message = NULL;
  Level=l;
}
//
// Method to set the source at which the error was generated.
//
// const char *s    The NULL terminated string to identify the
//                  source of error.
//
void ErrorObj::SetSource(const char *s)
{
  if (s) {Src = new char [strlen(s)+1]; strcpy(Src,s);}
}
//
// Method to return the entire message string (error message
// + the identifier string).
//
const char* ErrorObj::what()
{
  int N=0;
  if (Id) N += strlen(Id);
  if (Msg) N += strlen(Msg) + 2 + 1;

  if (Message) {delete Message; Message=NULL;}
  Message = new char [N];
  if (Id) {strcpy(Message,Id); strcat(Message,": ");}
  if (Msg) strcat(Message,Msg);
  return Message;
}
//
// opertor<<() overloaded to print the ErrorObj.
//
ostream &operator<<(ostream& o, const ErrorObj &E)
{
  if (E.Id && (strlen(E.Id) > 0))    o << E.Id;
  if (E.Msg && (strlen(E.Msg) > 0))  o <<": "<< E.Msg;
  return o;
}
