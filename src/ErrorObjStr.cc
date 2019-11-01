// $Id$
#include <ErrorObjStr.h>
/******************************************************************
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
******************************************************************/
//
// Constructor for the ErrorObj type.
//
//   const string& m    The NULL terminated error message string
//   const string& i    The NULL terminated error identifier string.
//   int l            The severity level of the error.
//
ErrorObj::ErrorObj(const string& m, const string& i, int l)
  :Message(), Src(), Msg(), Id()
  //:Id(i,strlen(i)),Msg(m,strlen(m))
{
  Id = i;
  Msg = m;
  Message.resize(0);
  Level=l;
}
//
// Method to set the source at which the error was generated.
//
// const char *s    The NULL terminated string to identify the
//                  source of error.
//
void ErrorObj::SetSource(const string& s)
{
  Src=s;
}
//
// Method to return the entire message string (error message
// + the identifier string).
//
const string ErrorObj::what()
{
  Message = Id + Message + string(": ") + Msg;
  // int N=0;
  // N += Id.length();
  // N += Msg.length() + 2 + 1;

  // if (Message) {delete Message; Message=NULL;}
  // Message = new char [N];
  // if (Id) {strcpy(Message,Id); strcat(Message,": ");}
  // if (Msg) strcat(Message,Msg);
  return Message;
}
//
// opertor<<() overloaded to print the ErrorObj.
//
ostream &operator<<(ostream& o, const ErrorObj &E)
{
  if (E.Id.length() > 0) o << E.Id;
  if (E.Msg.length() > 0) o << ": "<<E.Msg;

  // if (E.Id && (strlen(E.Id) > 0))    o << E.Id;
  // if (E.Msg && (strlen(E.Msg) > 0))  o <<": "<< E.Msg;
  return o;
}
