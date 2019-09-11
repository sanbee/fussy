/* $Id: ErrorObj.h,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $ */
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

  History:

    First version.

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in

******************************************************************/
//
// The ErrorObj class.  An exception of this type is thrown
// to report errors during VM execution or parsing.
//
#if !defined(ERROROBJ_H)
#define      ERROROBJ_H

#include <cstring>
#include <iostream>
#include "namespace.h"
#include <stdlib.h>

class ErrorObj{
 public:
  enum {Informational=100,Recoverable,Severe,Fatal};
  ErrorObj(){Msg = Id = Message = Src = NULL;};
  ErrorObj(const char *m,const char *i,int l=0);
  ~ErrorObj(){cleanup();}
  //  delete() {cleanup();};
  void cleanup()
    {
      if (Msg)     {delete Msg;Msg=NULL;} 
      if (Id)      {delete Id; Id=NULL;}
      if (Src)     {delete Src; Src=NULL;}
      //      if (Message) {delete Message;Message=NULL;}
    };
  
  void SetSource(const char *m=0);
  char *Source() {return Src;}
  int Severity() {return Level;}
  const char *what();

  ostream &operator<<(const char *m) {return cerr << m;}
  ostream &operator<<(const std::string &m) {return cerr << m;}
  ostream &operator<<(ErrorObj &E) {return cerr << E;}
  friend ostream &operator<<(ostream& o,const ErrorObj&);

 private:
  char *Id,*Msg,*Src,*Message;
  int Level;
};

ostream &operator<<(ostream& o, const ErrorObj &E);

#endif
