/* $Id$ */
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
// The ErrorObj class.  An exception of this type is thrown
// to report errors during VM execution or parsing.
//
#if !defined(ERROROBJ_H)
#define      ERROROBJ_H

#include <string>
#include <cstring>
#include <iostream>
#include "namespace.h"
#include <stdlib.h>
using namespace std;

class ErrorObj{
 public:
  enum {Informational=100,Recoverable,Severe,Fatal};
  ErrorObj():Message(), Src(), Msg(), Id(),Level(Informational){};
  ErrorObj(const string &m,const string& i,int l=0);
  ~ErrorObj(){};
  //  delete() {cleanup();};
  void cleanup() {Msg.resize(0); Id.resize(0); Src.resize(0); Message.resize(0);};
  
  void SetSource(const string& m=0);
  string Source() {return Src;}
  int Severity() {return Level;}
  const string what();

  ostream &operator<<(const std::string &m) {return cerr << m;}
  ostream &operator<<(ErrorObj &E) {return cerr << E;}
  friend ostream &operator<<(ostream& o,const ErrorObj&);

 private:
  string Message,Src,Msg,Id;
  int Level;
};

ostream &operator<<(ostream& o, const ErrorObj &E);

#endif
