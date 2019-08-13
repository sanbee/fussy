//-*-C++-*-
// $Id: IDResource.h,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $
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
 *

  History:

    First version.

               Sanjay Bhatnagar, Dec 2001
               sanjay@ncra.tifr.res.in

******************************************************************/

#ifndef IDRESOURCE_H
#define IDRESOURCE_H

#include <stdio.h>
#include <stdlib.h>
#include "BitField.h"

class IDResource{
 public:
  IDResource():IDList(){}
  ~IDResource() {}

  unsigned int GetNewID();
  void ReleaseID(unsigned int);
  void prtIDList();
  unsigned int HighestID();

 private:
  BitField IDList;
};

#endif
