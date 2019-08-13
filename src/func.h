/* $Id: func.h,v 1.2 2006/01/17 18:57:09 sbhatnag Exp $ */
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

******************************************************************/

#if !defined(FUNC_H)
#define FUNC_H
#include <tables.h>


int funcInit(int);
int funcstart(int NArgs);
int funcrun(Calc_Symbol *,int);
#endif
