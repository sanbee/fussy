/* -*-C++-*- */
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

               Sanjay Bhatnagar, Dec 2001
               sanjay@ncra.tifr.res.in

******************************************************************/
s(s1,s2,n1,n2) {return log(s1/s2)/log(n1/n2);}
s1=0.915pm0.071
s2=2.215pm0.166
n1=610
n2=240
t=log(s1/s2)/log(n1/n2);
t=sqrt((s1.rms/s1.val)^2 + (s2.rms/s2.val)^2)/log(n1/n2)
print "Spectal index = ",s(s1,s2,n1,n2),"\nComputed error by hand = ",t,"\n";
quit

