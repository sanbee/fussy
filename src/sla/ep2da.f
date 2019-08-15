C Copyright 2019 S. Bhatnagar.
C 
C This file is a part of Fussy.
C 
C Fussy is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C
C Fussy is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C 
C You should have received a copy of the GNU General Public License
C along with Fussy.  If not, see <https://www.gnu.org/licenses/>.

	subroutine ep2da(epoch,year,month,day,fday)

C-------------------------------------------------------------------------
C
C  This subroutine finds the year,month,day,fraction of the day,given
C  the epoch
C
C  input:
C	epoch   real*8  
C
C  output:
C	year    integer
C	month   integer
C	day     integer
C	fday    real
C
C------------------------------------------------------------------------

	real*8   epoch,oney,dayno,dbm(13),dbml(13),db(13)
	real  fday
	integer year,month,day,i,j
	parameter(oney=365.2425)
	logical ly,x

	data dbm/0,31,59,90,120,151,181,212,243,273,304,334,365/
	data dbml/0,31,60,91,121,152,182,213,244,274,305,335,366/

	year=dint(epoch)
	dayno=(epoch-year)*oney
c	write(*,*) 'ep2da' ,year,dayno
	
	if(dayno.eq.0.0)then
	    month=1
	    day=1
	    go to 10
	endif

	if(dayno.ge.365.0.or.dayno.ge.366.0) then
	    month=12
	    day=31
	    go to 10
	endif


	if(mod(year,100).eq.0)then
	    if(mod(year,4).eq.0)then
	         ly=.true.
	    else
		 ly=.false.
	    endif
	elseif(mod(year,4).eq.0)then
	    ly=.true.
	else
	    ly=.false.
	endif
	
	if(ly)then
	     do i=1,13
		db(i)=dbml(i)
	     end do
	else
	     do j=1,13
		db(j)=dbm(j)
	     end do
	endif

	do i=1,13
	   x=dble(db(i)).gt.dayno
	   if(x)then
	       month=i-1
	       day=(dayno-db(month))
	   if(day.lt.1)then
	       month=month-1
	       day=db(month+1)-db(month)
	   endif
	       fday=(dayno-db(month))-day
	       if(fday.gt..97) then
		  fday=fday-1.
		  day=day+1
	       endif
	       go to 10
	   endif
	end do
 10     return
	end

