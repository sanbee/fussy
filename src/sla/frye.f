C
C Copyright 1985 P. T. Wallace.
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
	

	subroutine frye(iy,imon,day,epoch)

C-----------------------------------------------------------------------------
C  This subroutine gives the date in the fractional form of the year
C
C input:
C	iy (year)             integer
C	im (month)	      integer
C	day(day)	      double precision
C
C output:
C       epoch                  double precision
C-----------------------------------------------------------------------------


	double precision epoch,day
	double precision moday,daynumber,yeday
	integer cons,iy,imon,im
	parameter(yeday=365.2425)

	im=imon
	if(mod(iy,100).eq.0)then
              if(mod(iy,400).eq.0)then
                    cons=62
	      else
		    cons=63
	      endif
	elseif(mod(iy,4).eq.0)then
	     cons=62
	else
	     cons=63
	endif

	if(im.gt.2)then
	     go to 10
	endif
	
	im=im-1
	
	moday=int(im*cons/2.0)

	go to 20
 
 10     im=im+1
	moday=int(im*30.6)-cons

 20     daynumber=moday+day

	epoch=iy+daynumber/yeday
	
	return
	end
