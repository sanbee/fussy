C
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

	subroutine djm2e(djm,epoch)
C-----------------------------------------------------------------
C   This subroutine converts modified julian day number to 
C   epoch
C
C   input:
C	  djm(modified julian day number)  double precision
C
C   output:
C	  epoch                            double precision
C
C   called:
C	   sladjcl,frye
C-----------------------------------------------------------------

	double precision djm,epoch,fd,day
	integer iy,im,id,j
	
	call sladjcl(djm,iy,im,id,fd,j)
	

	day=id+fd

	call frye(iy,im,day,epoch)

	return
	end
