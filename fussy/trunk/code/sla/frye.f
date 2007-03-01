	

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
