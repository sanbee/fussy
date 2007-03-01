


C++
      SUBROUTINE SLADJCL (DJM,IY,IM,ID,FD,J)
C
C
C
C  Modified Julian Date to Gregorian year, month, day,
C  and fraction of a day.
C
C
C  Given:
C     DJM      dp     modified Julian Date (JD-2400000.5)
C
C  Returned:
C     IY       int    year AD
C     IM       int    month
C     ID       int    day
C     FD       dp     fraction of day
C     J        int    status:
C                      -1 = unacceptable date (before 4701BC March 1)
C
C  The algorithm is derived from that of Hatcher 1984
C  (QJRAS 25, 53-55).
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   February 1987
C
C-----------------------------------------------------------------------


      DOUBLE PRECISION DJM
      INTEGER IY,IM,ID
      DOUBLE PRECISION FD
      INTEGER J

      DOUBLE PRECISION F,D
      INTEGER JD,N4,ND10



C  Check if date is acceptable
      IF (DJM.LE.-2395522D0.OR.DJM.GE.1D9) THEN
         J=-1
      ELSE
         J=0

C     Separate day and fraction
         F=MOD(DJM,1D0)
         IF (F.LT.0D0) F=F+1D0
         D=NINT(DJM-F)

C     Express day in Gregorian calendar
         JD=NINT(D)+2400001

         N4=4*(JD+((6*((4*JD-17918)/146097))/4+1)/2-37)
         ND10=10*(MOD(N4-237,1461)/4)+5

         IY=N4/1461-4712
         IM=MOD(ND10/306+2,12)+1
         ID=MOD(ND10,306)/10+1
         FD=F

         J=0

      END IF

	return

      END


