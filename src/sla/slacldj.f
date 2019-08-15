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

C++
      SUBROUTINE SLACLDJ (IY,IM,ID,DJM,J)
C
C
C
C  Gregorian Calendar to Modified Julian Date
C
C
C  Given:
C     IY,IM,ID     int    year, month, day in Gregorian calendar
C
C  Returned:
C     DJM          dp     modified Julian Date (JD-2400000.5) for 0 hrs
C     J            int    status:
C                           0 = OK
C                           1 = bad year   (MJD not computed)
C                           2 = bad month  (MJD not computed)
C                           3 = bad day    (MJD computed)
C
C  The year must be -4699 (i.e. 4700BC) or later.
C
C  The algorithm is derived from that of Hatcher 1984
C  (QJRAS 25, 53-55).
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   December 1985
C-----------------------------------------------------------------------


      INTEGER IY,IM,ID
      DOUBLE PRECISION DJM
      INTEGER J

C  Month lengths in days
      INTEGER MTAB(12)
      DATA MTAB/31,28,31,30,31,30,31,31,30,31,30,31/



C  Preset status
      J=0

C  Validate year
      IF (IY.LT.-4699) THEN
         J=1
      ELSE

C     Validate month
         IF (IM.GE.1.AND.IM.LE.12) THEN

C        Allow for leap year
            IF (MOD(IY,4).EQ.0) THEN
               MTAB(2)=29
            ELSE
               MTAB(2)=28
            END IF
            IF (MOD(IY,100).EQ.0.AND.MOD(IY,400).NE.0)
     :         MTAB(2)=28

C        Validate day
            IF (ID.LT.1.OR.ID.GT.MTAB(IM)) J=3

C        Modified Julian Date
               DJM=DBLE((1461*(IY-(12-IM)/10+4712))/4
     :                  +(306*MOD(IM+9,12)+5)/10
     :                  -(3*((IY-(12-IM)/10+4900)/100))/4
     :                  +ID-2399904)

C        Bad month
         ELSE
            J=2
         END IF

      END IF

      END
