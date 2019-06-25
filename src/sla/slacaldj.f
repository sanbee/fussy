

C++
      SUBROUTINE SLACALDJ (IY,IM,ID,DJM,J)
C
C
C
C  Gregorian Calendar to Modified Julian Date
C
C  (Includes century default feature:  use SLACLDJ for years
C   before 100AD.)
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
C  Acceptable years are 00-49, interpreted as 2000-2049,
C                       50-99,     '       '  1950-1999,
C                       100 upwards, interpreted literally.
C
C  Called:  SLACLDJ
C
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   November 1985
C
C Copyright 2019 Sanjay Bhatnagar.
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
C-----------------------------------------------------------------------


      INTEGER IY,IM,ID
      DOUBLE PRECISION DJM
      INTEGER J

      INTEGER NY




C  Default century if appropriate
      IF (IY.GE.0.AND.IY.LE.49) THEN
         NY=IY+2000
      ELSE IF (IY.GE.50.AND.IY.LE.99) THEN
         NY=IY+1900
      ELSE
         NY=IY
      END IF

C  Modified Julian Date
      CALL SLACLDJ(NY,IM,ID,DJM,J)

      END
