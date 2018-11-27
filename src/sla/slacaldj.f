

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
