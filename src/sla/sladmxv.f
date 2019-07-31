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

C++
      SUBROUTINE SLADMXV (DM,VA,VB)
C
C
C
C  Performs the 3-D forward unitary transformation:
C     vector VB = matrix DM * vector VA
C
C  (double precision)
C
C
C  Given:
C     DM       dp(3,3)    matrix
C     VA       dp(3)      vector
C
C  Returned:
C     VB       dp(3)      result vector
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   March 1986
C-----------------------------------------------------------------------


      DOUBLE PRECISION DM(3,3),VA(3),VB(3)

      INTEGER I,J
      DOUBLE PRECISION W,VW(3)


C  Matrix DM * vector VA -> vector VW
      DO 11 J=1,3
         W=0D0
         DO 10 I=1,3
            W=W+DM(J,I)*VA(I)
 10      CONTINUE
         VW(J)=W
 11   CONTINUE

C  Vector VW -> vector VB
      DO 12 J=1,3
         VB(J)=VW(J)
 12   CONTINUE

      END
