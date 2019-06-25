

C++
      SUBROUTINE SLADCC2S (V, A, B)
C
C
C
C  Direction cosines to spherical coordinates (double precision)
C
C
C  Given:
C     V     dp(3)   x,y,z vector
C
C  Returned:
C     A,B   dp      spherical coordinates in radians
C
C
C  The spherical coordinates are longitude (+ve anticlockwise
C  looking from the +ve latitude pole) and latitude.  The
C  Cartesian coordinates are right handed, with the x axis
C  at zero longitude and latitude, and the z axis at the
C  +ve latitude pole.
C
C  Adapted from original code by P.T.Wallace, with permission
C
C
C  P.T.Wallace   Starlink   July 1986
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


      DOUBLE PRECISION V(3),A,B

      DOUBLE PRECISION X,Y


      X=V(1)
      Y=V(2)

      IF (X.EQ.0D0.AND.Y.EQ.0D0) THEN
         A=0D0
      ELSE
         A=ATAN2(Y,X)
      END IF
      B=ATAN2(V(3),SQRT(X*X+Y*Y))

      END
