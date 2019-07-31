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
      SUBROUTINE SLADCS2C (A, B, V)
C
C
C
C  Spherical coordinates to direction cosines (double precision)
C
C
C  Given:
C     A,B       dp      spherical coordinates in radians
C                        (RA,Dec), (Long,Lat) etc
C
C  Returned:
C     V         dp(3)   x,y,z unit vector
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
C  P.T.Wallace   Starlink   October 1984
C-----------------------------------------------------------------------


      DOUBLE PRECISION A,B,V(3)

      DOUBLE PRECISION COSB



      COSB=COS(B)

      V(1)=COS(A)*COSB
      V(2)=SIN(A)*COSB
      V(3)=SIN(B)

      END
