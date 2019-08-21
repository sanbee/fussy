C
C Copyright 1984 P. T. Wallace.
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
      DOUBLE PRECISION FUNCTION SLADRANRM (ANGLE)
C
C
C
C  Normalise angle into range 0-2 pi  (double precision)
C
C
C  Given:
C
C     ANGLE     dp      the angle in radians
C
C
C  The result is ANGLE expressed in the range 0-2 pi (double
C  precision).
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   December 1984
C-----------------------------------------------------------------------


      DOUBLE PRECISION ANGLE

      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)

      SLADRANRM=MOD(ANGLE,D2PI)
      IF (SLADRANRM.LT.0D0) SLADRANRM=SLADRANRM+D2PI

      END
