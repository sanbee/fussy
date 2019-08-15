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
      SUBROUTINE SLAPREBN (BEP0,BEP1, RMATP)
C
C
C
C  Generate the matrix of precession between two epochs,
C  using the old, pre-IAU1976, Bessel-Newcomb model
C
C  (double precision)
C
C
C  Given:
C     BEP0    dp         beginning Besselian epoch
C     BEP1    dp         ending Besselian epoch
C
C  Returned:
C     RMATP  dp(3,3)    precession matrix
C
C
C  Reference:
C     Explanatory Supplement to the A.E., 1960, section
C      2B, p30.
C
C
C  The matrix is in the sense   V(BEP1)  =  RMATP * V(BEP0) .
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   July 1984
C-----------------------------------------------------------------------


      DOUBLE PRECISION BEP0,BEP1,RMATP(3,3)

      DOUBLE PRECISION AS2R
      DOUBLE PRECISION T0,T
      DOUBLE PRECISION TAS2R
      DOUBLE PRECISION ZETA0,Z,THETA
      DOUBLE PRECISION SZE,CZE,SZ,CZ,STH,CTH,CTHSZ,CTHCZ

C  Arc seconds to radians
      PARAMETER (AS2R=0.4848136811095359949D-05)



C  Interval between basic epoch B1900.0 and beginning epoch,
C   in tropical centuries
      T0=(BEP0-1900D0)/100D0

C  Interval over which precession required, in tropical centuries
      T=(BEP1-BEP0)/100D0

C  Rotations (Euler angles)
      TAS2R=T*AS2R

      ZETA0=(2304.250D0+1.396*T0+(0.302D0+0.018D0*T)*T)*TAS2R

      Z=ZETA0+0.791D0*T*TAS2R

      THETA=(2004.682D0-0.853D0*T0+(-0.42665-0.042D0*T)*T)*TAS2R

C  Elements of rotation matrix
      SZE=SIN(ZETA0)
      CZE=COS(ZETA0)

      SZ=SIN(Z)
      CZ=COS(Z)

      STH=SIN(THETA)
      CTH=COS(THETA)

      CTHSZ=CTH*SZ
      CTHCZ=CTH*CZ

      RMATP(1,1)= CZE*CTHCZ - SZE*SZ
      RMATP(2,1)= CZE*CTHSZ + SZE*CZ
      RMATP(3,1)= CZE*STH
      RMATP(1,2)=-SZE*CTHCZ - CZE*SZ
      RMATP(2,2)=-SZE*CTHSZ + CZE*CZ
      RMATP(3,2)=-SZE*STH
      RMATP(1,3)=-STH*CZ
      RMATP(2,3)=-STH*SZ
      RMATP(3,3)= CTH

      END
