


C++
      SUBROUTINE SLAPREC (EP0,EP1, RMATP)
C
C
C
C  Form the matrix of precession between two epochs (IAU1976/FK5)
C
C
C  (double precision)
C
C
C  References:
C     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
C      equations (6) & (7), p283.
C     Kaplan,G.H., 1981. USNO circular no. 163, pA2.
C
C
C  Given:
C
C     EP0    dp         beginning epoch
C     EP1    dp         ending epoch
C
C
C  Returned:
C
C     RMATP  dp(3,3)    precession matrix
C
C
C  Notes:
C
C  1)  The epochs are TDB (loosely ET) Julian epochs.
C
C  2)  The matrix is in the sense   V(EP1)  =  RMATP * V(EP0) .
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   February 1984
C
C-----------------------------------------------------------------------


      DOUBLE PRECISION EP0,EP1,RMATP(3,3)

      DOUBLE PRECISION AS2R
      DOUBLE PRECISION T0,T
      DOUBLE PRECISION TAS2R
      DOUBLE PRECISION W
      DOUBLE PRECISION ZETA,Z,THETA
      DOUBLE PRECISION SZE,CZE,SZ,CZ,STH,CTH,CTHSZ,CTHCZ

C  Arc seconds to radians
      PARAMETER (AS2R=0.4848136811095359949D-05)



C  Interval between basic epoch J2000.0 and beginning epoch (JC)
      T0=(EP0-2000D0)/100D0

C  Interval over which precession required (JC)
      T=(EP1-EP0)/100D0

C  Rotations (Euler angles)
      TAS2R=T*AS2R
      W=2306.2181D0+(1.39656D0-0.000139D0*T0)*T0

      ZETA=(W+((0.30188D0-0.000344D0*T0)+0.017998D0*T)*T)*TAS2R

      Z=(W+((1.09468D0+0.000066D0*T0)+0.018203D0*T)*T)*TAS2R

      THETA=((2004.3109D0+(-0.85330D0-0.000217D0*T0)*T0)
     :      +((-0.42665D0-0.000217D0*T0)-0.041833D0*T)*T)*TAS2R

C  Elements of rotation matrix
      SZE=SIN(ZETA)
      CZE=COS(ZETA)

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
