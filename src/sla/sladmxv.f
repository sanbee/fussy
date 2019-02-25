

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
C
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
