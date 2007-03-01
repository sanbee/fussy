

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
