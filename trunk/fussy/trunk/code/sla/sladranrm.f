

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
C
C-----------------------------------------------------------------------


      DOUBLE PRECISION ANGLE

      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)

      SLADRANRM=MOD(ANGLE,D2PI)
      IF (SLADRANRM.LT.0D0) SLADRANRM=SLADRANRM+D2PI

      END
