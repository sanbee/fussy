
C++
      DOUBLE PRECISION FUNCTION SLAGMST (UT1)

C
C
C
C  Conversion from universal time to sidereal time
C
C  (double precision)
C
C  The IAU 1982 expression (see page S15 of 1984 Astronomical
C  Almanac) is used, but rearranged to reduce rounding errors.
C
C
C  Given:
C
C    UT1    dp     universal time (strictly UT1) expressed as
C                  modified Julian Date (JD-2400000.5)
C
C
C  The result is the Greenwich mean sidereal time (double
C  precision, radians).
C
C
C  Called:  SLADRANRM
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   September 1987
C
C-----------------------------------------------------------------------


      DOUBLE PRECISION UT1

      DOUBLE PRECISION SLADRANRM

      DOUBLE PRECISION D2PI,S2R
      PARAMETER (D2PI=6.283185307179586476925287D0,
     :           S2R=0.7272205216643039849D-4)

      DOUBLE PRECISION TU


C  Julian centuries from fundamental epoch J2000 to this UT
      TU=(UT1-51544.5D0)/36525D0

C  GMST at this UT
      SLAGMST=SLADRANRM(MOD(UT1,1D0)*D2PI+
     :                    (24110.54841D0+
     :                    (8640184.812866D0+
     :                    (0.093104D0-6.2D-6*TU)*TU)*TU)*S2R)

      END
