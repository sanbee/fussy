
C++
      SUBROUTINE SLAPRECES (SYSTEM,EP0,EP1,RA,DC)
C
C
C
C  Precession - either FK4 (Bessel-Newcomb, pre-IAU1976) or
C  FK5 (Fricke, post-IAU1976) as required.
C
C
C  Given:
C     SYSTEM     char   precession to be applied: 'FK4' or 'FK5'
C     EP0,EP1    dp     starting and ending epoch
C     RA,DC      dp     RA,Dec, mean equator & equinox of epoch EP0
C
C  Returned:
C     RA,DC      dp     RA,Dec, mean equator & equinox of epoch EP1
C
C  Called:    SLADRANRM, SLAPREBN, SLAPREC, SLADCS2C,
C             SLADMXV, SLADCC2S
C
C  Notes:
C
C  1)  The epochs are Besselian if SYSTEM='FK4' and Julian if 'FK5'.
C      For example, to precess coordinates in the old system from
C      equinox 1900.0 to 1950.0 the call would be:
C          CALL SLAPRECES ('FK4', 1900D0, 1950D0, RA, DC)
C
C  2)  This routine will NOT correctly convert between the old and
C      the new systems - for example conversion from B1950 to J2000.
C      For these purposes see SLAFK425, SLAFK45Z, SLAFK54Z.
C
C  3)  If an invalid SYSTEM is supplied, values of -99D0,-99D0 will
C      be returned for both RA and DC.
C
C  Adapted from original code by P.T.Wallace, with permission
C
C  P.T.Wallace   Starlink   March 1986
C
C-----------------------------------------------------------------------


      CHARACTER SYSTEM*(*)
      DOUBLE PRECISION EP0,EP1,RA,DC

      DOUBLE PRECISION PM(3,3),V1(3),V2(3)

      DOUBLE PRECISION SLADRANRM




C  Validate SYSTEM
      IF (SYSTEM.NE.'FK4'.AND.SYSTEM.NE.'FK5') THEN
         RA=-99D0
         DC=-99D0
      ELSE

C     Generate appropriate precession matrix
         IF (SYSTEM.EQ.'FK4') THEN
            CALL SLAPREBN(EP0,EP1,PM)
         ELSE
            CALL SLAPREC(EP0,EP1,PM)
         END IF

C     Convert RA,Dec to x,y,z
         CALL SLADCS2C(RA,DC,V1)

C     Precess
         CALL SLADMXV(PM,V1,V2)

C     Back to RA,Dec
         CALL SLADCC2S(V2,RA,DC)
         RA=SLADRANRM(RA)

      END IF

      END


