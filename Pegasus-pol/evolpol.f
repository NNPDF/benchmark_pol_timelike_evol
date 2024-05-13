*
* ..File poltest.f
*
*
* ..A sample main program for the (longitudinally) polarized evolution.
*   The internal default input is given in Eq. (4.4) of the manual.
*
* =====================================================================
*
*
       PROGRAM POLTEST
*
       IMPLICIT DOUBLE PRECISION (A - Z)
       DIMENSION PDFX(-6:6), XB(11)
       PARAMETER ( PI = 3.1415 92653 58979 D0 )
       INTEGER K1, K2, K3
*
* ..Access the first moments and normalizations of the input
*
       COMMON / PANORM / AUV, ADV, ALS, ASS, AGL,
     ,                   NUV, NDV, NLS, NSS, NGL
*
* ..Lists of mu_f^2 and x for LH benchmark
*
       DATA M2 / 1.D4 /
       DATA XB / 1.D-7,  1.D-6,  1.D-5,  1.D-4,  1.D-3,  1.D-2,
     ,           1.D-1,  3.D-1,  5.D-1,  7.D-1,  9.D-1 /
*
* ---------------------------------------------------------------------
*
* ..General initialization 
*
       CALL INITPOL (2)
*
* ..Input initialization 
*   (with the extra loop for run-time investigations)
*
       DO 3 K3 = 1, 1
*
       CALL INITPINP (2)
*
* ..Output of the first moments and normalizations (only once)
*
       IF (K3 .EQ. 1) THEN
       WRITE(6,10) AUV, ADV, ALS, ASS, AGL
  10   FORMAT (1X,'AUV =',F9.5,2X,'ADV =',F9.5,2X,'ALS =',F9.5,
     ,         2X,'ASS =',F9.5,2X,'AGL =',F9.5)
*
       WRITE(6,11) NUV, NDV, NLS, NSS, NGL
  11   FORMAT (1X,'NUV =',F9.5,2X,'NDV =',F9.5,2X,'NLS =',F9.5,
     ,         2X,'NSS =',F9.5,2X,'NGL =',F9.5,/)
       END IF
*
* ---------------------------------------------------------------------
*
* ..Loop over mu^2 and x 
*
*
        DO 1 K1 = 1, 11
         X = XB (K1)
*
* ..Call of the Mellin inversion, reconstruction of L+ and L-
*
         CALL XPARTON (PDFX, AS, X, M2, -5, 5, 0)
*
         LMI = (PDFX(-2) - PDFX(-1) - PDFX(2) + PDFX(1)) * 0.5
         LPL =  PDFX(-1) + PDFX(-2) - PDFX(1) - PDFX(2)
*
* ..Output 
*
         IF (K3 .EQ. 1) THEN
         IF (K1 .EQ. 1) WRITE (6,12) M2, AS * 4*PI
  12     FORMAT (/,2X,'mu^2 =  ',1PE7.1,4X,
     ,          'alpha_s(mu_r^2(mu^2)) =  ',0PF9.6,/)
*
         IF (K1 .EQ. 1) WRITE (6,13)
  13     FORMAT (2X,'x',8X,'xu_v',7X,'xd_v',7X,'xL_-',7X,'xL_+',
     ,           7X,'xs_+',7X,'xc_+',7X,'xb_+',7X,'xg',/)
         WRITE (6,14) X, PDFX(1), PDFX(2), LMI, LPL, PDFX(-3),
     ,                   PDFX(-4), PDFX(-5), PDFX(0)
  14     FORMAT (1PE6.0,1X,8(1PE14.6))
         END IF
* 
   1    CONTINUE
   2   CONTINUE
   3   CONTINUE
*
       STOP
       END
*
* =================================================================av==
