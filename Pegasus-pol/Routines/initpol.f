* ..File: initpol.f    
*                   
*
* ..The global initialization for the polarized parton evolution. 
*
* ..This routine initializes some constants, the array  NA  of complex
*    support points (depending on  IFAST),  the corresponding weights
*    WN  for the Gauss quadrature employed for the Mellin inversion, 
*    and the simple harmonic sums S_i, i=1,...,6 on the support points.
*
* ..The corresponding arrays of N-space splitting functions, evolution
*    matrix elements and flavour-threshold operator matrix elements are 
*    then initialized (depending on  IMODEV, NPORD, IVFNS, NFF, FR2) 
*    by the respective subroutines and stored in common blocks by them. 
*
* ..The default values of the initialization parameters speficied below
*    can by overwritten either by reading the file  usrinit.dat  (for 
*    IPAR = 1)  or by calling the subroutine  USRINIT  (for IPAR = 2).
*
* =====================================================================
*
*
       SUBROUTINE INITPOL (IPAR)
* 
       IMPLICIT DOUBLE COMPLEX (A - Z)
       INTEGER IPAR, NINTM, NINTF, NI, NPTS, NDIM, NMAX, IFAST, IMODEV, 
     1         IVFNS, NFF, NFMIN, NFMAX, NFLOW, NFHIGH, NPORD, NUORD, 
     2         NAORD, NASTPS, K, K1, K2, K3
       PARAMETER (NINTM = 18, NINTF = 10, NPTS = 8, NDIM = NINTM* NPTS,
     1            NFMIN = 3,  NFMAX = 6)
*
c       PARAMETER (NINTM = 17, NINTF = 10, NPTS = 8, NDIM = NINTM* NPTS,
c     1            NFMIN = 3,  NFMAX = 6)


       DOUBLE PRECISION PI, EMC, ZETA, CF, CA, TR, FR2, LOGFR, C, PHI
       DOUBLE PRECISION WN(NDIM), DOWN(NINTM), DOWNF(NINTF), UP(NINTM), 
     1                  ZS(NPTS),  WZ(NPTS)
      DOUBLE PRECISION FLI4, JJ
*
* ---------------------------------------------------------------------
*
* ..Input common-blocks 
*
       COMMON / INVFST / IFAST
       COMMON / EVMOD  / IMODEV
       COMMON / ORDER  / NPORD
       COMMON / VARFLV / IVFNS
       COMMON / NFFIX  / NFF
       COMMON / FRRAT  / LOGFR
*
* ..Output common-blocks 
*
       COMMON / ASPAR  / NAORD, NASTPS
       COMMON / NFUSED / NFLOW, NFHIGH
       COMMON / ITORD  / NUORD
*
       COMMON / NNUSED / NMAX 
       COMMON / NCONT  / C, CC
       COMMON / WEIGHTS/ WN
       COMMON / MOMS   / NA(NDIM)
       COMMON / HSUMS  / S(NDIM,6)
       COMMON / HSUMSNEG  / SNEG(NDIM,4)
       COMMON / HSUMSTHR  / STHR(NDIM,2)
       COMMON / HSUMSFOR  / SFOR(NDIM,4)
       COMMON / HSUMSFOR2  / SEXTR(NDIM,8)                           
*
       COMMON / RZETA  / ZETA(6)
       COMMON / KRON2D / D(2,2)   
       COMMON / COLOUR / CF, CA, TR
*
* ---------------------------------------------------------------------
*
* ..Weights and support points for normalized 8 point gauss quadrature 
*
       DATA WZ
     1  / 0.10122 85362 90376 D0,  0.22238 10344 53374 D0, 
     2    0.31370 66458 77887 D0,  0.36268 37833 78362 D0, 
     3    0.36268 37833 78362 D0,  0.31370 66458 77887 D0,
     4    0.22238 10344 53374 D0,  0.10122 85362 90376 D0/
*
       DATA ZS
     1  /-0.96028 98564 97536 D0, -0.79666 64774 13627 D0, 
     2   -0.52553 24099 16329 D0, -0.18343 46424 95650 D0, 
     3    0.18343 46424 95650 D0,  0.52553 24099 16329 D0,
     4    0.79666 64774 13627 D0,  0.96028 98564 97536 D0/
*
* ..Default lower boundaries for the integration intervals
*
       DATA DOWN  / 0.D0,  0.1D0, 0.3D0, 0.6D0, 1.0D0, 1.6D0, 2.4D0,
     1              3.5D0, 5.0D0, 7.0D0, 10.D0, 14.D0, 19.D0, 25.D0,
     2              32.D0, 40.D0, 50.D0, 60.D0 /

c       DATA DOWN / 0.D0, 0.5D0, 1.D0, 2.D0, 3.D0, 4.D0, 6.D0, 8.D0,
c     1     1.D1, 1.2D1, 1.5D1, 1.8D1, 2.1D1, 2.4D1, 2.7D1, 3.D1, 3.3D1/


       DATA DOWNF / 0.D0,  0.5D0, 1.0D0, 2.4D0, 5.0D0, 10.D0, 17.D0,
     1              27.D0, 40.D0, 55.D0 /
*
* ---------------------------------------------------------------------
*
* ..Some constants and the two-dimensional Kronecker symbol
*
       PI  = 3.1415 92653 58979 D0
       EMC = 0.5772 15664 90153 D0
       ZETA(1) = EMC
       ZETA(2) = 1.64493 40668 48226 D0
       ZETA(3) = 1.20205 69031 59594 D0
       ZETA(4) = 1.08232 32337 11138 D0
       ZETA(5) = 1.03692 77551 43370 D0
       ZETA(6) = 1.01734 30619 84449 D0
*
       I = DCMPLX (0.D0, 1.D0)
       D(1,1) = DCMPLX (1.D0, 0.D0)
       D(1,2) = DCMPLX (0.D0, 0.D0)
       D(2,1) = DCMPLX (0.D0, 0.D0)
       D(2,2) = DCMPLX (1.D0, 0.D0)
*
* ..QCD colour factors
*
       CA = 3.D0
       CF = 4./3.D0
       TR = 0.5 D0

*
* ---------------------------------------------------------------------
*
* ..Some default settings of the external initialization parameters
*   (standard-speed iterated VFNS evolution at NLO for mu_f/mu_r = 1)
*
       IFAST  = 0
       IVFNS  = 1
       NFF    = 4
       IMODEV = 1
       NPORD  = 1
       FR2    = 1.D0
*
* ..Override these values by reading the file  usrinit.dat (for IPAR=1)
*    or by calling the subroutine  USRINIT (for IPAR=2)
*
       IF ( IPAR .EQ. 1) THEN
         OPEN (91,FILE='usrpinit.dat',STATUS='old')
         READ (91,*) IFAST
         READ (91,*) IVFNS
         READ (91,*) NFF
         READ (91,*) IMODEV
         READ (91,*) NPORD
         READ (91,*) FR2
         CLOSE(91)
       ELSE IF ( IPAR .EQ. 2) THEN
         CALL USRPINIT (IFAST, IVFNS, NFF, IMODEV, NPORD, FR2)
       END IF
*
       LOGFR = LOG(FR2)
*
* ..Stop some nonsense
*
       IF ( (IVFNS .EQ. 0) .AND. (NFF .LT. 3) ) THEN
         WRITE (6,*) 'Wrong flavour number for FFNS evolution. STOP'
         STOP
       END IF
       IF ( (IVFNS .EQ. 0) .AND. (NFF .GT. 5) ) THEN
         WRITE (6,*) 'Wrong flavour number for FFNS evolution. STOP'
         STOP
       END IF
*
       IF ( NPORD .GT. 2 ) THEN
         WRITE (6,*) 'Specified order in a_s too high. STOP'
         STOP
       END IF
*
       IF ( (IVFNS .NE. 0) .AND. (FR2 .GT. 4.001D0) ) THEN
         WRITE (6,*) 'Too low mu_r for VFNS evolution. STOP'
         STOP
       END IF
*
* ---------------------------------------------------------------------
* 
* ..Location of the Mellin inversion contour in the complex N plane 
*    (the C here corresponds to C-1 in Eq. (3.2) of the manual)
*
       C   = 0.99D0
       PHI = 3./4.D0 * PI
       CC  = EXP (I*PHI)
*
* ..Lower and upper boundaries for the integration intervals 
*
* ..More accurate option
*
       IF ( IFAST .EQ. 0) THEN
         DO 1 K1 = 2, NINTM
           DOWN(K1) = DOWN(K1)
           UP(K1-1) = DOWN(K1)
  1      CONTINUE
         NMAX = NDIM
         NI   = NINTM
*
* ..Faster option
*
       ELSE
         DO 11 K1 = 2, NINTF
           DOWN(K1) = DOWNF(K1)
           UP(K1-1) = DOWN(K1)
  11     CONTINUE
         NMAX = NINTF * NPTS
         NI   = NINTF
*
       END IF
       UP(NI)  = 70.D0
* 
* ---------------------------------------------------------------------
*
* ..Support points NA(K) and weights WN(K) for the gauss integrations  
*   (the factor [upper limit - lower limit]/2 is put into the weights)
*
      CALL ACINI 

       K = 0
       JJ = 0.D0
       DO 2 K2 = 1, NI
         SUM  = UP(K2) + DOWN(K2) 
         DIFF = UP(K2) - DOWN(K2) 
       DO 3 K3 = 1, NPTS
         K = K + 1
         JJ = JJ + 1.D0
         Z = (SUM + DIFF * ZS(K3)) * 0.5
         WN(K) = DIFF * 0.5 * WZ(K3) 
         NA(K) = CC * Z + C + 1.     
*
* ..The lowest simple harmonic sums on the support points
*
       N1 = NA(K) + 1.
       Ns21 = NA(K)/2. + 1.
       Np1s21 = (NA(K)+1.)/2. + 1.
       Np2s21 = (NA(K)+2.)/2. + 1.
       Nm1s21 = (NA(K)-1.)/2. + 1.      

       S(K,1) = PSI(N1) + EMC
       S(K,2) = ZETA(2) - DPSI (N1,1)
       S(K,3) = ZETA(3) + 0.5 * DPSI (N1,2)
       S(K,4) = ZETA(4) - 1./6.D0 * DPSI (N1,3)
       S(K,5) = ZETA(5) + 1./24.D0 * DPSI (N1,4)
       S(K,6) = ZETA(6) - 1./120.D0 * DPSI (N1,5)
*
* ..IB: Additional harmonic sums for the alphaS^2 Coefficients of g1
*       [S_{-k} (N):]
*    
      CALL BET(N1,V1) 
      CALL BET1(N1,V2) 
      CALL BET2(N1,V3) 
      CALL BET3(N1,V4) 
*                                          
       SNEG(K,1) = (-1.D0) * V1 - DLOG(2.D0)
       SNEG(K,2) = (1.D0)  * V2 - ZETA(2)/2.D0
       SNEG(K,3) = (-1.D0) * V3/2.D0 - 3.D0*ZETA(3)/4.D0
       SNEG(K,4) = (-1.D0) * V4/6.D0 - 7.D0/8.D0*ZETA(4)
*
*       [ S_{2,1},S_{-2,1}  (N):]
*
       STHR(K,1) = 2 * ZETA(3) - ACG18(NA(K))
       STHR(K,2) = ZETA(2)*SNEG(K,1) - 5.D0/8.D0*ZETA(3) +  
     1             ZETA(2)*DLOG(2.D0) +ACG3(NA(K))


C*
C*       [ S_{3,1},S_{-3,1},S_{-2,2},S_{2,1,1}  (N):]
C*
       SFOR(K,1) = ZETA(2)*S(K,2) -1/4.D0*S(K,2)**2 - 1/4.D0*S(K,4) -
     1             3.D0/20.D0*ZETA(2)**2 + 1/2.D0*ACG22(NA(K))

       SFOR(K,2) = ZETA(2)*SNEG(K,2) - ZETA(3)*SNEG(K,1)  
     1             - 3.D0/5.D0*ZETA(2)**2 + 2*CMPLX(FLI4(1./2.D0),0.D0) 
     2             + 3.D0/4.D0*ZETA(3)*DLOG(2.D0) 
     3             - 1./2.D0*ZETA(2)*DLOG(2.D0)**2
     4             + 1./12.D0*DLOG(2.D0)**4 
     5             + (-1.D0)*ACG6(NA(K))

       SFOR(K,3) = 2.D0*ZETA(2)*SNEG(K,2) + 3.D0/40.D0*ZETA(2)**2
     1            - 2.D0*SFOR(K,2) + (-1.D0)*ACG5(NA(K))

       SFOR(K,4) = 6.D0/5.D0*ZETA(2)**2 - ACG21(NA(K)) 


       SEXTR(K,1) = PSI(Ns21) + EMC
       SEXTR(K,2) = ZETA(2) - DPSI (Ns21,1)

       SEXTR(K,3) = PSI(Np1s21) + EMC
       SEXTR(K,4) = ZETA(2) - DPSI (Np1s21,1)

       SEXTR(K,5) = PSI(Np2s21) + EMC
       SEXTR(K,6) = ZETA(2) - DPSI (Np2s21,1)

       SEXTR(K,7) = PSI(Nm1s21) + EMC
       SEXTR(K,8) = ZETA(2) - DPSI (Nm1s21,1)

  3    CONTINUE
  2    CONTINUE 
*
* ---------------------------------------------------------------------
*
* ..Some more (internal) evolution and initialization parameters 
*
       NAORD  = NPORD
       NASTPS = 20 
*
       IF ( (IMODEV .EQ. 1) .OR. (IMODEV .EQ. 2) ) THEN
         NUORD = 15
       ELSE
         NUORD = NPORD
       END IF
*
       IF ( IVFNS .EQ. 0) THEN
         NFLOW  = NFF
         NFHIGH = NFF
       ELSE 
         NFLOW  = NFMIN
         NFHIGH = NFMAX
       END IF
*
* ---------------------------------------------------------------------
*
* ..The N-space splitting functions, evolution operators etc.
*
       CALL BETAFCT
       CALL PNS0MOM 
       CALL PSG0PMOM 
       CALL LSGMOM 
*
       IF (NPORD .GT. 0) THEN
         CALL PNS1PMOM 
         CALL PSG1PMOM 
         CALL USG1MOM 
         IF (NUORD .GT. 1) CALL USG1HMOM
       END IF
*
* ..For later, when the polarized 3-loop splitting functions are known
*
      IF (NPORD .GT. 1) THEN
        CALL PNS2PMOM 
        CALL PSG2PMOM 
        CALL UNS2MOM 
        CALL USG2MOM 
        IF (NUORD .GT. 2) CALL USG2HMOM
      END IF
*
       IF (IVFNS .NE. 0) THEN
         CALL ANS2PMOM
         CALL ASG2PMOM
       END IF
*
* ---------------------------------------------------------------------
*
       RETURN
       END 
*
* =================================================================av==
