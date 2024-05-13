*
* ..File: asg2pmom.f     
*
*
* ..The polarized counterpart to  ASG2MOM  in  asg2mom.f.  
*
* ..These quantities, presented in  Bierenbaum, Blümlein, 
*    De Freitas, Goedicke and Klein, Nucl.Phys.B 988 (2023) 116114 
*    e-Print: 2211.15337 [hep-ph], are required 
*    for the N_f matching of the NNLO parton densities.
*
* ..The results (written to the common-block  ANS2)  are computed on an 
*    external NDIM-dimensional array  NA  of complex Mellin moments 
*    provided by the common-block  MOMS. 
*
* ..The SU(N_colours=3) colour factors  CF, CA and TF  are taken from
*    the common-block  COLOUR.  The simple harmonic sums S_i(N) are
*    provided by the common-block  HSUMS,  and the lowest integer
*    values of the Riemann Zeta-function are provided by  RZETA.
*
* =====================================================================
*
*
       SUBROUTINE ASG2PMOM 
*
       IMPLICIT DOUBLE COMPLEX (A - Z)
       INTEGER NMAX, NDIM, KN, NFMIN, NFMAX, NF
       PARAMETER ( ZERO = (0.D0, 0.D0) )
       PARAMETER ( NDIM = 144, NFMIN = 3, NFMAX = 6 )
       DOUBLE PRECISION ZETA(6), CF, CA, TR       
* ---------------------------------------------------------------------
*
* ..Input common-blocks 
*
       COMMON / MOMS   / NA (NDIM)
       COMMON / NNUSED / NMAX
       COMMON / HSUMS  / S(NDIM,6)
       COMMON / RZETA  / ZETA
       COMMON / COLOUR / CF, CA, TR
*
* ..Output common-block 
*
       COMMON / ASG2   / A2SG (NDIM, NFMIN:NFMAX, 2, 2)
*
* ---------------------------------------------------------------------
*
* ..Begin of the Mellin-N loop
*
       DO 1 KN = 1, NMAX
*
* ..Some abbreviations
*
       N  = NA(KN)
       S1 = S(KN,1)
       S2 = S(KN,2)
       S3 = S(KN,3)
*
       NM = N - 1.
       N1 = N + 1.
       N2 = N + 2.
       NI = 1./N
       NMI = 1./NM
       N1I = 1./N1
       N2I = 1./N2
*
       S1M = S1 - NI
       S2M = S2 - NI*NI
       S3M = S3 - NI**3
       S11 = S1 + N1I
       S21 = S2 + N1I*N1I
       S31 = S3 + N1I**3
       S22 = S21 + N2I*N2I
*
      CALL BET(N1,V1) 
      CALL BET1(N1,V2) 
      CALL BET2(N1,V3) 
      CALL BET3(N1,V4)
*
*             
* ..The moments of the OME's DA_Hq^{PS,(2)} and DA_Hg^{S,(2)} given in 
*    Eqs. (138) and (111) of BBDKS. Note that for the former, an 
*    additional finite renormalization is needed to go from the Larin 
*    to the the M scheme
*
*
* ... Anomalous dimension terms
*
       G0QG_HAT = - 8.D0 * TR * NM/N/N1
*
       G0GQ = - 4.D0 * CF * N2/N/N1
*
       G0QQ = - CF * (2.D0*(2.+3.*N+3.*N**2)/N/N1-8.*S1)       
*
* ... Polinomials in N
       POL1 = 12.D0*N**8 + 52.D0*N**7 + 60.D0*N**6 -25.D0*N**4 
     1        - 2.D0*N**3 + 3.D0*N**2 + 8.D0*N + 4.D0  
*     
       POL2 = 2.*N**8 + 10.*N**7 + 22.*N**6 +36.*N**5 + 29.*N**4 
     1        + 4.*N**3 + 33.*N**2 + 12.*N + 4.D0
*
       POLR3 = 15.D0*N**6 + 45.D0*N**5 + 374.D0*N**4 + 601.D0*N**3
     1        + 161.D0*N**2 - 24.D0*N + 36.D0   
*
       POLR8 = -15.D0*N**8 -60.D0*N**7 - 82.D0*N**6 - 44.D0*N**5
     1        - 15.D0*N**4 - 4.D0*N**2 -12.D0*N -8.D0  
*                   
* ... Finite renormalization term from Larin to M scheme
*  
       ZQQPS = - CF * TR  * 8.D0*N2*(N**2-N-1.)/N**3/N1**3
*
       A2HQ = - 4.D0 * CF * TR * N2/N**2/N1**2 * (NM * (2.D0*S2+ZETA(2)) 
     1        - (4.D0*N**3 - 4.D0*N**2 - 3.D0*N - 1.D0)/N**2/N1**2)
*
     2       + ZETA(2)/8.D0 * G0QG_HAT * G0GQ
* 
*
       A2HG = CF*TR * (
     1         4./3.D0*NM/N/N1*(-4.*S3 + S1**3 +3.*S1*S2+ 6.*S1*ZETA(2)) 
     2        - 4.D0*(N**4 + 17.*N**3 + 43.*N**2 +33.*N
     3        + 2.D0)*S2/N**2/N1**2/N2 
     4        - 4.D0*(3.*N**2+3.*N-2.D0)*S1**2/N**2/N1/N2 
     5        - 2.D0*NM*(3.*N**2+3.*N+2.D0)*ZETA(2)/N**2/N1**2
     6        - 4.D0*(N**3-2.*N**2-22.*N-36.D0)*S1/N**2/N1/N2
     7        - 2.D0*POL1/N**4/N1**4/N2)
*     
     8        + CA * TR * (
     9           4.D0*(N**2+4.*N+5.D0)*S1**2/N/N1**2/N2  
     1        + 4.D0*(7.*N**3+24.*N**2+15.*N-16.D0)*S2/N**2/N1**2/N2
     2        + 8.D0*NM*N2*ZETA(2)/N**2/N1**2 
     3        + 4.D0*(N**4+4.*N**3-N**2-10.*N+2.d0)*S1/N/N1**3/N2
     4        - 4.D0*POL2/N**4/N1**4/N2
     5        - 16.D0*NM/N/N1**2*V2 
     6        + 4.D0*NM/3./N/N1*(12.*ACG3(N1)+3.*V3-8.*S3-S1**3
     7        - 9.*S1*S2 -12.*S1*V2 -12.*V1*ZETA(2)-3.*ZETA(3)))
*...  added simplified Gamma0_gg+2*beta0
     8        +1/8.D0*G0QG_HAT*(8.D0*CA*(-2./N/N1+S1)-G0QQ)
*
* ..The moments of the OME's DA_{gq,H}^{S,(2)} and DA_{gg,H}^{S,(2)} 
*    given in Eqs. (175) and (188) of Bierenblaum et al.
*
       A2GQ =  CF * TR *N2*(8.D0*(22.+41.*N+28.*N**2)/27./N/N1**3
     1         - 8.D0*(2.+5.*N)*S1/9./N/N1**2
     2         + 4.D0*(S1**2+S2)/3./N/N1) 
*
       A2GG =  CA*TR*(2.D0*POLR3/27./N**3/N1**3 
     1         - 4.D0*(47.+56.*N)*S1/27./N1)
     2         +CF*TR* POLR8/N**4/N1**4  
*
* ---------------------------------------------------------------------
*
* ..Flavour-number loop and output to the array 
*
       DO 2 NF = NFMIN, NFMAX
*
       A2SG(KN,NF,1,1) = (A2HQ + NF * ZQQPS) 
       A2SG(KN,NF,1,2) = A2HG               
       A2SG(KN,NF,2,1) = A2GQ               
       A2SG(KN,NF,2,2) = A2GG               
*
* ---------------------------------------------------------------------
*
  2    CONTINUE
  1    CONTINUE
*
       RETURN
       END
*
* =================================================================av==
