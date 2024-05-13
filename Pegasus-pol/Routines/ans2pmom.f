*
* ..File: ans2pmom.f     
*
*
* ..The polarized counterpart to  ANS2MOM  in  ans2mom.f.  
*    Contribution  DA2NS  to the non-singlet operator matrix element 
*    (OME) in N-space in the MS(bar) scheme for mu_f^2 = m_H^2.
*    The coupling constant is normalized as  a_s = alpha_s/(4*pi).
*
* ..This quantity, presented in Eq. 133 of Bierenbaum, Blümlein, 
*    De Freitas, Goedicke and Klein, Nucl.Phys.B 988 (2023) 116114 
*    e-Print: 2211.15337 [hep-ph], is required 
*    for the N_f matching of the NNLO polarized parton densities.
*
* ..The results (written to the common-block  ANS2)  are computed on an 
*    external NDIM-dimensional array  NA  of complex Mellin moments 
*    provided by the common-block  MOMS. 
*
* ..The SU(N_colours=3) colour factors  CF, CA and TF  are taken from
*    the common-block  COLOUR.  The simple harmonic sums S_i(N) are
*    provided by the common-block  HSUMS,  and the lowest integer
*    values of the Riemann Zeta-function are provided by  RZETA.
* =====================================================================
*
*
       SUBROUTINE ANS2PMOM 
*
       IMPLICIT DOUBLE COMPLEX (A - Z)
       INTEGER NMAX, NDIM, KN
       PARAMETER ( ZERO = (0.D0, 0.D0) )
       PARAMETER ( NDIM = 144 )
c       PARAMETER ( NDIM = 136 )
       DOUBLE PRECISION ZETA(6), CF, CA, TR       
*
* ---------------------------------------------------------------------
*
* ..Input common-block
*
       COMMON / MOMS   / NA (NDIM)
       COMMON / NNUSED / NMAX
       COMMON / HSUMS  / S(NDIM,6)
       COMMON / RZETA  / ZETA
       COMMON / COLOUR / CF, CA, TR
*
* ..Output common-block 
*
       COMMON / ANS2   / A2NS (NDIM)      
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
       N1 = N + 1.
       NI = 1./N
       N1I = 1./N1
*
       S1M = S1 - NI
       S2M = S2 - NI*NI
       S3M = S3 - NI**3
       S21 = S2 + N1I*N1I
       S31 = S3 + N1I**3
*
* ---------------------------------------------------------------------
*
*  ..Moments of the basic x-space functions 
*
       A0 = - S1M
*
       C0 = NI
       C1 = N1I
*
       D1  = - NI*NI
       D11 = - N1I*N1I
*
       G1  = S2M - ZETA(2)  
       G12 = S21 - ZETA(2)  
       G2  = - 2.* ( S3M - ZETA(3) )  
       G22 = - 2.* ( S31 - ZETA(3) )  
*
* ---------------------------------------------------------------------
*
* ..The moments of the OME \Delta A_{qq,H}^{NS,(2)} given in 
*   Eq. (133) of BBDGK 
*
*
        A2QQ = - 8./3.D0 * S3 -8./3.D0 * ZETA(2) * S1 + 40./9.D0 * S2 
     1         + 2.D0 * (3.*N**2 + 3.*N +2.) * ZETA(2) / (3.*N*N1)
     2         - 224./27.D0 * S1 + (219.* N**6 + 657.* N**5 
     3         + 1193.* N**4 + 763.* N**3 - 40.*N**2 -48.* N + 72.)/
     4           (54. * N**3 * N1**3)

       G0QQ = - 1./4.D0 * (-4./3.D0) * (-1.) * ( 2.D0 * (2. + 3.*N
     1           + 3.*N**2)/(N*N1) - 8.D0 *S1 ) * ZETA(2)    
*
* ..Output to the array 
*
       A2NS(KN) = CF*TR * (A2QQ + G0QQ)
*
* ---------------------------------------------------------------------
*
  1    CONTINUE
*
       RETURN
       END
*
* =================================================================av==
