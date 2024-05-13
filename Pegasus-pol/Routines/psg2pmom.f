*
* ..File: psg2mom.f     (requires previous call of PNS2MOM)
*
*
* ..The routine  PSG2MOM  for the polarized parametrized 
*    NNLO (alpha_s^3) singlet QCD splitting functions  P2NS  in N-space 
*    for  NF = NFMIN .. NFMAX = 3...6  massless quark flavours in the 
*    MS(bar) scheme. The coupling constant is normalized as  
*    a_s = alpha_s/(4*pi). 
*
* ..These quantities are determined on an external NDIM-dimensional 
*    array  NA  of complex Mellin moments provided by the common-block 
*    MOMS.  The results are written to the common-block  PSG2. The last
*    two array arguments refer to the quark-gluon mixing matrix with
*    1 = q  and  2 = g.
*
* ..The QCD colour factors have been hard-wired in the approximations.
*    The harmonic sums S_i(N) and the lowest integer values of the Zeta
*    function are provided by the common-blocks  HSUMS  and  RZETA.
*
* =====================================================================
*
*
       SUBROUTINE PSG2PMOM 
*
       IMPLICIT DOUBLE COMPLEX (A - Z)
       INTEGER NMAX, NDIM, NFMIN, NFMAX, IAPP2, KN, NF
       PARAMETER (NDIM = 144, NFMIN = 3, NFMAX = 6)
       DOUBLE PRECISION ZETA(6)
*
* ---------------------------------------------------------------------
*
* ..Input common-blocks 
*
       COMMON / MOMS   / NA (NDIM)
       COMMON / NNUSED / NMAX
       COMMON / HSUMS  / S(NDIM,6)
       COMMON / RZETA  / ZETA
*
       COMMON / PNS2   / P2NS (NDIM, NFMIN:NFMAX, 3)
*
* ..Output common-block 
*
       COMMON / PSG2   / P2SG (NDIM, NFMIN:NFMAX, 2, 2)
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
       S4 = S(KN,4)
*
       NI = 1./N
       NI2 = NI*NI
       NI3 = NI*NI2
       NM = N - 1.
       NMI = 1./NM
       NMI2 = NMI*NMI
*
       N1 = N + 1.
       N1I = 1./N1
       N1I2 = N1I*N1I
       N1I3 = N1I*N1I2
       N2 = N + 2.
       N2I = 1./N2
       N2I2 = N2I*N2I
       N2I3 = N2I*N2I2       
*
       S1M = S1 - NI
       S11 = S1 + N1I
       S2M = S2 - NI2
       S21 = S2 + N1I2
       S31 = S3 + N1I3
*
* ---------------------------------------------------------------------
*
* ..The moments of the functions employed in the parametrizations:
*
* ...1/(1-x)_+ [A0]  and  x^a ln^b (1-x) [B`b(a)' with M for a = -1]
*
       A0  = - S1M
       B1  = - S1 * NI
       B1M = - S1M * NMI
       B11 = - S11 * N1I
       B2  = (S1**2 + S2) * NI
       B2M = (S1M**2 + S2M) * NMI
       B21 = (S11**2 + S21) * N1I
       B3  = - (S1**3 + 3.*S1*S2 + 2.*S3) * NI
       B31 = - (S11**3 + 3.*S11*S21 + 2.*S31) * N1I
       B4  = (S1**4 + 6.*S1**2*S2 + 8.*S1*S3 + 3.*S2**2 + 6.*S4) * NI
*
* ...x^a [C`a']
*
       C0 = NI
       CM = NMI
       C1 = N1I
       C2 = N2I
       C3 = 1./(N+3.)
       C4 = 1./(N+4.)
*
* ...x^a ln^b x [D`b(a)']
*
       D1  = - NI2
       D1M = - NMI2
       D11 = - N1I2
       D2  = 2.* NI3
       D21 = 2.* N1I3
       D22 = 2.* N2I3
       D3  = - 6.* NI2*NI2
       D31 = - 6.* N1I2*N1I2
       D32 = - 6.* N2I2*N2I2
       D4  = 24.* NI2*NI3
       D41 = 24.* N1I2*N1I3
*
* ...x^a ln^b x ln(1-x) [E`b(a)']
*
       E1  = S1*NI2 + (S2-ZETA(2))*NI
       E11 = S11*N1I2 + (S21-ZETA(2))*N1I
       E2  = 2.* ( - S1*NI3 + (ZETA(2)-S2)*NI2 - (S3-ZETA(3))*NI )
*
* ---------------------------------------------------------------------
*
*  .. The parametrized n_f-components of DeltaP_ps^(2) and DeltaP_qg^(2)
*     [deltaP_qq^(2) is obtained by adding the non-singlet quantity 
*     DeltaP_ns^+(2) provided by the subroutine  P2NSP  to DeltaP_ps^(2)]

       PS1 = - 344./27.d0 * (D4-D41) - 90.9198 * (D3-D31) 
     ,       - 81.50 * (D31-D32) - 368.6 * (D2-D21) + 349.9 * (D21-D22)
     ,       - 739.0 * (D1-D11) - 6.541 * (B3-B31) - 12.61 * (B2-B21) 
     ,       - 204.76 * (B1-B11) - 1362.6 * (C0-C1) + 1617.4 * (C1-C2) 
     ,       - 674.8 * (C2-C3) + 167.41 * (C3-C4) + 232.57 * (E1-E11)
       PS2 =   1.1741 * (D3-D31) - 0.8253 * (D31-D32) 
     ,       + 13.287 * (D2-D21) + 10.657 * (D21-D22) + 45.482*(D1-D11) 
     ,       + 1.7805 * (B2-B21) + 9.517 * (B1-B11) + 49.13 * (C0-C1) 
     ,       - 30.77 * (C1-C2) - 4.307 * (C2-C3) - 0.5094 * (C3-C4)
*
       QG1 = - 151./3.d0 * D4 - 385.64 * D3 - 73.30 * D31 - 894.8 * D2 
     ,       + 1145.3 * D21 - 1461.2 * D1 + 3.784 * B4 - 5.300 * B3 
     ,       - 90.26 * B2 + 278.32 * B1 - 2972.4 * C0 + 4672.* C1 
     ,       - 1221.6 * C2 - 18.00 * C3 + 825.4 * E1 
       QG2 = 16./9.d0 * D4 + 30.739 * D3 + 10.186 * D31 + 196.96 * D2
     ,       + 179.1 * D21 + 526.3 * D1 + 0.7374 * B3 + 7.320 * B2 
     ,       - 6.256 * B1 + 499.65 * C0 - 432.18 * C1 - 141.63 * C2 
     ,       - 11.34 * C3 - 47.30 * E1
*
* ...and of DeltaP^(2)_gq and DeltaP^(2)_gg  [GQ2 is exact]
*
       GQ0 =   11512./81.d0 * D4 + 888.003 * D3 + 175.1 * D31
     ,       + 2140. * D2 - 850.7 * D21 + 4046.6 * D1 + 5.143 * B4 
     ,       + 59.30 * B3 + 451.55 * B2 + 1843.7 * B1 + 6159. * C0 
     ,       - 3825.9 * C1 + 1942. * C2 - 742.1 * C3 - 1424.8 * E1
       GQ1 = - 128./27.d0 * D4 - 39.3872 * D3 - 30.023 * D31
     ,       - 202.46 * D2 -126.53 * D21 - 308.98 * D1 - 4.963 * B3 
     ,       - 47.86 * B2 - 171.78 * B1 - 301.07* C0 - 296.0 * C1 
     ,       + 406.13 * C2 - 101.62 * C3 - 16.18 * E1
       GQ2 = (  6.d0 * B2 - 3.d0* B21 + 8.d0 * B1 + 2.d0* B11 
     ,       - 12.d0 * C0 + 10.d0 * C1 ) * 16.d0/27.d0
*
       GG0 =   504.d0 * D4 + 3777.5 * D3 + 1167. * D31
     ,       + 10902. * D2 - 863.0 * D21 + 23091. * D1 + 30988. * C0 
     ,       - 39925. * C1 + 13447. * C2 - 4576. * C3
     ,       - 12292. * E1 - 13247. * (B1-B11) + 3801. *B1
     ,       + 4427.762 + 2643.521 * A0 
       GG1 = - 766./27.d0 * D4 - 357.798 * D3 + 131.0 * D31
     ,       - 1877.2 * D2 + 613.1 * D21 - 3524. * D1 - 1173.5 * C0
     ,       + 2648.6 * C1 - 2160.8 * C2 + 1251.7 * C3 - 7932. * E1
     ,       - 6746. * (B1-B11) - 295.7 * B1 - 528.536 - 412.172 * A0
       GG2 = - 1.1809 * D3 - 6.679 * D2 + 15.764 * D21 - 13.29 * D1
     ,       - 16.606 * C0 + 32.905 * C1 - 18.30 * C2 + 2.637 * C3
     ,       - 16.944 * E1 - 0.210 * B1 + 6.4607 - 16./9.d0 * A0
*
* ---------------------------------------------------------------------
*
* ..Flavour-number loop and output to the array 
*
       DO 2 NF = NFMIN, NFMAX
*
         P2SG(KN,NF,1,1) = P2NS(KN,NF,1) + NF *( PS1 + NF * PS2 )
         P2SG(KN,NF,1,2) =       NF * ( QG1 + NF * QG2 )
         P2SG(KN,NF,2,1) = GQ0 + NF * ( GQ1 + NF * GQ2 )
         P2SG(KN,NF,2,2) = GG0 + NF * ( GG1 + NF * GG2 )
*
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
