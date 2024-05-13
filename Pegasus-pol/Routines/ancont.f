C************************************************************************      
      SUBROUTINE ACINI
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  INITIALIZE CONSTANTS
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
      COMMON /KM/KEY,MAX
C
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/ACCON5/ XLI4
      COMMON/IAPP  / IAPP
      COMMON/VAL   / ZERRO,ONNE
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACLOG2/ AK2(10)
      COMMON/ACLOG3/ AK3(11)
      COMMON/ACLOG4/ BK1(9)
      COMMON/ACLOG5/ BK2(10)
      COMMON/ACLOG6/ CK1(12)
      COMMON/ACLOG7/ CK2(13)
      COMMON/ACLOG8/ CK3(10)
      COMMON/ACLOG9/ CK4(13)
      COMMON/ACLG10/ CK5(9)
      COMMON/ACLG11/ DK5(10)
      COMMON/POLY1 / P21(4)
      COMMON/POLY2 / P22(4)
      COMMON/POLY3 / P23(5),P33(5)
      COMMON/POLY4 / P24(5),P34(5)
C
      DIMENSION P11(4),P12(4),P13(5),P14(5)
C
      DATA AK1/0.999999974532240D+0,
     &        -0.499995525890027D+0,
     &         0.333203435554182D+0,
     &        -0.248529457735332D+0,
     &         0.191451164493502D+0,
     &        -0.137466222203386D+0,
     &         0.792107405737825D-1,
     &        -0.301109652783781D-1,
     &         0.538406198111749D-2/
C
      DATA AK2/0.999999980543793D+0,
     &        -0.999995797779624D+0,
     &         0.916516447393493D+0,
     &        -0.831229921350708D+0,
     &         0.745873737923571D+0,
     &        -0.634523908078600D+0,
     &         0.467104011423750D+0,
     &        -0.261348046799178D+0,
     &         0.936814286867420D-1,
     &        -0.156249375012462D-1/
C
      DATA AK3/9.99999989322696D-1,
     &        -1.49999722020708D+0,
     &         1.74988008499745D+0,
     &        -1.87296689068405D+0,
     &         1.91539974617231D+0,
     &        -1.85963744001295D+0,
     &         1.62987195424434D+0,
     &        -1.17982353224299D+0,
     &         6.28710122994999D-1,
     &        -2.11307487211713D-1,
     &         3.28953352932140D-2/
C
      DATA BK1/0.693147166991375D+0,
     &        -0.306850436868254D+0,
     &         0.193078041088284D+0,
     &        -0.139403892894644D+0,
     &         0.105269615988049D+0,
     &        -0.746801353858524D-1,
     &         0.427339135378207D-1,
     &        -0.161809049989783D-1,
     &         0.288664611077007D-2/
C
      DATA BK2/0.480453024731510D+0,
     &         0.480450679641120D+0,
     &        -0.519463586324817D+0,
     &         0.479285947990175D+0,
     &        -0.427765744446172D+0,
     &         0.360855321373065D+0,
     &        -0.263827078164263D+0,
     &         0.146927719341510D+0,
     &        -0.525105367350968D-1,
     &         0.874144396622167D-2/
C
      DATA CK1/-0.283822933724932D+0,
     &          0.999994319023731D+0,
     &         -0.124975762907682D+1,
     &          0.607076808008983D+0,
     &         -0.280403220046588D-1,
     &         -0.181869786537805D+0,
     &          0.532318519269331D+0,
     &         -0.107281686995035D+1,
     &          0.138194913357518D+1,
     &         -0.111100841298484D+1,
     &          0.506649587198046D+0,
     &         -0.100672390783659D+0/
C
      DATA CK2/ 0.480322239287449D+0,
     &         -0.168480825099580D+1,
     &          0.209270571620726D+1,
     &         -0.101728150275998D+1,
     &          0.160179976133047D+0,
     &         -0.351982236677917D+0,
     &          0.141033316846244D+1,
     &         -0.353343997843391D+1,
     &          0.593934696819832D+1,
     &         -0.660019784804042D+1,
     &          0.466330349413063D+1,
     &         -0.189825467489058D+1,
     &          0.339772909487512D+0/
C
      DATA CK3/-0.243948949064443D-1,
     &          0.000005136294145D+0,
     &          0.249849075518710D+0,
     &         -0.498290708990997D+0,
     &          0.354866791547134D+0,
     &         -0.522116678353452D-1,
     &         -0.648354706049337D-1,
     &          0.644165053822532D-1,
     &         -0.394927322542075D-1,
     &          0.100879370657869D-1/
C
      DATA CK4/ 0.192962504274437D+0,
     &          0.000005641557253D+0,
     &         -0.196891075399448D+1,
     &          0.392919138747074D+1,
     &         -0.290306105685546D+1,
     &          0.992890266001707D+0,
     &         -0.130026190226546D+1,
     &          0.341870577921103D+1,
     &         -0.576763902370864D+1,
     &          0.645554138192407D+1,
     &         -0.459405622046138D+1,
     &          0.188510809558304D+1,
     &         -0.340476080290674D+0/
C
      DATA CK5/-0.822467033400775D+0,
     &          0.887664705657325D-1,
     &         -0.241549406045162D-1,
     &          0.965074750946139D-2,
     &         -0.470587487919749D-2,
     &          0.246014308378549D-2,
     &         -0.116431121874067D-2,
     &          0.395705193848026D-3,
     &         -0.664699010014505D-4/
C
      DATA DK5/-0.822467033400775D+0,
     &          0.999999974532240D+0,
     &         -0.249997762945014D+0,
     &          0.111067811851394D+0,
     &         -0.621323644338330D-1,
     &          0.382902328987004D-1,
     &         -0.229110370338977D-1,
     &          0.113158200819689D-1,
     &         -0.376387065979726D-2,
     &          0.598229109013054D-3/
C
      PII = 3.141592653589793238462643D0
      ZETA2 = PII**2/6.0D0       
      ZETA3 = 1.20205690315959428540D0
      ZET2=ZETA2
      ZET3=ZETA3
      ZLI4 = FLI4(0.5D0)
      XLI4 = FLI4(0.5D0)
      D2 = DLOG(2.0D0)
      DL = DLOG(2.0D0)
      GE   = 0.57721566490153D+0
C
      ZERRO=0.0D0
      ONNE =1.0D0
C
C----------------------------------------------------------------------
      P11(1)=-49.0D0/36.0+ZETA2
      P11(2)=11.0D0/6.0D0
      P11(3)=-7.0D0/12.0D0
      P11(4)=1.0D0/9.0D0
C-------------------------------                       
      P21(1)=11.0D0/6.0
      P21(2)=-3.0D0
      P21(3)=3.0D0/2.0D0
      P21(4)=-1.0D0/3.0D0
C-------------------------------                       
      P12(1)=ZETA3-11.0D0/6.0*ZETA2+4.0D0/3.0D0
      P12(2)=3.0D0*ZETA2-13.0D0/4.0D0
      P12(3)=-3.0D0/2.0D0*ZETA2+5.0D0/2.0D0
      P12(4)=1.0D0/3.0D0*ZETA2-7.0D0/12.0D0
C-------------------------------                       
      P22(1)=-1.0D0
      P22(2)=5.0D0/2.0D0
      P22(3)=-2.0D0
      P22(4)=1.0D0/2.0D0
C-------------------------------                       
      P13(1)=ZETA3-2035.0D0/1728.0D0
      P13(2)=205.0D0/144.0D0
      P13(3)=-95.0D0/288.0D0
      P13(4)=43.0D0/432.0D0
      P13(5)=-1.0D0/64.0D0
C-------------------------------                       
      P23(1)=205.0D0/144.0D0
      P23(2)=-25.0D0/12.0D0
      P23(3)=23.0D0/24.0D0
      P23(4)=-13.0D0/36.0D0
      P23(5)=1.0D0/16.0D0
C-------------------------------                       
      P33(1)=-25.0D0/24.0D0
      P33(2)=2.0D0
      P33(3)=-3.0D0/2.0D0
      P33(4)=2.0D0/3.0D0
      P33(5)=-1.0D0/8.0D0
C-------------------------------                       
      P14(1)=257.D0/144.0D0-205.0D0/72.0D0*ZET2+ZET2**2
      P14(2)=-167.0D0/36.0D0+25.0D0/6.0D0*ZET2
      P14(3)=101.0D0/24.0D0-23.0D0/12.0D0*ZET2
      P14(4)=-59.0D0/36.0D0+13.0D0/18.0D0*ZET2
      P14(5)=41.0D0/144.0D0-ZET2/8.0D0
C-------------------------------                       
      P24(1)=-167.0D0/36.0D0+25.0D0/6.0D0*ZET2
      P24(2)=235.0D0/18.0D0-8.0D0*ZET2
      P24(3)=-40.0D0/3.0D0+6.0D0*ZET2
      P24(4)=109.0D0/18.0D0-8.0D0/3.0D0*ZET2
      P24(5)=-41.0D0/36.0D0+ZET2/2.0D0
C-------------------------------                       
      P34(1)=35.0D0/12.0D0
      P34(2)=-26.0D0/3.0D0
      P34(3)=19.0D0/2.0D0
      P34(4)=-14.0D0/3.0D0
      P34(5)=11.0D0/12.0D0
C----------------------------------------------------------------------
C
C >>> ACCOUNT FOR POLYNOM PARTS
C
      CK1(1)=CK1(1)+P11(1)
      CK1(2)=CK1(2)+P11(2)
      CK1(3)=CK1(3)+P11(3)
      CK1(4)=CK1(4)+P11(4)
C
      CK2(1)=CK2(1)+P12(1)
      CK2(2)=CK2(2)+P12(2)
      CK2(3)=CK2(3)+P12(3)
      CK2(4)=CK2(4)+P12(4)
C
      CK3(1)=CK3(1)+P13(1)
      CK3(2)=CK3(2)+P13(2)
      CK3(3)=CK3(3)+P13(3)
      CK3(4)=CK3(4)+P13(4)
      CK3(5)=CK3(5)+P13(5)
C
      CK4(1)=CK4(1)+P14(1)
      CK4(2)=CK4(2)+P14(2)
      CK4(3)=CK4(3)+P14(3)
      CK4(4)=CK4(4)+P14(4)
      CK4(5)=CK4(5)+P14(5)
C
C----------------------------------------------------------------------
C
      KEY = 2
      MAX = 10000
C
      IAPP=1
C

      RETURN
      END

      COMPLEX*16 FUNCTION ACG1(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1+X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK2,DL,ZERRO,ONNE
C
      COMMON/ACLOG2/ AK2(10)
      COMMON/ACCON1/ DL
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
      DO 1 L=2,11
      K=L-1
      T=T+AK2(K)/(ZN+DBLE(K+1))
1     CONTINUE
C
      ACG1=(DL*DL- ZN*T)/2.0D0
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG2(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1+X)**2/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK3,DL,ZERRO,ONNE
C
      COMMON/ACLOG3/ AK3(11)
      COMMON/ACCON1/ DL
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
      DO 1 L=3,13
      K=L-2
      T=T+AK3(K)/(ZN+DBLE(K+2))
1     CONTINUE
C
      ACG2=(DL**3- ZN*T)/3.0D0
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG3(ZN1)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LI2(X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,DL,ZERRO,ONNE,ZET2,GE
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/IAPP  / IAPP
      COMMON/VAL   / ZERRO,ONNE
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON4/ GE
C
      ZN=ZN1+ONNE
C
      IF(IAPP.EQ.1) GOTO 10
      IF(IAPP.EQ.2) GOTO 20
      IF(IAPP.EQ.3) GOTO 30
      WRITE(6,*) '*** ERROR IN  ACG3, IAPP=',IAPP,' WRONG, STOP ***'
      STOP
C
10    T=DCMPLX(DL*ZET2,ZERRO)
      Z=ZN1
      Z1=Z+ONNE
      DO 1 L=1,9
      ZL =DCMPLX(DBLE(L),ZERRO)
      ZL1=Z+ZL+ONNE
      CALL PSI0(ZL1,PS)
      S1=PS+GE
      T=T-AK1(L)*(ZET2*Z/(Z+ZL)+ZL/(Z+ZL)**2*S1)
1     CONTINUE
      GOTO 100
C
20    T=1.01/(ZN+ONNE)-0.846/(ZN+ONNE*2)+1.155/(ZN+ONNE*3)
     &   -1.074/(ZN+ONNE*4)+0.55/(ZN+ONNE*5)
      GOTO 100
30    T=1.004/(ZN+ONNE)-0.846/(ZN+ONNE*2)+1.342/(ZN+ONNE*3)
     &   -1.532/(ZN+ONNE*4)+0.839/(ZN+ONNE*5)
100   CONTINUE
      ACG3=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG4(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LI2(-X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,DL,ZERRO,ONNE,ZET2
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(-ZET2/2.0D0*DL,ZERRO)
C
      DO 1 L=1,9
      ZL =DCMPLX(DBLE(L),ZERRO)
      ZNL =ZN+ZL
      ZNL1=ZNL+DCMPLX(ONNE,ZERRO)
      CALL BET(ZNL1,V1)
C
      T=T+AK1(L)*(ZN/ZNL*ZET2/2.0D0+ZL/ZNL**2*(DL-V1))
1     CONTINUE
C
      ACG4=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG5(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(X) LI2(X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,ZERRO,ONNE,DL,ZET2,GE
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/VAL   / ZERRO,ONNE
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON4/ GE
C
      T=DCMPLX(ZERRO,ZERRO)
      Z=ZN
      DO 1 L=1,9
      ZL =DCMPLX(DBLE(L),ZERRO)
      ZNL=Z+ZL
      ZL1=ZNL+ONNE
      CALL PSI0(ZL1,PS)
      CALL PSI1(ZL1,S1P)
      S1=PS+GE
      T=T-AK1(L)*ZL/ZNL**2*(ZET2+S1P-2.0D0*S1/ZNL)
1     CONTINUE
C
      ACG5=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG6(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LI3(X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,DL,ZERRO,ONNE,ZET3,GE,ZET2
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(DL*ZET3,ZERRO)
      DO 1 L=1,9
      ZL=DCMPLX(DBLE(L),ZERRO)
      ZNL=ZN+ZL
      ZNL1=ZNL+ONNE
      CALL PSI0(ZNL1,V1)
      S1=V1+GE
C
      T=T-AK1(L)*(ZN/ZNL*ZET3+ZL/ZNL**2*(ZET2-S1/ZNL))
1     CONTINUE
C
      ACG6=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG7(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LI3(-X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,DL,ZERRO,ONNE,ZET2,ZET3
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(-3.0D0*ZET3/4.0D0*DL,ZERRO)
C
      DO 1 L=1,9
      ZL =DCMPLX(DBLE(L),ZERRO)
      ZNL =ZN+ZL
      ZNL1=ZNL+DCMPLX(ONNE,ZERRO)
      CALL BET(ZNL1,V1)
C
      T=T+AK1(L)*(ZN/ZNL*3.0D0*ZET3/4.0D0+ZL/ZNL**2/2.0D0*ZET2
     & -ZL/ZNL**3*(DL-V1))
1     CONTINUE
C
      ACG7=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG8(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: S12(X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,DL,ZERRO,ONNE,ZET2,GE,ZET3
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
      DO 1 L=1,9
      ZL=DCMPLX(DBLE(L),ZERRO)
      ZNL=ZN+ZL
      ZNL1=ZNL+ONNE
      CALL PSI0(ZNL1,PS0)
      CALL PSI1(ZNL1,PS1)
      S1=PS0+GE
      S2=ZET2-PS1
      T=T-AK1(L)*(ZN*ZET3/ZNL+ZL/ZNL**2/2.0D0*(S1**2+S2))
1     CONTINUE
C
      ACG8=T+DL*ZET3
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG9(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: S12(-X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,AK2,AK3,DL,ZERRO,ONNE,ZET3
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACLOG2/ AK2(10)
      COMMON/ACLOG3/ AK3(11)
      COMMON/ACCON1/ DL
      COMMON/ACCON3/ ZET3
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZET3*DL/8.0D0,ZERRO)
      DO 1 K=1,9
      T1=DCMPLX(ZERRO,ZERRO)
      DO 2 L=2,11
      L1=L-1
      ZNKL=ZN+DCMPLX(DBLE(K+L),ZERRO)
      T1=T1+AK2(L1)/ZNKL
2     CONTINUE
      ZK=DCMPLX(DBLE(K),ZERRO)
      ZNK=ZN+ZK
      T=T-AK1(K)*ZN/ZNK*(ZET3/8.0D0-T1/2.0D0)
1     CONTINUE
      T2=DCMPLX(ZERRO,ZERRO)
      DO 3 K=3,13
      L=K-2
      ZK=DCMPLX(DBLE(K),ZERRO)
      ZNK=ZN+ZK
      T2=T2+AK3(L)/ZNK
3     CONTINUE
      T=T-T2/2.0D0
C
      ACG9=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG10(ZN)
C     -----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: I1(X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,AK2,DL,ZERRO,ONNE,ZET3,GE
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACLOG2/ AK2(10)
      COMMON/ACCON1/ DL
      COMMON/ACCON4/ GE
      COMMON/ACCON3/ ZET3
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(-5.0D0/8.0D0*ZET3*DL,ZERRO)
C
      DO 1 K=2,11
      L=K-1
      ZNK=ZN+DBLE(K)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      S1=PS+GE
      T=T+AK2(L)*S1/ZNK
1     CONTINUE
      DO 2 K=1,9
      ZNK=ZN+DBLE(K)
      T2=DCMPLX(ZERRO,ZERRO)
      DO 3 L=1,9
      ZNKL=ZNK+DBLE(L)
      ZNKL1=ZNKL+ONNE
      CALL PSI0(ZNKL1,PS1)
      S2=PS1+GE
      T2=T2-AK1(L)*S2/ZNKL
3     CONTINUE
      T=T+AK1(K)*ZN/ZNK*(5.0D0/8.0D0*ZET3+T2)
2     CONTINUE
C
      ACG10=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG11(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1-X)/(1+X) LI2(X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,CK1,P21,DL,ZERRO,ONNE,ZET2,GE,ZET3
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACLOG6/ CK1(12)
      COMMON/POLY1 / P21(4)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
      T1=DCMPLX((-ZET2+DL**2)/2.0D0,ZERRO)
      T2=DCMPLX(7.0D0/4.0D0*ZET3-ZET2*DL+DL**3/3.0D0,ZERRO)
C
      DO 1 K=1,12
      L=K-1
      U2=T1
      ZK=DCMPLX(DBLE(L))
C
      DO 2 M=1,9
      ZM=DCMPLX(DBLE(M))
      ZM1=ZM+ONNE
      ZNM=ZN+ZM+ZK
      ZNM1=ZNM+ONNE
      CALL PSI0(ZNM1,PS)
      CALL PSI0(ZM1,PS1)
      S1=PS+GE
      S11=PS1+GE
      U2=U2-AK1(M)*(ZM/ZNM*S1-S11)
2     CONTINUE
C
      T=T+CK1(K)*U2
1     CONTINUE
C
      DO 3 K=1,4       
      L=K-1
C
      U3=T2
      ZK=DCMPLX(DBLE(L),ZERRO)
      DO 4 M=1,9
      ZM=DCMPLX(DBLE(M),ZERRO)
      ZM1=ZM+ONNE
      ZMN=ZN+ZK+ZM
      ZMN1=ZMN+ONNE
      CALL PSI0(ZMN1,PS5)
      CALL PSI1(ZMN1,PS2)
      CALL PSI0(ZM1,PS3)
      CALL PSI1(ZM1,PS4)
      S1=PS5+GE
      S2=-PS2+ZET2
      S11=PS3+GE
      S21=-PS4+ZET2
      U3=U3+AK1(M)*(ZM/ZMN*(S1**2+S2)-(S11**2+S21))
4     CONTINUE
      T=T+P21(K)*U3
3     CONTINUE
C
      ACG11=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG12(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1-X)/(1+X)*LI2(-X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C

      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,AK2,DL,ZERRO,ONNE,GE
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACLOG2/ AK2(10)
      COMMON/ACCON1/ DL
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
      DO 1 K=1,9
      ZK=DCMPLX(DBLE(K),ZERRO)
      T2=DCMPLX(ZERRO,ZERRO)
      ZNK=ZN+ZK
      DO 2 L=2,11
      L1=L-1
      ZNKL=ZNK+DBLE(L)
      T2=T2+AK2(L1)*ZNK/ZNKL
2     CONTINUE
      ZNK1=ZNK+ONNE
      CALL BET(ZNK1,U1)
      CALL PSI0(ZNK1,U2)
      V1=U1*(U2+GE-DL)
      CALL BET1(ZNK1,V2)
C
      RES=-ONNE/2.0D0*(DL**2-T2)+V2-V1
      T=T-AK1(K)/ZK*RES
1     CONTINUE
C
      ACG12=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG13(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1+X)/(1+X)*LI2(-X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK2,AK3,DL,ZERRO,ONNE,ZET2
C
      COMMON/ACLOG2/ AK2(10)
      COMMON/ACLOG3/ AK3(11)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/VAL   / ZERRO,ONNE
C
      T0=DCMPLX(-1.0D0/4.0D0*ZET2*DL**2,ZERRO)
C
      T1=DCMPLX(ZERRO,ZERRO)
      DO 1 L=3,13
      K=L-2
      T1=T1+AK3(K)/(ZN+DBLE(L))
1     CONTINUE
C
      T2=DCMPLX(ZERRO,ZERRO)
      DO 2 L=2,11
      K=L-1
      ZNK1=ZN+DBLE(L+1)
      CALL BET(ZNK1,V1)
      T2=T2+AK2(K)*ZN/(ZN+DBLE(L))*(ZET2/2.0D0-(DL-V1)/(ZN+DBLE(L)))
2     CONTINUE
C
      ACG13=T0+(T1+T2)/2.0D0
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG14(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LOG^2(1+X)-LOG^2(2))/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 BK2,ZERRO,ONNE
C
      COMMON/ACLOG5/ BK2(10)
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
      DO 1 L=1,10
      ZNL=ZN+DBLE(L)
      T=T+BK2(L)/ZNL
1     CONTINUE
C
      ACG14=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG15(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LOG(1+x)-LOG(2))/(X-1)*LI2(X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 BK1,ZERRO,ONNE,ZET2,GE
C
      COMMON/ACLOG4/ BK1(9)
      COMMON/ACCON2/ ZET2
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=ZERRO
      DO 1 L=1,9
      ZNK=ZN+DCMPLX(DBLE(L),ZERRO)
      ZNK2=ZNK+ONNE
      CALL PSI0(ZNK2,PS)
      S1=PS+GE
      T=T+BK1(L)/ZNK*(ZET2-S1/ZNK)
1     CONTINUE
C
      ACG15= T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG16(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LOG(1+x)-LOG(2))/(X-1)*LI2(-X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 BK1,ZERRO,ONNE,ZET2,DL
C
      COMMON/ACLOG4/ BK1(9)
      COMMON/ACCON2/ ZET2
      COMMON/ACCON1/ DL
      COMMON/VAL   / ZERRO,ONNE
C
      T=ZERRO
      DO 1 L=1,9
      ZNK=ZN+DCMPLX(DBLE(L),ZERRO)
      ZNK2=ZNK+ONNE
      CALL BET(ZNK2,V1)
      T1=DL-V1
      T=T+BK1(L)/ZNK*(-ZET2/2.0D0+T1/ZNK)
1     CONTINUE
C
      ACG16= T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG17(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(X) LOG^2(1+X)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK2,ZERRO,ONNE,ZET2
C
      COMMON/ACLOG2/ AK2(10)
      COMMON/VAL   / ZERRO,ONNE
      COMMON/ACCON2/ ZET2
C
      T=DCMPLX(ZERRO,ZERRO)
      DO 1 K=2,11
      L=K-1
      ZNK1=ZN+DBLE(K+1)
      CALL PSI1(ZNK1,V1)
      T=T+AK2(L)*V1
1     CONTINUE
C
      ACG17=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG18(ZN)
C     -----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LI2(X)-ZETA2)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 CK1,P21,ZERRO,ONNE,ZET2,ZET3,GE
C
      COMMON/ACLOG6/ CK1(12)
      COMMON/POLY1 / P21(4)
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      ZN1=ZN+ONNE
      T=DCMPLX(ZERRO,ZERRO)
C
      CALL PSI0(ZN1,PS1)
      CALL PSI1(ZN1,PS2)
      SPS1=PS1+GE
      SPS2=-PS2+ZET2
      T=T+(SPS1**2+SPS2)/ZN-ZET2*SPS1
C
      DO 1 L=1,12
      ZNK1=ZN+DBLE(L-1)
      ZNK2=ZNK1+ONNE
      CALL PSI0(ZNK2,PS)
      S1=PS+GE
      T=T+CK1(L)*S1/ZNK1*ZN
1     CONTINUE
C
      DO 2 L=1,4
      ZNK1=ZN+DBLE(L-1)
      ZNK2=ZNK1+ONNE
      CALL PSI0(ZNK2,PS)
      CALL PSI1(ZNK2,PS1)
      S1=PS+GE
      S2=-PS1+ZET2
      T=T-P21(L)*(S1**2+S2)/ZNK1*ZN
2     CONTINUE
C
      ACG18=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG19(ZN)
C     -----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LI2(-X)+ZETA2/2)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,ZERRO,ONNE,ZET2,GE
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACCON2/ ZET2
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
      ZN1=ZN+ONNE
      CALL PSI0(ZN1,PS)
      S1=PS+GE
      T=T+S1*ZET2/2.0D0
      DO 1 L=1,9
      ZK=DCMPLX(DBLE(L))
      ZNK1=ZN+ZK+ONNE
      CALL PSI0(ZNK1,PS)
      S1=PS+GE
      T=T-AK1(L)*S1/ZK
1     CONTINUE
C
      ACG19=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG20(ZZ)
C     -----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LI3(X)-ZETA3)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 DL,ZERRO,ONNE,ZET2,ZET3,GE,P24,P34,P22,CK2,CK4
C
      COMMON/ACLOG7/ CK2(13)
      COMMON/ACLOG9/ CK4(13)
      COMMON/POLY2 / P22(4)
      COMMON/POLY4 / P24(5),P34(5)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZET2**2/2.0D0,ZERRO)
C
      ZN = ZZ
      ZN1=ZN+ONNE
      CALL PSI0(ZN1,PS)
      S1=PS+GE
      T=T-ZET3*S1
C
      DO 1 K=1,13
      L=K-1
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      S1=PS+GE
      R=ZN/ZNK
C
      T=T+CK2(K)*R*S1
1     CONTINUE
C
      DO 3 K=1,4
      L=K-1
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      CALL PSI1(ZNK1,PS1)
      S1=PS+GE
      S2=-PS1+ZET2
      R=ZN/ZNK
      T1=S1**2+S2
C
      T=T-P22(K)*R*T1
3     CONTINUE
C
      DO 4 K=1,13
      L=K-1
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      S1=PS+GE
C
      T=T-CK4(K)/ZNK*ZN/2.0D0
4     CONTINUE
C
      DO 5 K=1,5
      L=K-1
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      CALL PSI1(ZNK1,PS1)
      S1=PS+GE
      S2=-PS1+ZET2
      T1=-S1/ZNK
      T2=(S1**2+S2)/ZNK
C
      T=T-(P24(K)*T1+P34(K)*T2)*ZN/2.0D0
5     CONTINUE
C
C
      ACG20=T

C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG21(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (S12(X)-ZETA3)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 ZERRO,ONNE,ZET2,ZET3,GE,CK3,P23,P33
C
      COMMON/ACLOG8/ CK3(10)
      COMMON/POLY3 / P23(5),P33(5)
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
      ZN1=ZN+ONNE
      CALL PSI0(ZN1,PS)
      CALL PSI1(ZN1,PS1)
      CALL PSI2(ZN1,PS2)
      S1=PS+GE
      S2=-PS1+ZET2
      S3= PS2/2.0D0+ZET3
C
      T=T-ZET3*S1
      T=T+(S1**3+3.0D0*S1*S2+2.0D0*S3)/2.0D0/ZN
C
      DO 1 K=1,10
      L=K-1
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      S1=PS+GE
      T=T+CK3(K)*S1*ZN/ZNK
1     CONTINUE
C
      DO 2 K=1,5
      L=K-1
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      CALL PSI1(ZNK1,PS1)
      CALL PSI2(ZNK1,PS2)
      S1=PS+GE
      S2=-PS1+ZET2
      S3= PS2/2.0D0+ZET3
      T2=S1**2+S2
      T3=S1**3+3.0D0*S1*S2+2.0D0*S3
      T=T+ZN/ZNK*(P33(K)*T3-P23(K)*T2)
2     CONTINUE
C
      ACG21=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG22(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(X) LI2(X)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 CK1,P21,GE,ZERRO,ONNE
C
      COMMON/ACLOG6/ CK1(12)
      COMMON/POLY1 / P21(4)
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(ZERRO,ZERRO)
C
      DO 1 L=1,12
      ZNK1=ZN+DBLE(L)
      CALL PSI1(ZNK1,PS1)
      T=T+CK1(L)*PS1
1     CONTINUE
C
      DO 2 L=1,4
      ZNK1=ZN+DBLE(L)
      CALL PSI0(ZNK1,PS)
      CALL PSI1(ZNK1,PS1)
      CALL PSI2(ZNK1,PS2)
      TA=(PS+GE)*PS1-PS2/2.0D0
      T=T-P21(L)*TA
2     CONTINUE
C
      ACG22=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG23(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LI3(-X)+3/4*ZETA3)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,GE,ZERRO,ONNE,ZET2,ZET3,XLI4,DL
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/ACCON5/ XLI4
      COMMON/VAL   / ZERRO,ONNE
C
C >>> M[Li3(x)/(1+x)](N)
C
      U=ACG6(ZN)
C
      T=DCMPLX(ZERRO,ZERRO)
      T=T-ZET2**2/2.0D0+ZET3*DL
      ZN1=ZN+ONNE
      CALL PSI0(ZN1,PS)
      S1=PS+GE
      T=T+3.0D0/4.0D0*ZET3*S1-U
      DO 1 L=1,9
      ZK=DCMPLX(DBLE(L),ZERRO)
      ZNK=ZN+ZK
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      S1=PS+GE
      R=ZN/ZNK
      H=ZET3-ZET2/ZNK-ZET2/ZK+
     &   S1*(1.0D0/ZNK**2+1.0D0/ZK**2+1.0D0/ZNK/ZK)
      T=T-AK1(L)*R*H
1     CONTINUE
C
      ACG23=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG24(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (I1(X)+5/8*ZETA3)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,CK5,DK5,DL,ZERRO,ONNE,ZET3,GE,ZET2
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACLG10/ CK5(9)
      COMMON/ACLG11/ DK5(10)
      COMMON/ACCON1/ DL
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(-2.0D0*ZET3*DL,ZERRO)
C
C >>> M[S12(x)/(1+x)](N)
C
      U=ACG8(ZN)
C
      T=T+2.0D0*U
      ZN1=ZN+ONNE
      CALL PSI0(ZN1,PS)
      S1=PS+GE
      T=T+5.0D0/8.0D0*ZET3*S1
C
      DO 1 L=1,9
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      CALL PSI1(ZNK1,PS1)
      S1=PS+GE
      S2=-PS1+ZET2
      T1=ZN/ZNK*(2.0D0*ZET3-(S1**2+S2)/ZNK)
      T=T+AK1(L)*T1
1     CONTINUE
C
      DO 2 L=1,9
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      S1=PS+GE
      T=T+CK5(L)*S1/ZNK*ZN
2     CONTINUE
C
      DO 3 K=1,10
      L=K-1
      ZNK=ZN+DBLE(L)
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      CALL PSI1(ZNK1,PS1)
      S1=PS+GE
      S2=-PS1+ZET2
      T1=ZN/ZNK*(S1**2+S2)
      T=T-DK5(K)*T1
3     CONTINUE
C
      ACG24=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG25(ZN)
C     ----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (S12(-X) -ZETA3/8)/(X-1)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
C
      REAL*8 AK1,AK2,GE,DL,ZET3,ZERRO,ONNE
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACLOG2/ AK2(10)
      COMMON/ACCON1/ DL
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/VAL   / ZERRO,ONNE
C
C >>> M[I1(x)/(1+x)](N)
C
      U=ACG10(ZN)
C
      T=DCMPLX(5.0D0/16.0D0*ZET3*DL,ZERRO)+U/2.0D0
      ZN1=ZN+ONNE
      CALL PSI0(ZN1,PS)
      S1=PS+GE
      T=T-ZET3/8.0D0*S1
C
      DO 1 L=2,11
      K=L-1
      ZK=DCMPLX(DBLE(L),ZERRO)
      ZNK=ZN+ZK
      ZNK1=ZNK+ONNE
      CALL PSI0(ZNK1,PS)
      S1=PS+GE
      T=T+AK2(K)*S1/ZK/ZNK*ZN/2.0D0
1     CONTINUE
C
      CO=-5.0D0/8.0D0*ZET3
C
      DO 2 K=1,9
      ZK=DCMPLX(DBLE(K),ZERRO)
      ZNK=ZN+ZK
      T1=CO
      DO 3 L=1,9
      ZNKL=ZN+DBLE(K+L)
      ZNKL1=ZNKL+ONNE
      CALL PSI0(ZNKL1,PS)
      S1=PS+GE
      T1=T1+AK1(L)*S1/ZNKL
3     CONTINUE
      T=T+AK1(K)/ZNK*ZN/2.0D0*T1
2     CONTINUE
C
      ACG25=T
C
      RETURN
      END
      COMPLEX*16 FUNCTION ACG26(ZN)
C     -----------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG^3(1-X)/(1+X)
C---  FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 AK1,ZERRO,ONNE,XLI4,ZET2,ZET3,GE
C
      COMMON/ACLOG1/ AK1(9)
      COMMON/ACCON2/ ZET2
      COMMON/ACCON3/ ZET3
      COMMON/ACCON4/ GE
      COMMON/ACCON5/ XLI4
      COMMON/VAL   / ZERRO,ONNE
C
      T=DCMPLX(-6.0D0*XLI4,ZERRO)
      DO 1 K=1,9
      ZK=DCMPLX(DBLE(K),ZERRO)
      ZK1=ZK+ONNE
      ZKN=ZK+ZN
      ZKN1=ZKN+ONNE
      CALL PSI0(ZKN1,PS)
      CALL PSI1(ZKN1,PS1)
      CALL PSI2(ZKN1,PS2)
      S1=PS+GE
      S2=-PS1+ZET2
      S3= PS2/2.0D0+ZET3
      CALL PSI0(ZK1,PS)
      CALL PSI1(ZK1,PS1)
      CALL PSI2(ZK1,PS2)
      H1=PS+GE
      H2=-PS1+ZET2
      H3= PS2/2.0D0+ZET3
      T=T-AK1(K)*(ZK/ZKN*(S1**3+3.0D0*S1*S2+2.0D0*S3)
     &                  -(H1**3+3.0D0*H1*H2+2.0D0*H3))
1     CONTINUE
C
      ACG26=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION FLI2(X)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  LI2(X)
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      EXTERNAL FR1
C
      PII = 3.141592653589793238462643D0
      ZETA2 = PII**2/6.0D0       
C
      IF(X.LT.-1.0D0.OR.X.GT.1.0D0) GOTO 100
      IF(X.EQ.1.0D0)  GOTO 5
      IF(X.EQ.-1.0D0) GOTO 6
      IF(X.LT.0.0D0)  GOTO 1
      IF(X.EQ.0.0D0)  GOTO 2
      IF(X.GT.0.5D0)  GOTO 3
      T=FR1(X)
      GOTO 200
100   WRITE(6,*) 'FLI2 -> NOT ALLOWED,X=',X,'STOP ***'
      STOP
1     Y=-X
      IF(Y.GT.0.5D0) GOTO 4
      Y2=Y**2
      T=FR1(Y2)/2.0D0-FR1(Y)
      GOTO 200
2     T=0.0D0
      GOTO 200
3     XM=1.0D0-X
      T=-FR1(XM)-LOG(X)*LOG(XM)+ZETA2
      GOTO 200
4     YM=1.0D0-Y
      T1=-FR1(YM)-LOG(Y)*LOG(YM)+ZETA2
      Y2=Y**2
      IF(Y2.LT.0.5) THEN
      T2=FR1(Y2)
      ELSE
      T2=-FR1(1.0D0-Y2)-LOG(Y2)*LOG(1.0D0-Y2)+ZETA2
      ENDIF
      T=T2/2.0D0-T1
      GOTO 200
5     T=ZETA2
      GOTO 200
6     T=-ZETA2/2.0D0
      GOTO 200
C 
200   FLI2=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION FR1(X)
C     --------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  SUBSIDIARTY ROUTINE FOR  LI2(X)
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      Y=LOG(1.0D0-X)
      Y2=Y*Y
C
      T = (-1.D0+(-1.D0/4.D0+(-1.D0/36.D0+(1.D0/3600.D0+(
     &-1.D0/211680.D0+(1.D0/10886400.D0+(-1.D0/526901760.D0
     &+(691.D0/16999766784000.D0+(-1.D0/1120863744000.D0
     &+3617.D0/0.18140058832896D18*Y2)*Y2)*Y2)*Y2)*Y2)*Y2)
     &*Y2)*Y)*Y)*Y
C
      FR1=T
C
      RETURN
      END
      SUBROUTINE GAMMAL(ZZ,RES)
C     -------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  LOG(GAMMA(Z)) FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 T1,T2,ZZ,Z,X,X2,RES
C
      Z=ZZ
      PII = 3.141592653589793238462643D0
C
      ONNE=DCMPLX(1.0D0,0.0D0)
      T=DCMPLX(0.0D0,0.0D0)
2     R=SQRT(DREAL(Z)**2+DIMAG(Z)**2)
      IF(R.GT.10.0D0) GOTO 1
      T=T-LOG(Z)
      Z=Z+ONNE
      GOTO 2
1     CONTINUE
C
      T1=Z*(LOG(Z)-1.0D0)+LOG(2.0D0*PII/Z)/2.0D0
C
      X=ONNE/Z
      X2=X*X
      T2 = (1.D0/12.D0+(-1.D0/360.D0+(1.D0/1260.D0+(-1.D0/1680.D0+(1.D0/
     #1188.D0+(-691.D0/360360.D0+(1.D0/156.D0-3617.D0/122400.D0*X2)*X2
     #)*X2)*X2)*X2)*X2)*X2)*X
C
      RES=T1+T2+T
C
      RETURN
      END
      SUBROUTINE GAMMA(ZZ,RES)
C     ------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  GAMMA(Z) FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      COMPLEX*16 ZZ,T,RES
C
      CALL GAMMAL(ZZ,T)
C
      RES=EXP(T)
C
      RETURN
      END
      SUBROUTINE BETAA(AA,BB,RES)
C     --------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C---  BETA(A,B) FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      COMPLEX*16 T1,T2,T3,T,RES,AA,BB
C
      CALL GAMMAL(AA,T1)
      CALL GAMMAL(BB,T2)
      CALL GAMMAL(AA+BB,T3)
      T=T1+T2-T3
C
      RES=EXP(T)
C
      RETURN
      END
      SUBROUTINE PSI0(ZZ,RES)
C     -----------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  PSI(Z) FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 Z,T,ONNE,Y,T0,RES,ZZ
C
      ONNE=DCMPLX(1.0D0,0.0D0)
C
      Z=ZZ
      T=DCMPLX(0.0D0,0.0D0)
2     R=SQRT(DREAL(Z)**2+DIMAG(Z)**2)
      IF(R.GT.10.0D0) GOTO 1
      T=T-ONNE/Z
      Z=Z+ONNE
      GOTO 2
1     Y=ONNE/Z
      Y2=Y*Y
      T0 = (-1.D0/2.D0+(-1.D0/12.D0+(1.D0/120.D0+(-1.D0/252.D0+(1.D0/240
     #.D0+(-1.D0/132.D0+(691.D0/32760.D0+(-ONNE/12.0D0+ONNE*3617.0D0
     #             /8160.D0*Y2
     #  )*Y2  )*Y2  )*Y2  )*Y2  )*Y2  )*Y2  )*Y)*Y-LOG(Y)
C
      RES=T+T0
C
      RETURN
      END
      SUBROUTINE PSI1(ZZ,RES)
C     -----------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  PSI'(Z) FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 Z,T,ONNE,Y,T0,RES,ZZ
C
      ONNE=DCMPLX(1.0D0,0.0D0)
C
      Z=ZZ
      T=DCMPLX(0.0D0,0.0D0)
2     R=SQRT(DREAL(Z)**2+DIMAG(Z)**2)
      IF(R.GT.10.0D0) GOTO 1
      T=T+ONNE/Z**2
      Z=Z+ONNE
      GOTO 2
1     Y=ONNE/Z
      Y2=Y*Y
      T0 = (1.D0+(1.D0/2.D0+(1.D0/6.D0+(-1.D0/30.D0+
     &(1.D0/42.D0+(-1.D0/30.D0+(5.D0/66.D0-691.D0/2730.D0*Y2)
     &*Y2)*Y2)*Y2)*Y2)*Y)*Y)*Y
C
      RES=T+T0
C
      RETURN
      END
      SUBROUTINE PSI2(ZZ,RES)
C     -----------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  PSI''(Z) FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 Z,T,ONNE,Y,T0,RES,ZZ
C
      ONNE=DCMPLX(1.0D0,0.0D0)
      TWO=ONNE*2.0D0
C
      Z=ZZ
      T=DCMPLX(0.0D0,0.0D0)
2     R=SQRT(DREAL(Z)**2+DIMAG(Z)**2)
      IF(R.GT.10.0D0) GOTO 1
      T=T-TWO/Z**3
      Z=Z+ONNE
      GOTO 2
1     Y=ONNE/Z
      Y2=Y*Y
      T0 =(-1.D0+(-1.D0+(-1.D0/2.D0+(1.D0/6.D0+(-1.D0/6.D0+(3.D0/
     &10.D0+(-5.D0/6.D0+691.D0/210.D0*Y2)*Y2)*Y2)*Y2)*Y2)*Y)*Y)*Y2
C
      RES=T+T0
C
      RETURN
      END
      SUBROUTINE PSI3(ZZ,RES)
C     -----------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  PSI'''(Z) FOR COMPLEX ARGUMENT
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 Z,T,ONNE,Y,T0,RES,ZZ
C
      ONNE=DCMPLX(1.0D0,0.0D0)
      SIX=ONNE*6.0D0
C
      Z=ZZ
      T=DCMPLX(0.0D0,0.0D0)
2     R=SQRT(DREAL(Z)**2+DIMAG(Z)**2)
      IF(R.GT.10.0D0) GOTO 1
      T=T+SIX/Z**4
      Z=Z+ONNE
      GOTO 2
1     Y=ONNE/Z
      Y2=Y*Y
C
      T0 = (2.D0+(3.D0+(2.D0+(-1.D0+(4.D0/3.D0+(-3.D0+(10.D0+(-691.D0/15
     #.D0+(280.D0-10851.D0/5.D0*Y2  )*Y2  )*Y2  )*Y2  )*Y2  )*Y2  )*Y2
     #)*Y)*Y)*Y2*Y
C
      RES=T+T0
C
      RETURN
      END
      SUBROUTINE BET(ZZ,RES)
C     ----------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  BET(Z) FOR COMPLEX ARGUMENT  $\beta(z)$
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 Z,ONNE,RES,V1,V2,Z1,Z2,ZZ
C
      ONNE=DCMPLX(1.0D0,0.0D0)
C
      Z=ZZ
      Z1=(Z+ONNE)/2.0D0
      Z2=Z/2.0D0
      CALL PSI0(Z1,V1)
      CALL PSI0(Z2,V2)
C
      RES=(V1-V2)/2.0D0
C
      RETURN
      END
      SUBROUTINE BET1(ZZ,RES)
C     -----------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  BET'(Z) FOR COMPLEX ARGUMENT  $\beta'(z)$
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 Z,ONNE,RES,V1,V2,Z1,Z2,ZZ
C
      ONNE=DCMPLX(1.0D0,0.0D0)
C
      Z=ZZ
      Z1=(Z+ONNE)/2.0D0
      Z2=Z/2.0D0
      CALL PSI1(Z1,V1)
      CALL PSI1(Z2,V2)
C
      RES=(V1-V2)/4.0D0
C
      RETURN
      END
      SUBROUTINE BET2(ZZ,RES)
C     -----------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  BET''(Z) FOR COMPLEX ARGUMENT  $\beta''(z)$
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 Z,ONNE,RES,V1,V2,Z1,Z2,ZZ
C
      ONNE=DCMPLX(1.0D0,0.0D0)
C
      Z=ZZ
      Z1=(Z+ONNE)/2.0D0
      Z2=Z/2.0D0
      CALL PSI2(Z1,V1)
      CALL PSI2(Z2,V2)
C
      RES=(V1-V2)/8.0D0
C
      RETURN
      END
      SUBROUTINE BET3(ZZ,RES)
C     -----------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  BET'''(Z) FOR COMPLEX ARGUMENT  $\beta'''(z)$
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMPLEX*16 Z,ONNE,RES,V1,V2,Z1,Z2,ZZ
C
      ONNE=DCMPLX(1.0D0,0.0D0)
C
      Z=ZZ
      Z1=(Z+ONNE)/2.0D0
      Z2=Z/2.0D0
      CALL PSI3(Z1,V1)
      CALL PSI3(Z2,V2)
C
      RES=(V1-V2)/16.0D0 !IB debug: originally divided 1/8.D0
C
      RETURN
      END

      DOUBLE PRECISION FUNCTION FLI3(X)
C     -----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  LI3(X) FOR -1. LE . X . LE .+1
C
C************************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      A=1D0
      F=0D0
      AN=0D0
      TCH=1D-16
1     AN=AN+1D0
      A=A*X
      B=A/AN**3
      F=F+B
      IF(ABS(B)-TCH)2,2,1
2     FLI3=F
      END
      DOUBLE PRECISION FUNCTION FLI4(X)
C     -----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  LI4(X) FOR -1. LE . X . LE .+1
C
C************************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      A=1D0
      F=0D0
      AN=0D0
      TCH=1D-16
1     AN=AN+1D0
      A=A*X
      B=A/AN**4
      F=F+B
      IF(ABS(B)-TCH)2,2,1
2     FLI4 = F
      END


      DOUBLE PRECISION FUNCTION SUM1(K,N)
C     -----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  SINGLE (ALTERNATING) HARMONIC SUM
C
C************************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      K1 = ABS(K)
      S1 = 0.0D0
C
      DO 10 L=1,N
         IF ((K. LE. 0) .AND. (MOD(L,2) .NE. 0)) THEN
            S1 = S1 - 1.0D0/DBLE(L)**K1
         ELSE
            S1 = S1 + 1.0D0/DBLE(L)**K1
         END IF
 10   CONTINUE
C
      SUM1 = S1
      RETURN
      END
               
      DOUBLE PRECISION FUNCTION SUM2(J,K,N)
C     -------------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  DOUBLE (ALTERNATING) HARMONIC SUM
C
C************************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      J1 = ABS(J)
      K1 = ABS(K)
C
      S1 = 0.0D0
      S2 = 0.0D0
C
      DO 10 L=1,N
         IF ((K .LE. 0) .AND. (MOD(L,2) .NE. 0)) THEN
            S1 = S1 - 1.0D0/DBLE(L)**K1
         ELSE
            S1 = S1 + 1.0D0/DBLE(L)**K1
         END IF
         IF ((J .LE. 0) .AND. (MOD(L,2) .NE. 0)) THEN
            S2 = S2 - S1/DBLE(L)**J1
         ELSE
            S2 = S2 + S1/DBLE(L)**J1
         END IF
 10   CONTINUE
C
      SUM2 = S2
      RETURN
      END
      DOUBLE PRECISION FUNCTION SUM3(I,J,K,N)
C     ---------------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  TRIPLE (ALTERNATING) HARMONIC SUM
C
C************************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      I1 = ABS(I)
      J1 = ABS(J)
      K1 = ABS(K)
C
      S1 = 0.0D0
      S2 = 0.0D0
      S3 = 0.0D0
C
      DO 10 L=1,N
         IF ((K .LE. 0) .AND. (MOD(L,2) .NE. 0)) THEN
            S1 = S1 - 1.0D0/DBLE(L)**K1
         ELSE
            S1 = S1 + 1.0D0/DBLE(L)**K1
         END IF
         IF ((J .LE. 0) .AND. (MOD(L,2) .NE. 0)) THEN
            S2 = S2 - S1/DBLE(L)**J1
         ELSE
            S2 = S2 + S1/DBLE(L)**J1
         END IF
         IF ((I .LE. 0) .AND. (MOD(L,2) .NE. 0)) THEN
            S3 = S3 - S2/DBLE(L)**I1
         ELSE
            S3 = S3 + S2/DBLE(L)**I1
         END IF
 10   CONTINUE
C
      SUM3 = S3
      RETURN
      END
      DOUBLE PRECISION FUNCTION SUM4(I,J,K,L,N)
C     -----------------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  QUADRUPLE (ALTERNATING) HARMONIC SUM
C
C************************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      I1 = ABS(I)
      J1 = ABS(J)
      K1 = ABS(K)
      L1 = ABS(L)
C
      T1 = 0.0D0
      T2 = 0.0D0
      T3 = 0.0D0
      T4 = 0.0D0
C
      DO 10 M=1,N
         IF ((L .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T1 = T1 - 1.0D0/DBLE(M)**L1
         ELSE
            T1 = T1 + 1.0D0/DBLE(M)**L1
         END IF
         IF ((K .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T2 = T2 - T1/DBLE(M)**K1
         ELSE
            T2 = T2 + T1/DBLE(M)**K1
         END IF
         IF ((J .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T3 = T3 - T2/DBLE(M)**J1
         ELSE
            T3 = T3 + T2/DBLE(M)**J1
         END IF
         IF ((I .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T4 = T4 - T3/DBLE(M)**I1
         ELSE
            T4 = T4 + T3/DBLE(M)**I1
         END IF
 10   CONTINUE
C
      SUM4 = T4
      RETURN
      END
      DOUBLE PRECISION FUNCTION SUM5(I,J,K,L,I10,N)
C     ---------------------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  FIVEFOLD  (ALTERNATING) HARMONIC SUM
C
C************************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      I1 = ABS(I)
      J1 = ABS(J)
      K1 = ABS(K)
      L1 = ABS(L)
      II1= ABS(I10)
C
      T1 = 0.0D0
      T2 = 0.0D0
      T3 = 0.0D0
      T4 = 0.0D0
      T5 = 0.0D0
C
      DO 10 M=1,N
         IF ((I10 .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T1 = T1 - 1.0D0/DBLE(M)**II1
         ELSE
            T1 = T1 + 1.0D0/DBLE(M)**II1
         END IF
         IF ((L .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T2 = T2 - T1/DBLE(M)**L1
         ELSE
            T2 = T2 + T1/DBLE(M)**L1
         END IF
         IF ((K .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T3 = T3 - T2/DBLE(M)**K1
         ELSE
            T3 = T3 + T2/DBLE(M)**K1
         END IF
         IF ((J .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T4 = T4 - T3/DBLE(M)**J1
         ELSE
            T4 = T4 + T3/DBLE(M)**J1
         END IF
         IF ((I .LE. 0) .AND. (MOD(M,2) .NE. 0)) THEN
            T5 = T5 - T4/DBLE(M)**I1
         ELSE
            T5 = T5 + T4/DBLE(M)**I1
         END IF
 10   CONTINUE
C
      SUM5 = T5
      RETURN
      END

      DOUBLE PRECISION FUNCTION XCG1(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1+X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      N1=N
C
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
C
      T1=SUM2(-1,1,N1)
      T2=SUM1(1,N1)
      T3=SUM1(-1,N1)
      T4=SUM1(-2,N1)
C
      T=(DL*DL/2.0D0+T1-(T2-T3)*DL-T2*T3-T4)*IFA
C
      XCG1=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG2(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG^2(1+X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T1=SUM3(1,1,-1,N1)
      T2=SUM2(1,-1,N1)
      T3=SUM2(1,1,N1)
      T4=SUM1(1,N1)
      T5=SUM1(-1,N1)
C
      T=(T1-DL*(T2-T3)-DL**2/2.0D0*(T4-T5)+DL**3/6.0D0)*IFA*2.0D0
C
      XCG2=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG3(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LI2(X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T1=SUM2(-2,1,N1)
      T2=SUM1(-1,N1)
C
      IFA=-IFA
      T= (T1-ZETA2*T2+5.0D0/8.0D0*ZETA3-ZETA2*DL)*IFA
C
      XCG3=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG4(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LI2(-X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T1=SUM2(2,-1,N1)
      T2=SUM1(-1,N1)
      T3=SUM1(2,N1)
      T4=SUM1(-2,N1)
C
      IFA=-IFA
      T= (T1+DL*(T3-T4)+ZETA2*T2/2.0D0-ZETA3/4.0D0+ZETA2*DL/2.0D0)*IFA
C
      XCG4=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG5(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(X)*LI2(X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T0=SUM2(-2,2,N1)
      T1=SUM2(-3,1,N1)
      T2=SUM1(-2,N1)
      T=(T0+2.0D0*T1-2.0D0*ZETA2*T2-ZETA2**2*3.0D0/40.0D0)*IFA
C
      XCG5=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG6(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LI3(X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T1=SUM2(-3,1,N1)
      T2=SUM1(-2,N1)
      T3=SUM1(-1,N1)
C
      T=(T1-ZETA2*T2+ZETA3*T3+3.0D0/5.0D0*ZETA2**2-2.0D0*XLI4
     &  -3.0D0/4.0D0*ZETA3*DL+ZETA2*DL**2/2.0D0-DL**4/12.0D0)*IFA
C
      XCG6=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG7(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LI3(-X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T1=SUM2(3,-1,N1)
      T2=SUM1(3,N1)
      T3=SUM1(-3,N1)
      T4=SUM1(-2,N1)
      T5=SUM1(-1,N1)
C
      T=(T1+DL*(T2-T3)+ZETA2/2.0D0*T4-3.0D0/4.0D0*ZETA3*T5
     &   +ZETA2**2/8.0D0
     &   -3.0D0/4.0D0*ZETA3*DL)*IFA
C
      XCG7=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG8(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: S12(X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T1=SUM3(-2,1,1,N1)
      T2=SUM1(-1,N1)
C
      T=-IFA*(T1+XLI4-ZETA3*T2-ZETA2**2/8.0D0-ZETA3*DL/8.0D0
     &  -ZETA2*DL**2/4.0D0+DL**4/24.0D0)
C
      XCG8=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG9(N)
C     ---------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: S12(-X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
C
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T1=SUM3(2,1,-1,N1)
      T2=SUM2(2,1,N1)
      T3=SUM2(2,-1,N1)
      T4=SUM1(2,N1)
      T5=SUM1(-2,N1)
      T6=SUM1(-1,N1)
C
      T=-(T1+DL*(T2-T3)-DL*DL/2.0D0*(T4-T5)-ZETA3/8.0D0*T6-3.0D0*XLI4
     &  +6.0D0/5.0D0*ZETA2**2-11.0D0/4.0D0*ZETA3*DL
     &  +3.0D0/4.0D0*ZETA2*DL*DL-DL**4/8.0D0)*IFA
C
      IFA=-IFA
      XCG9=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG10(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: I1(X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
C
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
C
      T1=SUM3(-2,-1,-1,N1)
      T2=SUM3(2,-1,1,N1)
      T3=SUM2(-2,1,N1)
      T4=SUM2(-2,-1,N1)
      T5=SUM1(2,N1)
      T6=SUM1(-2,N1)
      T7=SUM1(-1,N1)
C
      T= -(T1+T2-DL*(T3-T4)+(T5-T6)/2.0D0*(ZETA2-DL**2)
     &    +5.0D0/8.0D0*ZETA3*(T7+DL)-3.0D0/20.0D0*ZETA2**2)*IFA
C
      IFA=-IFA
      XCG10=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG11(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1-X)*LI2(X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
C
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
      T1=SUM3(-1,2,1,N1)
      T2=SUM2(-1,1,N1)
      T3=SUM3(-2,1,1,N1)
      T4=SUM1(-1,N1)
C
      S12R=-IFA*(T3+XLI4-ZETA3*T4-ZETA2**2/8.0D0-ZETA3*DL/8.0D0
     &  -ZETA2*DL**2/4.0D0+DL**4/24.0D0)
C
      T=(T1-ZETA2*T2-19.0D0/40.0D0*ZETA2**2+XLI4+ZETA3*DL/4.0D0
     &  +DL**2*ZETA2/4.0D0+DL**4/24.0D0)*IFA-2.0D0*S12R
C
      XCG11=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG12(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1-X)*LI2(-X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
C
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF
C
      T1=SUM3(2,-1,1,N1)
      T2=SUM3(-2,-1,-1,N1)
      T3=SUM3(-1,-2,-1,N1)
      T4=SUM2(-2, 1,N1)
      T5=SUM2(-1,2,N1)
      T6=SUM2(-1,1,N1)
      T7=SUM1(-1,N1)
      T8=SUM1(-2,N1)
      T9=SUM1(3,N1)
      T10=SUM1(2,N1)
      T11=SUM1(-1,N1)
C
      T=(T1+T2+T3-(T4+T5)*DL+T6*ZETA2/2.0D0+T7*T8*DL+T9*DL
     &+(ZETA2-DL**2)/2.0D0*(T10-T8)+5.0D0/8.0D0*ZETA3*T7-4.0D0*XLI4
     &+3.0D0/2.0D0*ZETA2**2-21.0D0/8.0D0*ZETA3*DL
     &+3.0D0/4.0D0*ZETA2*DL**2-DL**4/6.0D0)*IFA
C
      XCG12=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG13(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(1+X)*LI2(-X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
      IF(MOD(N1,2).NE.1) THEN
         IFA= 1
         ELSE
         IFA=-1
      ENDIF

C
      T1=SUM3(1,2,-1,N1)
      T2=SUM3(2,1,-1,N1)
      T3=SUM2(2,1,N1)
      T4=SUM2(1,-2,N1)
      T5=SUM2(2,-1,N1)
      T6=SUM1(1,N1)
      T7=SUM1(2,N1)
      T8=SUM1(3,N1)
      T9=SUM1(-1,N1)
      T10=SUM2(1,-1,N1)
      T11=SUM1(-2,N1)
      T12=SUM1(1 ,N1)
C
      T=(T1+2.0D0*T2+(T3-T4-2*T5+T6*T7+T8-ZETA2/2.0D0*T9)*DL
     &  +T10*ZETA2/2.0D0-(T7-T11)*DL**2
     &  -(ZETA3/4.0D0-ZETA2*DL/2.0D0)*T12
     &  -3.0D0*XLI4+6.0D0/5.0D0*ZETA2**2-21.0D0/8.0D0*ZETA3*DL
     &  +ZETA2/2.0D0*DL**2-DL**4/8.0D0)*IFA
C
      XCG13=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG14(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LOG^2(1+X)-LOG^2(2))/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
C
      T1=SUM3(-1,1,-1,N1)
      T2=SUM2(-1,-1,N1)
      T3=SUM2(-1,1,N1)
      T4=SUM1(1,N1)
      T5=SUM1(-1,N1)
C
      T=(T1-DL*(T2-T3)+DL*DL/2.0D0*(T4-T5))*2.0D0
      T=T-DL**2*T4-ZETA3/4.0D0+ZETA2*DL-2.0D0/3.0D0*DL**3
C
      XCG14=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG15(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LOG(1+X)-LOG(2))/(X-1)*LI2(X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
C
      T1=SUM3(-1,-2,1,N1)
      T2=SUM3(2,-1,-1,N1)
      T3=SUM3(-2,-1,1,N1)
      T4=SUM2(-1,-1,N1)
      T5=SUM1(1,N1)
      T6=SUM1(-1,N1)
      T7=SUM2(2,1,N1)
      T8=SUM2(2,-1,N1)
      T9=SUM1(2,N1)
      T10=SUM1(-2,N1)
C
      T=T1+T2+T3-ZETA2*T4-(5.0D0/8.0D0*ZETA3-ZETA2*DL)*(T5-T6)
     &  +5.0D0/8.0D0*ZETA3*T5-DL*(T7-T8)
     &  -(ZETA2-DL**2)/2.0D0*(T9-T10)
C >>
     &  -DL*(   ZETA2*T5)
     &  +19.0D0/40.0D0*ZETA2**2-XLI4+7.0D0/4.0D0*ZETA3*DL
     &  -ZETA2*DL**2/4.0D0-DL**4/24.0D0
     &  +DL*(SUM2(2,1,N1)-ZETA3*2.0D0)
C
      XCG15=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG16(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LOG(1+x)-LOG(2))/(X-1)*LI2(-X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
C
      T= 2.0D0*SUM3(-2,1,-1,N)+SUM3(-1,2,-1,N)
     & +DL*(2.0D0*(-SUM2(-2,-1,N)+SUM2(-2,1,N))
     &            -SUM2(-1,-2,N)+SUM2(-1,2,N))
     & +ZETA2/2.0D0*SUM2(-1,-1,N)-DL**2*(SUM1(-2,N)-SUM1(2,N))
     & -ZETA3/4.0D0*SUM1(1,N)
     & -(ZETA3/4.0D0-ZETA2*DL/2.0D0)*(SUM1(-1,N)-SUM1(1,N))
C>>
     & +DL*(SUM2(-2,-1,N)-DL*(SUM1(2,N)-SUM1(-2,N))
     & +ZETA2/2.0D0*SUM1(1,N))
     & -33.0D0/20.0D0*ZETA2**2+4.0D0*XLI4
     & +13.0D0/4.0D0*ZETA3*DL-3.0D0/4.0D0*ZETA2*DL**2+DL**4/6.0D0
C
      XCG16=T
C
      RETURN


      END
      DOUBLE PRECISION FUNCTION XCG17(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(X)*LOG^2(1+X)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      N1=N
      DL=D2
      XLI4=ZLI4
C
      T1= SUM2(-1,-2,N)                                                  
      T2= SUM2(-1, 2,N)
      T3= SUM2(-1,-1,N)
      T4= SUM2(-1, 1,N)
      T5= SUM1(-1,N)
      T6= SUM1( 1,N)
      T7= SUM2(-2, 1,N)
      T8= SUM2(-2,-1,N)
      T9= SUM1(-2,   N)
      T10=SUM1( 2,   N)
C
      U1=DL*(T1-T2)+(ZETA3/8.0D0)*(T5-T6)
      U2=-ZETA2/2.0D0*T4
      U3=-(T7-T8)*DL+(T9-T10)*DL**2/2.0D0 +ZETA3/8.0D0*T6
C
      TEST1B=2.0D0*(SUM3(-1,2,-1,N)+SUM3(-1,1,-2,N)+SUM3(-2,1,-1,N))
      T     =  (U1+U2+U3)*2.0D0-TEST1B
      CO    =  7.0D0/4.0D0*ZETA2**2-4.0D0*ZLI4-21.0D0/4.0D0*ZETA3*D2
     &        +5.0D0/2.0D0*ZETA2*D2**2-D2**4/6.0D0
C
      XCG17= T+CO
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG18(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LI2(X)-ZETA2)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      N1=N
      DL=D2
      XLI4=ZLI4
C
      N1=N
C
      T1=SUM2(2,1,N1)
C
      T=-T1+2.0D0*ZETA3
C
      XCG18= T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG19(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LI2(-X)+ZETA2/2)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      N1=N
      DL=D2
      XLI4=ZLI4
C
      T1=SUM2(-2,-1,N1)
      T2=SUM1(2,N1)
      T3=SUM1(-2,N1)
      T4=SUM1(1,N1)
C
      T=-T1+DL*(T2-T3)-5.0D0/8.0D0*ZETA3
C
      XCG19= T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG20(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LI3(X)-ZETA3)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      N1=N
      DL=D2
      XLI4=ZLI4
C
      T1=SUM1(1,N1)
      T2=SUM1(2,N1)
      T3=SUM2(3,1,N1)
C
      T=         -ZETA2*T2+T3+ZETA2**2/2.0D0
C
      XCG20= T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG21(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (S12(X)-ZETA3)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      N1=N
      DL=D2
      XLI4=ZLI4
C
      T1=SUM3(2,1,1,N1)
      T2=SUM1(1,N1)
C
      T=-T1+6.0D0/5.0D0*ZETA2**2
C
      XCG21= T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG22(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: LOG(X)*LI2(X)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
C
      N1=N
C
      T1=SUM1(2,N1)
      T2=SUM1(4,N1)
      T3=SUM2(3,1,N1)
C
      T=-2.0D0*ZETA2*T1+T1**2/2.0D0+T2/2.0D0+2.0D0*T3
     &  +3.0D0/10.0D0*ZETA2**2
C
      XCG22=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG23(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (LI3(-X)+3*ZETA3/4)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
C
      N1=N
C
      T1=SUM2(-3,-1,N1)
      T2=SUM1(3,N1)
      T3=SUM1(-3,N1)
      T4=SUM1(2,N1)
      T5=SUM1(1,N1)
C
      T= T1-(T2-T3)*DL+ZETA2/2.0D0*T4-3.0D0/4.0D0*ZETA3*T5
     &  +2.0D0*XLI4-11.0D0/10.0D0*ZETA2**2+7.0D0/4.0D0*ZETA3*DL
     &  -ZETA2/2.0D0*DL**2+DL**4/12.0D0+3.0D0/4.0D0*ZETA3*T5
C
      XCG23= T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG24(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (I1(X)+5/8*ZETA3)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
      N1=N
C
      T1=SUM3(2,-1,-1,N1)
      T2=SUM3(-2,-1, 1,N1)
      T3=SUM2(2, 1,   N1)
      T4=SUM2(2,-1,   N1)
      T5=SUM1(2,N1)
      T6=SUM1(-2,N1)
      T7=SUM1(1,N1)
C
      T= 
C        -5.0D0/8.0D0*ZETA3*T7
     &  -T1-T2+DL*(T3-T4)+(ZETA2-DL**2)/2.0D0*(T5-T6)
     &  +ZETA2**2/4.0D0-2.0D0*XLI4-7.0D0/4.0D0*ZETA3*DL
     &   +ZETA2*DL**2/2.0D0        -DL**4/12.0D0
C
      XCG24=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG25(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR: (S12(-X)-ZETA(3)/8)/(X-1)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
C
      N1=N
C
      T1=SUM3(-2,1,-1,N1)
      T2=SUM2(-2,1,N1)
      T3=SUM2(-2,-1,N1)
      T4=SUM1(-2,N1)
      T5=SUM1(2,N1)
      T6=SUM1(1,N1)
C
      T= -T1-(T2-T3)*DL+(T4-T5)*DL**2/2.0D0+3.0D0/40.0D0*ZETA2**2
C
      XCG25=T
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION XCG26(N)
C     ----------------------------------
C************************************************************************
C
C     J. BLUEMLEIN:  01.10.1999 (1.00)
C
C************************************************************************
C
C---  MELLIN TRANSFORM FOR:  LOG^3(1-X)/(1+X)
C---  SUM REPRESENTATION FOR INTEGER N
C
C************************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /PIIZ/PII,ZETA2,ZETA3,ZLI4,D2
C
      DL=D2
      XLI4=ZLI4
C
      T1=SUM4(-1,1,1,1,N)
      IF(MOD(N,2).EQ.1) IFA=1
      IF(MOD(N,2).EQ.0) IFA=-1
C
      T=(T1+XLI4)*6*IFA
C
      XCG26=T
C
      RETURN
      END
