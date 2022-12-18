      PROGRAM MAIN
      IMPLICIT DOUBLE PRECISION (A-H,K,O-Z)
      COMMON /PAI/ PI,PI05,PI2
      COMMON /FILEOUT/ IRAO,IDIF,IRAD
      Integer :: nTPlus
      Double Precision :: kStart, kEnd, dk, akb
      Integer :: nK, iK

      PI     = 3.14159265358979D0
      PI05   = PI*0.5D0
      PI2    = PI*2.0D0
      IPRINT = 1
      NPRINT = 0

      write(6,*) "INPUT NB,H0,SIG1,SIG2: "

      NB = 80
      H0 = 1.D0
      SIG1 = 0.8D0
      SIG2 = 0.8D0

      WRITE(6,600) NB,H0,SIG1,SIG2

!     Center of Gravity
      OGD =0.05D0

!     Radius of Roll Gyration
      KZZB=0.35D0

      write(*,*) "NT plus value:"
      NTPlus = 3
      NT     = NB + nTPlus

      !NT=NB+3
      CALL OFFSET(NB,NT,H0,SIG1,SIG2,OGD,KZZB,NPRINT)

      WRITE(6,*) "KB Start, KB End, nK: "

      kStart = 0.01D0
      kEnd   = 5.0D0
      nK     = 300

      open(newunit=IRAO,file="MotionRAO.dat",status="replace")
      write(IRAO,'(a)') "# kB/2 |X2|/A |X3|/A |X4|/(kA)"// &
      "Phs(X2) Phs(X3) Phs(X4)"

      open(newunit=IDIF,file="WaveExtForce.dat",status="replace")
      write(IRAO,'(a)') "# kB/2 |F2|/A |F3|/A |F4|/(kA)"// &
      "Phs(F2) Phs(F3) Phs(F4)"

      open(newunit=IRAD,file="RadiationForce.dat",status="replace")
      write(IRAD,'(a)') "# nFreq "
      write(IRAD,'(a)') "# kB/2 "
      write(IRAD,'(a)') "# AddedMass(3, 3) "
      write(IRAD,'(a)') "# Damping(3, 3) "
      write(IRAD,'(i5)') nK

      dk = (kEnd - kStart) / ( nK - 1.D0 )
      akb = kStart
      do iK = 1, nK
          CALL SOLVE (NB,NT,AKB)
          CALL FORCE (NB,AKB,IPRINT)
          CALL MOTION(AKB,IPRINT)
          akb = akb + dk
      end do

    9 STOP

  600 FORMAT(//14X,48('*') &
         /19X,'2-D RADIATION AND DIFFRACTION PROBLEMS', &
         /19X,'    OF A GENERAL-SHAPED 2-D BODY', &
         /19X,'     BY INTEGRAL-EQUATION METHOD',/14X,48('*'), &
        //15X,'NUMBER OF PANELS OVER THE WHOLE BODY---(NB)=',I4, &
         /15X,'HALF-BEAM TO DRAFT RATIO---------H0(=B/2/D)=',F8.4, &
         /15X,'SECTIONAL AREA RATIO FOR RIGHT-SIG1(=S/B/D)=',F8.4, &
         /15X,'SECTIONAL AREA RATIO FOR LEFT--SIG2(=S/B/D)=',F8.4/)
      END
!============================================================
      SUBROUTINE OFFSET(NB,NT,H0,SIG1,SIG2,OGD,KZZB,IPRINT)
      IMPLICIT DOUBLE PRECISION (A-H,K,O-Z)

      PARAMETER (MX=105,NP=100,NQ=101)
      COMMON /PAI/ PI,PI05,PI2
      COMMON /MDT/ CMAS,C22,OG,KZZ,GM
      COMMON /ELM/ XP(MX),YP(MX),XQ(NQ),YQ(NQ)
      COMMON /VN2/ VN(3,NP)

      open(newunit = IOFF, file = "Offset.dat", status = "replace")

      IAD=NT-NB
      DTH=PI/DFLOAT(NB)

      SIGMA=SIG1
      RSUB=(H0+1.0D0)**2+8.0D0*H0*(1.0D0-4.0D0*SIGMA/PI)
      AMD =0.25D0*(3.0D0*(H0+1.0D0)-DSQRT(RSUB))
      A1  =0.5D0*(H0-1.0D0)/AMD
      A3  =0.5D0*(H0+1.0D0)/AMD-1.0D0
      AMB =AMD/H0

      DO 100 J=1,NB/2+1
      TH=PI05-DTH*DFLOAT(J-1)
      XQ(J)=AMB*( (1.0D0+A1)*DSIN(TH)-A3*DSIN(3.0D0*TH) )
      YQ(J)=AMB*( (1.0D0-A1)*DCOS(TH)+A3*DCOS(3.0D0*TH) )
  100 CONTINUE
      SIGMA=SIG2
      RSUB=(H0+1.0D0)**2+8.0D0*H0*(1.0D0-4.0D0*SIGMA/PI)
      AMD=0.25D0*(3.0D0*(H0+1.0D0)-DSQRT(RSUB))
      A1=0.5D0*(H0-1.0D0)/AMD
      A3=0.5D0*(H0+1.0D0)/AMD-1.0D0
      AMB=AMD/H0

      DO 105 J=NB/2+2,NB+1
      TH = PI05-DTH*DFLOAT(J-1)
      XQ(J)=AMB*((1.0D0+A1)*DSIN(TH)-A3*DSIN(3.0D0*TH))
      YQ(J)=AMB*((1.0D0-A1)*DCOS(TH)+A3*DCOS(3.0D0*TH))
  105 CONTINUE

      Do J = 1,NB+1
          write(IOFF, "(i5,99(1pe15.6))") J, XQ(J), YQ(J)
      End do
      close(IOFF)

      DO 110 I=1, NB
      XP(I)=(XQ(I+1)+XQ(I))/2.0D0
      YP(I)=(YQ(I+1)+YQ(I))/2.0D0
      DX=XQ(I+1)-XQ(I)
      DY=YQ(I+1)-YQ(I)
      D =DSQRT(DX*DX+DY*DY)
      VN(1,I)= DY/D
      VN(2,I)=-DX/D
      VN(3,I)=XP(I)*VN(2,I)-YP(I)*VN(1,I)
  110 CONTINUE

      IF(IAD.EQ.0) GOTO 130
      DS=(XQ(1)-XQ(NB+1))/DFLOAT(IAD+1)
      DO 120 I=1,IAD
      II=NB+I
      XP(II)=XQ(NB+1)+DS*DFLOAT(I)
      YP(II)=0.0D0
  120 CONTINUE

  130 CMAS=(SIG1+SIG2)/H0
      C22 =(XQ(1)-XQ(NB+1))/XQ(1)
      OG  =OGD/H0
      KZZ =KZZB
      SUM=0.0D0
         DO 200 J=1,NB
         S1 =YQ(J+1)-YQ(J)
         S2 =XQ(J  )*(2.0D0*YQ(J  )+YQ(J+1))
         S3 =XQ(J+1)*(2.0D0*YQ(J+1)+YQ(J  ))
         SUM=SUM+S1*(S2+S3)
  200    CONTINUE
      OBM=SUM/6.0D0
      GM =(2.0D0/3.0D0-OBM)/CMAS+OG

      WRITE(6,600) CMAS,C22,OGD,KZZ,GM
      IF(IPRINT.EQ.0) RETURN
      WRITE(6,610)
      DO 300 J=1,NB+1
  300 WRITE(6,620) J,XQ(J),YQ(J),XP(J),YP(J)
  600 FORMAT( &
          15X,'NONDIMENSIONAL MASS------- S/(B/2)**2=',F8.5, &
         /15X,'HEAVE RESTORING FORCE COEFF--AW/(B/2)=',F8.5, &
         /15X,'CENTER OF GRAVITY----------------OG/D=',F8.5, &
         /15X,'GYRATIONAL RADIUS-----------KZZ/(B/2)=',F8.5, &
         /15X,'METACENTRIC HEIGHT-----------GM/(B/2)=',F8.5/)
  610 FORMAT(/15X,'***** CHECK OF ORDINATES *****' &
         /8X,'J',6X,'XQ',8X,'YQ',10X,'XP',8X,'YP')
  620 FORMAT(7X,I2,1X,2F10.5,2X,2F10.5)
      RETURN
      END
!============================================================
      SUBROUTINE SDSUB(XPI,YPI,NB,SS,DD)
!     Kernel Function: Rankine Source
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      PARAMETER (MX=105,NQ=101)
      DIMENSION SS(NB),DD(NB)
      COMMON /ELM/ XP(MX),YP(MX),XQ(NQ),YQ(NQ)

      DO 100 J=1,NB
      SWA=0.0D0
      DWA=0.0D0
      IF(DABS(YPI).LT.1.0D-8) GOTO 10
      DX=XQ(J+1)-XQ(J)
      DY=YQ(J+1)-YQ(J)
      D=DSQRT(DX*DX+DY*DY)
      CDEL=DX/D
      SDEL=DY/D
      XA=XPI-XQ(J  )
      XB=XPI-XQ(J+1)

      SL=-1.0D0
      DO 200 L=1,2
      SL=-SL
      YA=SL*YPI-YQ(J  )
      YB=SL*YPI-YQ(J+1)
      SUBA=XA*CDEL+YA*SDEL
      SUBB=XB*CDEL+YB*SDEL
      COEF=XA*SDEL-YA*CDEL
      ABSC=DABS(COEF)
      WA1=0.5D0*(SUBB*DLOG(XB*XB+YB*YB)-SUBA*DLOG(XA*XA+YA*YA))
        IF(ABSC.LT.1.0D-10) THEN
        WA2=0.0D0
        WA3=0.0D0
        ELSE
      WA2=ABSC*(DATAN(SUBB/ABSC)-DATAN(SUBA/ABSC))
      WA3=WA2/COEF
      ENDIF
      SWA=SWA-(WA1+WA2)*SL
      DWA=DWA+ WA3*SL
  200 CONTINUE

   10 SS(J)=SWA
      DD(J)=DWA
  100 CONTINUE
      RETURN
      END
!============================================================
      SUBROUTINE SDCAL(XPI,YPI,AK,NB,ZS,ZD)
!     Kernel Function: Wave Term
      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX*16 (Z)

      PARAMETER (MX=105,NQ=101)
      DIMENSION ZS(NB),ZD(NB)
      COMMON /PAI/ PI,PI05,PI2
      COMMON /ELM/ XP(MX),YP(MX),XQ(NQ),YQ(NQ)

      Z0=(0.0D0,0.0D0)
      ZI=(0.0D0,1.0D0)
      DO 100 J=1,NB
      ZS(J)=Z0
      ZD(J)=Z0
  100 CONTINUE

      XX=XPI-XQ(1)
      YY=YPI+YQ(1)
      SGNX=DSIGN(1.0D0,XX)
      IF(DABS(XX).LT.1.0D-10) SGNX=0.0D0
      XE=-AK*YY
      YE=-AK*DABS(XX)
      ZETA=DCMPLX(XE,YE)
      CALL EZE1Z(XE,YE,EC,ES)
      RFL1=0.5D0*DLOG(XX**2+YY**2)
      RFT1=DATAN2(YY,XX)
      ZFC1= EC-PI*CDEXP(ZETA)*ZI
      ZFS1=(ES-PI*CDEXP(ZETA))*SGNX

      DO 200 J=1,NB
      XX=XPI-XQ(J+1)
      YY=YPI+YQ(J+1)
      SGNX=DSIGN(1.0D0,XX)
      IF(DABS(XX).LT.1.0D-10) SGNX=0.0D0
      XE=-AK*YY
      YE=-AK*DABS(XX)
      ZETA=DCMPLX(XE,YE)
      CALL EZE1Z(XE,YE,EC,ES)
      RFL2=0.5D0*DLOG(XX**2+YY**2)
      RFT2=DATAN2(YY,XX)
      ZFC2= EC-PI*CDEXP(ZETA)*ZI
      ZFS2=(ES-PI*CDEXP(ZETA))*SGNX

      DX=XQ(J+1)-XQ(J)
      DY=YQ(J+1)-YQ(J)
      D =DSQRT(DX*DX+DY*DY)
      CDEL=DX/D
      SDEL=DY/D
      SUB =SDEL*(RFL2-RFL1)+CDEL*(RFT2-RFT1)
      ZSUB=SDEL*(ZFC2-ZFC1)-CDEL*(ZFS2-ZFS1)
      ZS(J)=ZS(J)+2.0D0/AK*(SUB+ZSUB)
      ZD(J)=ZD(J)-2.0D0*(ZFS2-ZFS1)
      RFL1=RFL2
      RFT1=RFT2
      ZFC1=ZFC2
      ZFS1=ZFS2
  200 CONTINUE
      RETURN
      END
!============================================================
      SUBROUTINE SOLVE(NB,NT,AK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX*16 (Z)

      PARAMETER (MX=105,NP=100,NQ=101,NEQ=4,SML=1.0D-14)
      DIMENSION ZSA(MX,NP),ZSB(MX,NEQ),ZAA(NP,NP),ZBB(NP,NEQ)
      DIMENSION ZS(NP),ZD(NP),SS(NP),DD(NP)

      COMMON /PAI/ PI,PI05,PI2
      COMMON /ELM/ XP(MX),YP(MX),XQ(NQ),YQ(NQ)
      COMMON /VN2/ VN(3,NP)
      COMMON /FAI/ ZFI(4,NP)

      Z0=(0.0D0,0.0D0)
      ZI=(0.0D0,1.0D0)
      DO 10 I=1,NB
      DO 20 J=1,NB
   20 ZAA(I,J)=Z0
      DO 10 M=1,NEQ
      ZBB(I,M)=Z0
   10 CONTINUE

      DO 30 I=1,NT
      DO 40 J=1,NB
   40 ZSA(I,J)=Z0
      DO 50 M=1,NEQ
   50 ZSB(I,M)=Z0
      IF(I.LE.NB) ZSA(I,I)=DCMPLX(PI,0.0D0)
   30 CONTINUE

      DO 100 I=1,NT
      CALL SDSUB(XP(I),YP(I),NB,SS,DD)
      CALL SDCAL(XP(I),YP(I),AK,NB,ZS,ZD)

      DO 110 J=1,NB
      ZSA(I,J)=ZSA(I,J)+DD(J)+ZD(J)
  110 CONTINUE

      DO 120 M=1,3
      DO 120 J=1,NB
      ZSB(I,M)=ZSB(I,M)+(SS(J)+ZS(J))*VN(M,J)
  120 CONTINUE
      ZSB(I,4)=PI2*CDEXP(-AK*(YP(I)-ZI*XP(I)))
  100 CONTINUE

      DO 200 I=1,NB
      DO 210 J=1,NB
      DO 210 K=1,NT
      ZAA(I,J)=ZAA(I,J)+ZSA(K,I)*ZSA(K,J)
  210 CONTINUE
      DO 220 M=1,NEQ
      DO 220 K=1,NT
      ZBB(I,M)=ZBB(I,M)+ZSA(K,I)*ZSB(K,M)
  220 CONTINUE
  200 CONTINUE

      CALL ZSWEEP(NP,NB,ZAA,ZBB,NEQ,SML)
      IF(CDABS(ZAA(1,1)).LT.SML) WRITE(6,600)
  600 FORMAT(//10X,'*** ERROR: ZSWEEP IN SUBROUTINE (SOLVE)', &
         ' WAS ABNORMALLY DONE.',/23X,'PLEASE CHECK!'///)

      DO 250 M=1,NEQ
      DO 250 I=1,NB
      ZFI(M,I)=ZBB(I,M)
  250 CONTINUE
      RETURN
      END
!============================================================
      SUBROUTINE FORCE(NB,AK,IPRINT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX*16 (Z)
      PARAMETER (MX=105,NP=100,NQ=101)
      DIMENSION A(3,3),B(3,3),BE(3,3),EAMP(3),EPHA(3)
      COMMON /PAI/ PI,PI05,PI2
      COMMON /FILEOUT/ IRAO,IDIF,IRAD
      COMMON /ELM/ XP(MX),YP(MX),XQ(NQ),YQ(NQ)
      COMMON /VN2/ VN(3,NP)
      COMMON /FAI/ ZFI(4,NP)
      COMMON /FCE/ ZAB(3,3),ZEXF(3)

      Z0=(0.0D0,0.0D0)
      ZI=(0.0D0,1.0D0)
      DO 10 I=1,3
      DO 11 J=1,3
   11 ZAB(I,J)=Z0
      ZEXF( I)=Z0
   10 CONTINUE

      DO 100 K=1,NB
      DX=XQ(K+1)-XQ(K)
      DY=YQ(K+1)-YQ(K)
      D =DSQRT(DX*DX+DY*DY)
      DO 110 I=1,3
      DO 120 J=1,3
  120 ZAB(I,J)=ZAB(I,J)-ZFI(J,K)*VN(I,K)*D
      ZEXF(I )=ZEXF(I )+ZFI(4,K)*VN(I,K)*D
  110 CONTINUE
  100 CONTINUE

      DO 150 I=1,3
      DO 160 J=1,3
      A (I,J)= DREAL(ZAB(I,J))
      B (I,J)=-DIMAG(ZAB(I,J))
  160 CONTINUE
      EAMP(I)=CDABS(ZEXF(I))
      EPHA(I)=DATAN2(DIMAG(ZEXF(I)),DREAL(ZEXF(I)))*180.0D0/PI
  150 CONTINUE

      write(IDIF, 650) AK, (EAMP(I), I=1,3), (EPHA(I), I=1,3)
      write(IRAD, 650) AK

      do I = 1, 3
      write(IRAD, 650) ( A(I, J), J = 1, 3)
      enddo

      do I = 1, 3
      write(IRAD, 650) ( B(I, J), J = 1, 3)
      end do

      IF(IPRINT.EQ.0) RETURN
      WRITE(6,600) NB,AK
      DO 300 I=1,3
      C1=B (I,I)
      C2=BE(I,I)
      CHK=DABS(C1-C2)/DABS(C1+C2)*200.0D0
  300 WRITE(6,610) I,I,A(I,I),B(I,I)
      WRITE(6,615)
      DO 310 I=1,3
      DO 310 J=1,3
      IF(I.EQ.J) GOTO 310
      WRITE(6,610) I,J,A(I,J),B(I,J)
  310 CONTINUE
      WRITE(6,630)
      DO 320 I=1,3
      WRITE(6,640) I,ZEXF(I),EAMP(I),EPHA(I)
  320 CONTINUE

  600 FORMAT(//5X,'++++++++ ADDED-MASS & DAMPING COEFF. ( ', &
          'NB=',I3,', K*B/2=',F8.4,' )  +++++++',//10X, &
          'I  J',8X,'ADDED-MASS',6X,'DAMPING')
  610 FORMAT(8X,'(',I2,',',I2,')',3X,E13.4,3(2X,E13.4))
  615 FORMAT(' ')
  620 FORMAT(8X,'(',I2,',',I2,')',3X,E13.4,2(2X,E13.4))
  630 FORMAT(//5X,'+++++ WAVE EXCITING FORCE +++++', &
        //17X,'PRESSURE INTEGRAL',12X,'AMP',5X,'PHASE(DEG)')
  640 FORMAT(8X,I2,2E13.4,2X,2E13.4,3X,E11.4,2X,F9.3)
  650 FORMAT(99(1pe15.6))
      RETURN
      END
!============================================================
      SUBROUTINE MOTION(AK,IPRINT)
      IMPLICIT DOUBLE PRECISION (A-H,K,O-Y)
      IMPLICIT COMPLEX*16 (Z)
      DIMENSION ZAA(3,3),ZBB(3)
      DIMENSION AMPG(3),PHAG(3),ZMTNG(3)
      COMMON /PAI/ PI,PI05,PI2
      COMMON /FILEOUT/ IRAO,IDIF,IRAD
      COMMON /MDT/ CMAS,C22,OG,KZZ,GM
      COMMON /FCE/ ZAB(3,3),ZEXF(3)
      COMMON /MTN/ ZMTNO(3)

      SML=1.0D-14

      ZAA(1,1)=-AK*(CMAS+ZAB(1,1))
      ZAA(1,2)=-AK* ZAB(1,2)
      ZAA(1,3)=-AK*(ZAB(1,3)+OG*ZAB(1,1))
      ZBB(1  )= ZEXF(1)

      ZAA(2,1)=-AK*ZAB(2,1)
      ZAA(2,2)=-AK*(CMAS+ZAB(2,2))+C22
      ZAA(2,3)=-AK*(ZAB(2,3)+OG*ZAB(2,1))
      ZBB(2  )= ZEXF(2)

      ZAA(3,1)=-AK*(ZAB(3,1)+OG*ZAB(1,1))
      ZAA(3,2)=-AK*(ZAB(3,2)+OG*ZAB(1,2))
      ZAA(3,3)=-AK*(CMAS*KZZ**2+ZAB(3,3)+OG*ZAB(1,3) &
               +OG*(ZAB(3,1)+OG*ZAB(1,1)))+CMAS*GM
      ZBB(3  )= ZEXF(3)+OG*ZEXF(1)

      CALL ZSWEEP(3,3,ZAA,ZBB,1,SML)
      IF(CDABS(ZAA(1,1)).LT.SML) WRITE(6,600)
  600 FORMAT(///10X,'+++ ERROR: ZSWEEP IN (MOTION) +++'///)
      DO 100 I=1,3
      ZMTNG(I)=ZBB(I)
  100 CONTINUE
      ZMTNO(1)=ZMTNG(1)+OG*ZMTNG(3)
      ZMTNO(2)=ZMTNG(2)
      ZMTNO(3)=ZMTNG(3)

      DO 200 I=1,3
      AMPG(I)=CDABS(ZMTNG(I))
      IF(I.EQ.3) AMPG(I)=AMPG(I)/AK
      PHAG(I)=DATAN2(DIMAG(ZMTNG(I)),DREAL(ZMTNG(I)))*180.0D0/PI
  200 CONTINUE

      write(IRAO, 620) AK, ( AMPG(I), I=1, 3), ( PHAG(I), I=1, 3)

      IF(IPRINT.EQ.0) RETURN
      WRITE( 6,610) AK,(AMPG(I),PHAG(I),I=1,3)
  610 FORMAT(//5X,'+++++ MOTIONS ABOUT ''G'' FOR K*B/2=',F7.3, &
         '+++++',/21X,'AMP.',7X,'PHASE',/9X,'SWAY  ',E11.4, &
         2X,F9.3,' (DEG)' ,/9X, 'HEAVE ',E11.4,2X,F9.3,' (OEG)', &
         /9X, 'ROLL  ',E11.4,2X,F9.3,' (DEG)')
      RETURN
  620 FORMAT(99(1pe15.6))
      END

!============================================================
      SUBROUTINE ZSWEEP(NDIM,N,ZA,ZB,NEQ,EPS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX*16 (Z)

      DIMENSION ZA(NDIM,NDIM), ZB(NDIM,NEQ)
      DO 5 K=1,N
      P=0.0D0
      DO 1 I=K,N
      IF(P.GE.CDABS(ZA(I,K))) GOTO 1
      P=CDABS(ZA(I,K))
      IP=I
    1 CONTINUE
      IF(P.LE.EPS) GOTO 6
      IF(IP.EQ.K)  GOTO 7
        DO 2 J=K,N
        ZW=ZA(K,J)
        ZA(K,J)=ZA(IP,J)
    2   ZA(IP,J)=ZW
          DO 20 J=1,NEQ
          ZW=ZB(K,J)
          ZB(K,J)=ZB(IP,J)
   20     ZB(IP,J)=ZW
    7 CONTINUE
      IF(K.EQ.N) GOTO 70
      DO 3 J=K+1,N
    3 ZA(K,J)=ZA(K,J)/ZA(K,K)
   70   DO 30 J=1,NEQ
   30   ZB(K,J)=ZB(K,J)/ZA(K,K)
      DO 5 I=1,N
      IF(I.EQ.K) GOTO 5
      IF(K.EQ.N) GOTO 40
        DO 4 J=K+1,N
    4   ZA(I,J)=ZA(I,J)-ZA(I,K)*ZA(K,J)
   40   CONTINUE
        DO 45 J=1,NEQ
   45   ZB(I,J)=ZB(I,J)-ZA(I,K)*ZB(K,J)
    5 CONTINUE
      ZA(1,1)=(1.0D0,0.0D0)
      RETURN
    6 ZA(1,1)=DCMPLX(DABS(P),0.0D0)
      RETURN
      END

!============================================================
      SUBROUTINE EZE1Z(XX,YY,EC,ES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX*16 (Z)
      DOUBLE PRECISION  NEW

      DATA PI,GAMMA/3.14159265358979D0,0.5772156649015D0/

      X =XX
      Y =DABS(YY)
      R =DSQRT(X*X+Y*Y)
      C =DATAN2(Y,X)

      IF(R.GT.25.0D0)  GO TO 30
      IF(X.GT.0.0D0.AND.R.GT.8.0D0)  GO TO 20
      IF(X.LE.0.0D0.AND.Y.GT.10.0D0) GO TO 20

      ER=-GAMMA-DLOG(R)+R*DCOS(C)
      EI=-C+R*DSIN(C)
      SB=-R
        DO 100 N=2,100
        FN=DFLOAT(N)
        CN=C*FN
        SB=-SB*R*(FN-1.0D0)/FN/FN
        ER=ER-SB*DCOS(CN)
        EI=EI-SB*DSIN(CN)
        IF(N.EQ.100)  GO TO 1
        IF(EI.EQ.0.0D0)  GO TO 10
        IF(DABS(SB/EI).LE.1.0D-8) GO TO 10
        GO TO 100
   10   IF(DABS(SB/ER).LE.1.0D-8) GO TO 1
  100   CONTINUE
    1 CC=DEXP(X)*DCOS(Y)
      SS=DEXP(X)*DSIN(Y)
      EC=CC*ER-SS*EI
      ES=CC*EI+SS*ER
      IF(YY.LT.0.0D0) ES=-ES
      RETURN

   20 Z =DCMPLX(X,Y)
      Z1=(1.0D0,0.0D0)
      ZSUB=(10.0D0,0.0D0)
      ZS  =Z+ZSUB/(Z1+ZSUB/Z)
        DO 200 J=1,9
        ZSUB=DCMPLX(DFLOAT(10-J),0.0D0)
        ZS  =Z+ZSUB/(Z1+ZSUB/ZS)
  200   CONTINUE
      ZSUB=Z1/ZS
      EC=DREAL(ZSUB)
      ES=DIMAG(ZSUB)
      IF(YY.LT.0.0D0) ES=-ES
      RETURN

   30 OLD=-1.0D0/R
      EXC=OLD*DCOS(C)
      EXS=OLD*DSIN(C)
        DO 300 N=2,100
        NEW=-OLD/R*DFLOAT(N-1)
          IF(EXS.EQ.0.0D0) GO TO 31
          IF(DABS(NEW/EXS).LE.1.0D-8) GO TO 31
        GO TO 32
   31   IF(EXC.EQ.0.0D0) GO TO 32
        IF(DABS(NEW/EXC).LE.1.0D-8) GO TO 33
   32   IF(DABS(OLD).LT.DABS(NEW))  GO TO 33
        OLD=NEW
        EXC=EXC+OLD*DCOS(C*DFLOAT(N))
        EXS=EXS+OLD*DSIN(C*DFLOAT(N))
  300   CONTINUE
   33 EC=-EXC
      ES=EXS
      IF(DABS(PI-DABS(C)).LT.1.0D-10) ES=-PI*DEXP(X)
      IF(YY.LT.0.0D0) ES=-ES
      RETURN
      END

!============================================================

