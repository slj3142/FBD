
      PROGRAM MAIN

      USE modParam
      USE pkgEaFort

      IMPLICIT DOUBLE PRECISION (A-H,K,O-Z)

      Double Precision :: kStart, kEnd, dk, akb
      Integer :: nK, iK

      Type(typString)                  :: filePath
      Type(typJSONFile)                :: jsonFile
      Type(typJSON), pointer           :: json, input
      Integer :: NB, NTPlus
      Double Precision :: H0, SIG1, SIG2, OGD, KZZB


      PI     = 3.14159265358979D0
      PI05   = PI*0.5D0
      PI2    = PI*2.0D0
      IPRINT = 1
      NPRINT = 0


      ! JSON input
      filePath = "input.json"

      Call JSON_ReadFile( filePath%Chars(), jsonFile, json )

      Call JSON_GetChild( json, "input1", input )

      Call JSON_Print( input )

      Call JSON_GetInt( input, "NB", NB )

      Call JSON_GetReal( input, "H0", H0 )

      Call JSON_GetReal( input, "SIG1", SIG1 )

      Call JSON_GetReal( input, "SIG2", SIG2 )

      Call JSON_GetReal( input, "OGD", OGD )

      Call JSON_GetReal( input, "KZZB", KZZB )

      Call JSON_GetInt( input, "NTPlus", NTPlus )


      write(6,*) "INPUT NB,H0,SIG1,SIG2: "

      ! NB = 80
      ! H0 = 1.D0
      ! SIG1 = 0.8D0
      ! SIG2 = 0.8D0

      write(6,600) NB,H0,SIG1,SIG2

      ! !Center of Gravity
      ! OGD =0.05D0

      ! !Radius of Roll Gyration
      ! KZZB=0.35D0

      write(*,*) "NT plus value:"
      ! NTPlus = 3
      NT     = NB + NTPlus

      !NT=NB+3
      call OFFSET(NB,NT,H0,SIG1,SIG2,OGD,KZZB,NPRINT)

      write(6,*) "KB Start, KB End, nK: "

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

      do iK=1,nK
        call SOLVE (NB,NT,AKB)
        call FORCE (NB,AKB,IPRINT)
        call MOTION(AKB,IPRINT)
        
        akb = akb + dk
      enddo

    9 stop

  600 format(//14X,48('*') &
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

      USE modParam
      
      IMPLICIT DOUBLE PRECISION (A-H,K,O-Z)


      open(newunit = IOFF, file = "Offset.dat", status = "replace")

      IAD=NT-NB
      DTH=PI/DFLOAT(NB)

      SIGMA=SIG1
      RSUB=(H0+1.0D0)**2+8.0D0*H0*(1.0D0-4.0D0*SIGMA/PI)
      AMD =0.25D0*(3.0D0*(H0+1.0D0)-DSQRT(RSUB))
      A1  =0.5D0*(H0-1.0D0)/AMD
      A3  =0.5D0*(H0+1.0D0)/AMD-1.0D0
      AMB =AMD/H0

      do J=1,NB/2+1
        TH=PI05-DTH*DFLOAT(J-1)
        XQ(J)=AMB*( (1.0D0+A1)*DSIN(TH)-A3*DSIN(3.0D0*TH) )
        YQ(J)=AMB*( (1.0D0-A1)*DCOS(TH)+A3*DCOS(3.0D0*TH) )
      enddo

      SIGMA=SIG2
      RSUB=(H0+1.0D0)**2+8.0D0*H0*(1.0D0-4.0D0*SIGMA/PI)
      AMD=0.25D0*(3.0D0*(H0+1.0D0)-DSQRT(RSUB))
      A1=0.5D0*(H0-1.0D0)/AMD
      A3=0.5D0*(H0+1.0D0)/AMD-1.0D0
      AMB=AMD/H0

      do J=NB/2+2,NB+1
        TH = PI05-DTH*DFLOAT(J-1)
        XQ(J)=AMB*((1.0D0+A1)*DSIN(TH)-A3*DSIN(3.0D0*TH))
        YQ(J)=AMB*((1.0D0-A1)*DCOS(TH)+A3*DCOS(3.0D0*TH))
      enddo

      do J=1,NB+1
        write(IOFF, "(i5,99(1pe15.6))") J, XQ(J), YQ(J)
      enddo

      close(IOFF)


      do I=1, NB
        XP(I)=(XQ(I+1)+XQ(I))/2.0D0
        YP(I)=(YQ(I+1)+YQ(I))/2.0D0
        DX=XQ(I+1)-XQ(I)
        DY=YQ(I+1)-YQ(I)
        D =DSQRT(DX*DX+DY*DY)
        VN(1,I)= DY/D
        VN(2,I)=-DX/D
        VN(3,I)=XP(I)*VN(2,I)-YP(I)*VN(1,I)
      enddo

      if(IAD==0) GOTO 130

      DS=(XQ(1)-XQ(NB+1))/DFLOAT(IAD+1)

      do I=1,IAD
        II=NB+I
        XP(II)=XQ(NB+1)+DS*DFLOAT(I)
        YP(II)=0.0D0
      enddo

  130 CMAS=(SIG1+SIG2)/H0
      C22 =(XQ(1)-XQ(NB+1))/XQ(1)
      OG  =OGD/H0
      KZZ =KZZB
      SUM=0.0D0

      do J=1,NB
        S1 =YQ(J+1)-YQ(J)
        S2 =XQ(J  )*(2.0D0*YQ(J  )+YQ(J+1))
        S3 =XQ(J+1)*(2.0D0*YQ(J+1)+YQ(J  ))
        SUM=SUM+S1*(S2+S3)
      enddo

      OBM=SUM/6.0D0
      GM =(2.0D0/3.0D0-OBM)/CMAS+OG

      write(6,600) CMAS,C22,OGD,KZZ,GM

      if(IPRINT==0) RETURN

      write(6,610)

      do J=1,NB+1
        write(6,620) J,XQ(J),YQ(J),XP(J),YP(J)
      enddo

  600 format( &
          15X,'NONDIMENSIONAL MASS------- S/(B/2)**2=',F8.5, &
         /15X,'HEAVE RESTORING FORCE COEFF--AW/(B/2)=',F8.5, &
         /15X,'CENTER OF GRAVITY----------------OG/D=',F8.5, &
         /15X,'GYRATIONAL RADIUS-----------KZZ/(B/2)=',F8.5, &
         /15X,'METACENTRIC HEIGHT-----------GM/(B/2)=',F8.5/)
  610 format(/15X,'***** CHECK OF ORDINATES *****' &
         /8X,'J',6X,'XQ',8X,'YQ',10X,'XP',8X,'YP')
  620 format(7X,I2,1X,2F10.5,2X,2F10.5)

      RETURN


      END

!============================================================

      SUBROUTINE SDSUB(XPI,YPI,NB,SS,DD)
      !Kernel Function: Rankine Source

      USE modParam

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION SS(NB),DD(NB)
      

      do J=1,NB
        SWA=0.0D0
        DWA=0.0D0

        if(DABS(YPI)<1.0D-8) GOTO 10

        DX=XQ(J+1)-XQ(J)
        DY=YQ(J+1)-YQ(J)
        D=DSQRT(DX*DX+DY*DY)
        CDEL=DX/D
        SDEL=DY/D
        XA=XPI-XQ(J  )
        XB=XPI-XQ(J+1)

        SL=-1.0D0

        do L=1,2
          SL=-SL
          YA=SL*YPI-YQ(J  )
          YB=SL*YPI-YQ(J+1)
          SUBA=XA*CDEL+YA*SDEL
          SUBB=XB*CDEL+YB*SDEL
          COEF=XA*SDEL-YA*CDEL
          ABSC=DABS(COEF)
          WA1=0.5D0*(SUBB*DLOG(XB*XB+YB*YB)-SUBA*DLOG(XA*XA+YA*YA))

          if(ABSC<1.0D-10) then
            WA2=0.0D0
            WA3=0.0D0
          else
            WA2=ABSC*(DATAN(SUBB/ABSC)-DATAN(SUBA/ABSC))
            WA3=WA2/COEF
          endif

          SWA=SWA-(WA1+WA2)*SL
          DWA=DWA+ WA3*SL
        enddo

   10   SS(J)=SWA
        DD(J)=DWA
      enddo

      RETURN


      END

!============================================================

      SUBROUTINE SDCAL(XPI,YPI,AK,NB,ZS,ZD)
      !Kernel Function: Wave Term

      USE modParam

      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX(kind = 8) (Z)

      DIMENSION ZS(NB),ZD(NB)


      Z0=(0.0D0,0.0D0)
      ZI=(0.0D0,1.0D0)

      do J=1,NB
        ZS(J)=Z0
        ZD(J)=Z0
      enddo

      XX=XPI-XQ(1)
      YY=YPI+YQ(1)
      SGNX=DSIGN(1.0D0,XX)

      if(DABS(XX)<1.0D-10) SGNX=0.0D0

      XE=-AK*YY
      YE=-AK*DABS(XX)
      ZETA=DCMPLX(XE,YE)

      call EZE1Z(XE,YE,EC,ES)

      RFL1=0.5D0*DLOG(XX**2+YY**2)
      RFT1=DATAN2(YY,XX)
      ZFC1= EC-PI*CDEXP(ZETA)*ZI
      ZFS1=(ES-PI*CDEXP(ZETA))*SGNX

      do J=1,NB
        XX=XPI-XQ(J+1)
        YY=YPI+YQ(J+1)
        SGNX=DSIGN(1.0D0,XX)

        if(DABS(XX)<1.0D-10) SGNX=0.0D0

        XE=-AK*YY
        YE=-AK*DABS(XX)
        ZETA=DCMPLX(XE,YE)

        call EZE1Z(XE,YE,EC,ES)

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
      enddo

      RETURN


      END

!============================================================

      SUBROUTINE SOLVE(NB,NT,AK)

      USE modParam

      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX(kind = 8) (Z)

      PARAMETER (NEQ=4,SML=1.0D-14)
      DIMENSION ZSA(MX,NP),ZSB(MX,NEQ),ZAA(NP,NP),ZBB(NP,NEQ)
      DIMENSION ZS(NP),ZD(NP),SS(NP),DD(NP)


      Z0=(0.0D0,0.0D0)
      ZI=(0.0D0,1.0D0)

      do I=1,NB
        do J=1,NB
          ZAA(I,J)=Z0
        enddo

        do M=1,NEQ
          ZBB(I,M)=Z0
        enddo
      enddo

      do I=1,NT
        do J=1,NB
          ZSA(I,J)=Z0
        enddo

        do M=1,NEQ
          ZSB(I,M)=Z0
        enddo

        if(I<=NB) ZSA(I,I)=DCMPLX(PI,0.0D0)
      enddo

      do I=1,NT
        call SDSUB(XP(I),YP(I),NB,SS,DD)
        call SDCAL(XP(I),YP(I),AK,NB,ZS,ZD)

        do J=1,NB
          ZSA(I,J)=ZSA(I,J)+DD(J)+ZD(J)
        enddo

        do M=1,3
          do J=1,NB
            ZSB(I,M)=ZSB(I,M)+(SS(J)+ZS(J))*VN(M,J)
          enddo
        enddo

        ZSB(I,4)=PI2*CDEXP(-AK*(YP(I)-ZI*XP(I)))
      enddo

      do I=1,NB
        do J=1,NB
          do K=1,NT
            ZAA(I,J)=ZAA(I,J)+ZSA(K,I)*ZSA(K,J)
          enddo
        enddo

        do M=1,NEQ
          do K=1,NT
            ZBB(I,M)=ZBB(I,M)+ZSA(K,I)*ZSB(K,M)
          enddo
        enddo
      enddo

      call ZSWEEP(NP,NB,ZAA,ZBB,NEQ,SML)

      if(CDABS(ZAA(1,1))<SML) write(6,600)

  600 format(//10X,'*** ERROR: ZSWEEP IN SUBROUTINE (SOLVE)', &
         ' WAS ABNORMALLY DONE.',/23X,'PLEASE CHECK!'///)

      do M=1,NEQ
        do I=1,NB
          ZFI(M,I)=ZBB(I,M)
        enddo
      enddo

      RETURN


      END

!============================================================

      SUBROUTINE FORCE(NB,AK,IPRINT)

      USE modParam

      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX(kind = 8) (Z)

      DIMENSION A(3,3),B(3,3),BE(3,3),EAMP(3),EPHA(3)


      Z0=(0.0D0,0.0D0)
      ZI=(0.0D0,1.0D0)

      do I=1,3
        do J=1,3
          ZAB(I,J)=Z0
        enddo

        ZEXF( I)=Z0
      enddo

      do K=1,NB
        DX=XQ(K+1)-XQ(K)
        DY=YQ(K+1)-YQ(K)
        D =DSQRT(DX*DX+DY*DY)

        do I=1,3
          do J=1,3
            ZAB(I,J)=ZAB(I,J)-ZFI(J,K)*VN(I,K)*D
          enddo

        ZEXF(I )=ZEXF(I )+ZFI(4,K)*VN(I,K)*D
        enddo
      enddo

      do I=1,3
        do J=1,3
        A (I,J)= DREAL(ZAB(I,J))
        B (I,J)=-DIMAG(ZAB(I,J))
        enddo

        EAMP(I)=CDABS(ZEXF(I))
        EPHA(I)=DATAN2(DIMAG(ZEXF(I)),DREAL(ZEXF(I)))*180.0D0/PI
      enddo

      write(IDIF, 650) AK, (EAMP(I), I=1,3), (EPHA(I), I=1,3)
      write(IRAD, 650) AK

      do I=1,3
        write(IRAD, 650) ( A(I, J), J = 1, 3)
      enddo

      do I=1,3
        write(IRAD, 650) ( B(I, J), J = 1, 3)
      end do

      if(IPRINT==0) RETURN

      write(6,600) NB,AK

      do I=1,3
        C1=B (I,I)
        C2=BE(I,I)
        CHK=DABS(C1-C2)/DABS(C1+C2)*200.0D0

        write(6,610) I,I,A(I,I),B(I,I)
      enddo

      write(6,615)

      do I=1,3
        do J=1,3
          if(I==J) GOTO 310

          write(6,610) I,J,A(I,J),B(I,J)

          310 CONTINUE
        enddo
      enddo

      write(6,630)
      
      do I=1,3
        write(6,640) I,ZEXF(I),EAMP(I),EPHA(I)
      enddo

  600 format(//5X,'++++++++ ADDED-MASS & DAMPING COEFF. ( ', &
          'NB=',I3,', K*B/2=',F8.4,' )  +++++++',//10X, &
          'I  J',8X,'ADDED-MASS',6X,'DAMPING')
  610 format(8X,'(',I2,',',I2,')',3X,E13.4,3(2X,E13.4))
  615 format(' ')
  620 format(8X,'(',I2,',',I2,')',3X,E13.4,2(2X,E13.4))
  630 format(//5X,'+++++ WAVE EXCITING FORCE +++++', &
        //17X,'PRESSURE INTEGRAL',12X,'AMP',5X,'PHASE(DEG)')
  640 format(8X,I2,2E13.4,2X,2E13.4,3X,E11.4,2X,F9.3)
  650 format(99(1pe15.6))

      RETURN


      END

!============================================================

      SUBROUTINE MOTION(AK,IPRINT)

      USE modParam

      IMPLICIT DOUBLE PRECISION (A-H,K,O-Y)
      IMPLICIT COMPLEX(kind = 8) (Z)
      
      DIMENSION ZAA(3,3),ZBB(3)
      DIMENSION AMPG(3),PHAG(3),ZMTNG(3)


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

      call ZSWEEP(3,3,ZAA,ZBB,1,SML)

      if(CDABS(ZAA(1,1))<SML) write(6,600)

  600 format(///10X,'+++ ERROR: ZSWEEP IN (MOTION) +++'///)

      do I=1,3
        ZMTNG(I)=ZBB(I)
      enddo

      ZMTNO(1)=ZMTNG(1)+OG*ZMTNG(3)
      ZMTNO(2)=ZMTNG(2)
      ZMTNO(3)=ZMTNG(3)

      do I=1,3
        AMPG(I)=CDABS(ZMTNG(I))

        if(I==3) AMPG(I)=AMPG(I)/AK

        PHAG(I)=DATAN2(DIMAG(ZMTNG(I)),DREAL(ZMTNG(I)))*180.0D0/PI
      enddo

      write(IRAO, 620) AK, ( AMPG(I), I=1, 3), ( PHAG(I), I=1, 3)

      if(IPRINT==0) RETURN

      write( 6,610) AK,(AMPG(I),PHAG(I),I=1,3)

  610 format(//5X,'+++++ MOTIONS ABOUT ''G'' FOR K*B/2=',F7.3, &
         '+++++',/21X,'AMP.',7X,'PHASE',/9X,'SWAY  ',E11.4, &
         2X,F9.3,' (DEG)' ,/9X, 'HEAVE ',E11.4,2X,F9.3,' (OEG)', &
         /9X, 'ROLL  ',E11.4,2X,F9.3,' (DEG)')
  620 format(99(1pe15.6))

      RETURN

  
      END

!============================================================

      SUBROUTINE ZSWEEP(NDIM,N,ZA,ZB,NEQ,EPS)

      USE modParam

      IMPLICIT DOUBLE PRECISION (A-H,O-Y)      
      IMPLICIT COMPLEX(kind = 8) (Z)
  
      DIMENSION ZA(NDIM,NDIM), ZB(NDIM,NEQ)

      do K=1,N
        P=0.0D0

        do I=K,N
          if(P>=CDABS(ZA(I,K))) GOTO 1

          P=CDABS(ZA(I,K))
          IP=I

    1     CONTINUE
        enddo

        if(P<=EPS) GOTO 6

        if(IP==K)  GOTO 7

        do J=K,N
          ZW=ZA(K,J)
          ZA(K,J)=ZA(IP,J)
          ZA(IP,J)=ZW
        enddo

        do J=1,NEQ
          ZW=ZB(K,J)
          ZB(K,J)=ZB(IP,J)
          ZB(IP,J)=ZW
        enddo

    7   if(K==N) GOTO 70

        do J=K+1,N
          ZA(K,J)=ZA(K,J)/ZA(K,K)
        enddo

   70   do J=1,NEQ
          ZB(K,J)=ZB(K,J)/ZA(K,K)
        enddo

        do I=1,N
          if(I==K) GOTO 5

          if(K==N) GOTO 40

          do J=K+1,N
            ZA(I,J)=ZA(I,J)-ZA(I,K)*ZA(K,J)
          enddo

   40     CONTINUE

          do J=1,NEQ
            ZB(I,J)=ZB(I,J)-ZA(I,K)*ZB(K,J)
          enddo

    5     CONTINUE
        enddo
      enddo

      ZA(1,1)=(1.0D0,0.0D0)

      RETURN


    6 ZA(1,1)=DCMPLX(DABS(P),0.0D0)

      RETURN
  
  
      END
  
  !============================================================
  
      SUBROUTINE EZE1Z(XX,YY,EC,ES)

      USE modParam
  
      IMPLICIT DOUBLE PRECISION (A-H,O-Y)
      IMPLICIT COMPLEX(kind = 8) (Z)
      
      DOUBLE PRECISION  NEW
      
      DATA GAMMA/0.5772156649015D0/
      
      
      X =XX
      Y =DABS(YY)
      R =DSQRT(X*X+Y*Y)
      C =DATAN2(Y,X)
      
      if(R>25.0D0) GOTO 30
      
      if(X>0.0D0.AND.R>8.0D0) GOTO 20
      
      if(X<=0.0D0.AND.Y>10.0D0) GOTO 20
      
      ER=-GAMMA-DLOG(R)+R*DCOS(C)
      EI=-C+R*DSIN(C)
      SB=-R
      
      do N=2,100
        FN=DFLOAT(N)
        CN=C*FN
        SB=-SB*R*(FN-1.0D0)/FN/FN
        ER=ER-SB*DCOS(CN)
        EI=EI-SB*DSIN(CN)
      
        if(N==100) GOTO 1
      
        if(EI==0.0D0) GOTO 10
      
        if(DABS(SB/EI)<=1.0D-8) GOTO 10
      
        GOTO 100
      
   10   if(DABS(SB/ER)<=1.0D-8) GOTO 1
      
  100   CONTINUE
      enddo
      
    1 CC=DEXP(X)*DCOS(Y)
      SS=DEXP(X)*DSIN(Y)
      EC=CC*ER-SS*EI
      ES=CC*EI+SS*ER
      
      if(YY<0.0D0) ES=-ES
      
      RETURN
      
      
   20 Z =DCMPLX(X,Y)
      Z1=(1.0D0,0.0D0)
      ZSUB=(10.0D0,0.0D0)
      ZS  =Z+ZSUB/(Z1+ZSUB/Z)
      
      do J=1,9
        ZSUB=DCMPLX(DFLOAT(10-J),0.0D0)
        ZS  =Z+ZSUB/(Z1+ZSUB/ZS)
      enddo
      
      ZSUB=Z1/ZS
      EC=DREAL(ZSUB)
      ES=DIMAG(ZSUB)
      
      if(YY<0.0D0) ES=-ES
      
      RETURN
      
      
   30 OLD=-1.0D0/R
      EXC=OLD*DCOS(C)
      EXS=OLD*DSIN(C)
      
      do N=2,100
        NEW=-OLD/R*DFLOAT(N-1)
      
        if(EXS==0.0D0) GOTO 31
      
        if(DABS(NEW/EXS)<=1.0D-8) GOTO 31
      
        GOTO 32
      
   31   if(EXC==0.0D0) GOTO 32
      
        if(DABS(NEW/EXC)<=1.0D-8) GOTO 33
      
   32   if(DABS(OLD)<DABS(NEW)) GOTO 33
      
        OLD=NEW
        EXC=EXC+OLD*DCOS(C*DFLOAT(N))
        EXS=EXS+OLD*DSIN(C*DFLOAT(N))
      enddo
      
   33 EC=-EXC
      ES=EXS
      
      if(DABS(PI-DABS(C))<1.0D-10) ES=-PI*DEXP(X)
    
      if(YY<0.0D0) ES=-ES
      
      RETURN
      
      
      END
      
      !============================================================