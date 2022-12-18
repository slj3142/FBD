Function BesselI_IntReal(n, x) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!...
    Integer, Parameter :: IACC = 40
    Real(RP), Parameter :: BIGNI = 1.D-10, BIGNO = 1.D10
    Real(RP) :: fac, absX, bCoef, BI,BI1,BIP,BIM
    Integer  :: absN, M, i
    ! fac = 1.0_RP
    ! if ( x.lt.0.0_RP ) fac = (-1.0_RP)**n
    !funcValue = fac * Bessel_KN(n, dabs(x))

    absN = abs(n)
    If ( absN.eq.0 ) then
        funcValue = BesselI0_RealX(x)
    else if ( absN.eq.1 ) then
        funcValue = BesselI1_RealX(x)
    else
        absX  = dabs( x )
        bCoef = 2.0_RP / absX

        If ( absX.lt.1.D-12 ) then
            funcValue = 0.0_RP
        else

            !! Downwards recurrence
            !! An even integer, M, is computed which defines the start point of the
            !! downward recurrence. To improve accuracy increase the value of IACC.

            M = 2*((absN+INT(DSQRT(DBLE(IACC*absN)))))

            !! Initialise the remaining terms and start the downward recurrence
            BIP  = 0.0D0
            BI   = 0.0D0
            BI1  = 1.0D0

            do i = m, 1, -1
                BIM = DBLE(I)*bCoef*BI1 + BIP
                BIP = BI1
                BI1 = BIM

                IF ( DABS(BI1) .GT. BIGNO ) THEN
                   BI1  = BI1*BIGNI
                   BIP  = BIP*BIGNI
                   BI   = BI*BIGNI
                ENDIF

                if (i.eq.absN) BI = BIP

            end do

            funcValue = BI * BesselI0_RealX( x ) / BI1

        end if

        If ( (x.lt.0.0_RP).AND.(mod(absN, 2).EQ.1) ) funcValue = - funcValue

    end if

End Function

Function DerivBesselI_IntReal( n, x ) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!... Local Variables
    Real(RP) :: inp1, inm1
    funcValue = 0.0_RP
    inp1 = BesselI(n + 1, x)

    if ( n.eq.0 ) then
        funcValue = inp1
    else
        inm1      = BesselI(n - 1, x)
        funcValue = 0.5_RP * ( inm1 + inp1 )
    end if

End Function

Function BesselI0_RealX(x) Result( funcValue )
!!
!!   Return Bessel function I0 for positive x
!!
    Real(RP), intent(in) :: x
    Real(RP)             :: funcValue
    !!... Local Variables
    Integer  :: i
    Real(RP) :: absX, val1, val2, val3
    Double Precision, Parameter :: CONST1 = 3.75D0
    DOUBLE PRECISION :: P(7), Q(9)
    DATA P /  1.0D0,         3.51562290D0,  3.08994240D0,   &
    &         1.20674920D0,  0.26597320D0,  0.3607680D-1,   &
    &         0.458130D-2                                   /
    DATA Q /  0.398942280D0,  0.13285920D-1,  0.2253190D-2, &
    &        -0.1575650D-2,   0.9162810D-2,  -0.20577060D-1,&
    &         0.26355370D-1, -0.16476330D-1,  0.3923770D-2  /
    !!....
    funcValue = 0.0_RP
    absX = dabs(x)

    If ( absX.LT.CONST1 ) then
        val1 = ( absX / CONST1)**2
        val2 = P(7)
        do i = 6, 1, -1
            val2 = P(i) + val2 * val1
        end do
        funcValue = val2
    else
        val2 = CONST1 / absX
        val3 = Q(9)
        do i = 8, 1, -1
            val3 = Q(i) + val3*val2
        end do
        funcValue = dexp( absX ) / dsqrt( absX ) * val3
    end if

End Function

Function BesselI1_RealX(x) Result( funcValue )
!!
!!   Return Bessel function I1 for positive x
!!
    Real(RP), intent(in) :: x
    Real(RP)             :: funcValue
    !!... Local Variables
    Integer  :: i
    Real(RP) :: absX, val1, val2, val3
    Double Precision, Parameter :: CONST1 = 3.75D0
    DOUBLE PRECISION :: P(7), Q(9)
    DATA P /  0.50D0,         0.878905940D0,  0.514988690D0,    &
    &         0.150849340D0,  0.26587330D-1,  0.3015320D-2,     &
    &         0.324110D-3                                       /
    DATA Q /  0.398942280D0, -0.39880240D-1, -0.3620180D-2,     &
    &         0.1638010D-2,  -0.10315550D-1,  0.22829670D-1,    &
    &        -0.28953120D-1,  0.17876540D-1, -0.4200590D-2      /
    !!....
    funcValue = 0.0_RP
    absX = dabs(x)

    If ( absX.LT.CONST1 ) then
        val1 = ( absX / CONST1 )**2
        val2 = P(7)
        do i = 6, 1, -1
            val2 = P(i) + val2 * val1
        end do
        funcValue = x * val2
    else
        val2 = CONST1 / absX
        val3 = Q(9)
        do i = 8, 1, -1
            val3 = Q(i) + val3*val2
        end do
        funcValue = dsign( dexp( absX ) / dsqrt( absX ), x) * val3
    end if

End Function
