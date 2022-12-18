Function BesselK_IntReal(n, x) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!...
    Real(RP) :: fac, absX, bCoef, K0, K1, K2
    Integer  :: absN, i
    fac = 1.0_RP
    if ( x.lt.0.0_RP ) fac = (-1.0_RP)**n
    if (dabs(x).le.1.D-12) then
        Call GURU%Error( &
        &   msg   = "The argument 'x' is very small to evaluate the function.", &
        &   head  = "BesselK_IntReal", &
        &   value = x )
    elseif (x.lt.0.0_RP) then
        Call GURU%Error( &
        &   msg   = "The argument 'x' is negative. It is not supported.", &
        &   head  = "BesselY_IntReal", &
        &   value = x )
    end if

    absN = abs(n)
    absX = dabs( x )

    If ( absN.eq.0 ) then
        funcValue = BesselK0_RealX( absX )
    else if ( absN.eq.1 ) then
        funcValue = BesselK1_RealX( absX )
    else
        bCoef = 2.0_RP / absX
        K0 = BesselK0_RealX( absX )
        K1 = BesselK1_RealX( absX )
        do i = 1, absN-1
            K2 = real(i, rp) * bCoef * K1 + K0
            K0 = K1
            K1 = K2
        end do
        funcValue = K2
    end if

    funcValue = fac * funcValue

End Function

Function DerivBesselK_IntReal( n, x ) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!... Local Variables
    Real(RP) :: knp1, knm1
    funcValue = 0.0_RP
    knp1 = BesselK(n + 1, x)

    if ( n.eq.0 ) then
        funcValue = - knp1
    else
        knm1      = BesselK(n - 1, x)
        funcValue = -0.5_RP * ( knm1 + knp1 )
    end if

End Function

Function BesselK0_RealX(x) Result( funcValue )
!!
!!   Return Bessel function K0 for positive x
!!
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!... Local variables
    Real(RP) :: absX, val1, val2, val3
    DOUBLE PRECISION :: P(7), Q(7)
    Integer :: i
    DATA P / -0.577215660D0,  0.42278420D0,   0.230697560D0, &
    &         0.03488590D0,   0.2626980D-2,   0.10750D-3,    &
    &         0.740D-5                                       /
    DATA Q /  1.253314140D0, -0.078323580D0,  0.021895680D0, &
    &         -0.10624460D-1,  0.5878720D-2,  -0.251540D-2,  &
    &          0.532080D-3                                   /
    !!...
    funcValue = 0.0_RP
    absX = dabs(x)

    if ( absX.le.2.0_RP ) then
        val1 = absX*absX*0.25_RP
        val2 = P(7)

        do i = 6, 1, -1
            val2 = P(i) + val2 * val1
        end do
        val3 = BesselI0_RealX( absX )
        val3 = -val3 * dlog( absX * 0.5_RP )
        funcValue = val2 + val3
    else
        val2 = 2.0_RP / absX
        val3 = Q(7)
        do i = 6,1,-1
            val3 = Q(i) + val3*val2
        end do
        funcValue = dexp( - absX) / dsqrt( absX) * val3
    end if

End Function

Function BesselK1_RealX(x) Result( funcValue )
!!
!!   Return Bessel function K0 for positive x
!!
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!... Local variables
    Real(RP) :: absX, val1, val2, val3
    DOUBLE PRECISION :: P(7), Q(7)
    Integer :: i
    DATA P /  1.0D0,          0.154431440D0, -0.672785790D0,    &
    &        -0.181568970D0, -0.019194020D0, -0.001104040D0,    &
    &        -0.46860D-4                                        /
    DATA Q /  1.253314140D0,  0.234986190D0, -0.03655620D0,     &
    &         0.015042680D0, -0.007803530D0, 0.003256140D0,     &
    &        -0.682450D-3                                       /
    !!...
    funcValue = 0.0_RP
    absX = dabs(x)

    if ( absX.le.2.0_RP ) then
        val1 = absX*absX*0.25_RP
        val2 = P(7)
        do i = 6, 1, -1
            val2 = P(i) + val2 * val1
        end do
        val3 = BesselI1_RealX( absX )
        val3 = val3 * dlog( absX * 0.5_RP )
        funcValue = val2 / absX + val3
    else
        val2 = 2.0_RP / absX
        val3 = Q(7)
        do i = 6,1,-1
            val3 = Q(i) + val3*val2
        end do
        funcValue = dexp( - absX) / dsqrt( absX) * val3
    end if

End Function
