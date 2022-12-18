Function BesselY_IntReal(n, x) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!...
    Real(RP) :: fac
    fac = 1.0_RP
    if ( x.lt.0.0_RP ) fac = (-1.0_RP)**n
    if (dabs(x).le.1.D-12) then
        Call GURU%Error( &
        &   msg   = "The argument 'x' is very small to evaluate the function.", &
        &   head  = "BesselY_IntReal", &
        &   value = x )
    elseif (x.lt.0.0_RP) then
        Call GURU%Error( &
        &   msg   = "The argument 'x' is negative. It is not supported.", &
        &   head  = "BesselY_IntReal", &
        &   value = x )
    end if
    funcValue = fac * Bessel_YN(n, dabs(x))
End Function

Function DerivBesselY_IntReal( n, x ) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!... Local Variables
    Real(RP) :: ynp1, ynm1
    funcValue = 0.0_RP
    ynp1 = BesselY(n + 1, x)

    if ( n.eq.0 ) then
        funcValue = - ynp1
    else
        ynm1      = BesselY(n - 1, x)
        funcValue = 0.5_RP * ( ynm1 - ynp1 )
    end if

End Function
