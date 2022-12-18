Function BesselJ_IntReal(n, x) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!...
    funcValue = Bessel_JN(n, x)
End Function

Function DerivBesselJ_IntReal( n, x ) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Real(RP) :: funcValue
    !!... Local Variables
    Real(RP) :: jnp1, jnm1
    funcValue = 0.0_RP
    jnp1 = Bessel_JN(n + 1, x)

    if ( n.eq.0 ) then
        funcValue = - jnp1
    else
        jnm1      = Bessel_JN(n - 1, x)
        funcValue = 0.5_RP * ( jnm1 - jnp1 )
    end if

End Function
