Function Hankel1_IntReal(n, x) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Complex(CP)          :: funcValue
    !!...
    funcValue = dcmplx( Bessel_JN(n, x), BesselY( n, x) )
End Function

Function DerivHankel1_IntReal(n, x) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Complex(CP)          :: funcValue
    !!...
    funcValue = dcmplx( DerivBesselJ(n, x), DerivBesselY(n, x) )
End Function

Function Hankel2_IntReal(n, x) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Complex(CP)          :: funcValue
    !!...
    funcValue = dcmplx( Bessel_JN(n, x), -BesselY( n, x) )
End Function

Function DerivHankel2_IntReal(n, x) Result( funcValue )
    Integer, intent(in)  :: n
    Real(RP), intent(in) :: x
    Complex(CP)          :: funcValue
    !!...
    funcValue = dcmplx( DerivBesselJ(n, x), -DerivBesselY(n, x) )
End Function
