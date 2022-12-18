Function ZCosh(z) Result(funcValue)
    Implicit None
    Complex(CP), intent(in) :: z
    Complex(CP)             :: funcValue
    !!... Local Variables
    real(RP) :: x, y
    Real(RP) :: realPart, ImagPart
    x = dreal(z)
    y = dimag(z)
    realPart = dcosh(x) * dcos(y)
    imagPart = dsinh(x) * dsin(y)
    funcValue = dcmplx(realPart, imagPart)
End Function

Function ZSinh(z) Result(funcValue)
    Implicit None
    Complex(CP), intent(in) :: z
    Complex(CP)             :: funcValue
    !!... Local Variables
    real(RP) :: x, y
    Real(RP) :: realPart, ImagPart
    x = dreal(z)
    y = dimag(z)
    realPart = dsinh(x) * dcos(y)
    imagPart = dcosh(x) * dsin(y)
    funcValue = dcmplx(realPart, imagPart)
End Function
