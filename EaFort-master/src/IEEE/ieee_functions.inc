Function IsFinite_Real(x) result( isFinite )
    Real(RP), intent(in) :: x
    Logical :: isFinite
    !!... Local Variables
    isFinite = .FALSE.
    If ( IEEE_SUPPORT_DATATYPE(x) ) then
        isFinite = IEEE_IS_FINITE(x)
    End if
End Function

Function IsFinite_Complex(x) result( isFinite )
    Complex(CP), intent(in) :: x
    Logical :: isFinite
    !!... Local Variables
    Real(RP) :: realX, imagX

    realX = dreal(x)
    imagX = dimag(x)

    isFinite = .FALSE.
    If ( IEEE_SUPPORT_DATATYPE(realX) ) then
        isFinite = IEEE_IS_FINITE(realX)
    End if
    if (isFinite.eqv..false.) return

    isFinite = .FALSE.
    If ( IEEE_SUPPORT_DATATYPE(imagX) ) then
        isFinite = IEEE_IS_FINITE(imagX)
    End if

End Function

Function IsNaN_Real(x) result( isNaN )
    Real(RP), intent(in) :: x
    Logical :: isNaN
    !!... Local Variables
    isNaN = .true.
    If ( IEEE_SUPPORT_DATATYPE(x) ) then
        isNaN = IEEE_IS_NAN(x)
    End if
End Function

Function IsNaN_Complex(x) result( isNaN )
    Complex(CP), intent(in) :: x
    Logical :: isNaN
    !!... Local Variables
    Real(RP) :: realX, imagX

    realX = dreal(x)
    imagX = dimag(x)

    isNaN = .true.
    If ( IEEE_SUPPORT_DATATYPE(realX) ) then
        isNaN = IEEE_IS_FINITE(realX)
    End if
    if (isNaN.eqv..true.) return

    isNaN = .true.
    If ( IEEE_SUPPORT_DATATYPE(imagX) ) then
        isNaN = IEEE_IS_FINITE(imagX)
    End if

End Function
