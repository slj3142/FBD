Subroutine CrossProduct3_RealReal( a, b, c )
    Real(RP), dimension(3) :: a
    Real(RP), dimension(3) :: b
    Real(RP), dimension(3) :: c
    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)
End Subroutine

Subroutine CrossProduct3_RealComplex( a, b, c )
    Real(RP), dimension(3) :: a
    Complex(CP), dimension(3) :: b
    Complex(CP), dimension(3) :: c
    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)
End Subroutine

Subroutine CrossProduct3_ComplexReal( a, b, c )
    Complex(CP), dimension(3) :: a
    Real(RP), dimension(3) :: b
    Complex(CP), dimension(3) :: c
    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)
End Subroutine


Subroutine CrossProduct3_ComplexComplex( a, b, c )
    Complex(CP), dimension(3) :: a
    Complex(CP), dimension(3) :: b
    Complex(CP), dimension(3) :: c
    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)
End Subroutine
