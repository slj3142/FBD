!! -------------------------------------------------------------------------- !!
Subroutine testBesselFuncDeriv()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    Integer  :: nX, iX, n, iUnit
    Real(RP) :: x, dx, eps
    Real(RP), dimension(-5:5) :: funcValues, funcValues2, funcValues3, funcValues4

    x  = 1.D-4 -5.0_RP
    dx = 0.1_RP
    nX = 100

    eps = 1.D-8

    do iX = 1, nX

        do n = -5, 5
            ! funcValues(n)  = BesselJ(n, x)
            ! funcValues2(n) = BesselJ(n, x + eps)

            ! funcValues(n)  = BesselY(n, x)
            ! funcValues2(n) = BesselY(n, x + eps)

            ! funcValues(n)  = BesselK(n, x)
            ! funcValues2(n) = BesselK(n, x + eps)

            funcValues(n)  = BesselI(n, x)
            funcValues2(n) = BesselI(n, x + eps)

            funcValues3(n) = ( funcValues2(n) - funcValues(n) ) / eps

            ! funcValues4(n) = DerivBesselJ(n, x)
            ! funcValues4(n) = DerivBesselY(n, x)
            ! funcValues4(n) = DerivBesselK(n, x)
            funcValues4(n) = DerivBesselI(n, x)

        end do

        write(*,1001) x, funcValues3(-5:5) - funcValues4(-5:5)

        ! write(*,1001) x, funcValues4(-5:5)

        x = x + dx
    end do

    1001 format(99(1pe12.3))

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
