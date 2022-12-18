!! -------------------------------------------------------------------------- !!
Subroutine testBesselFunc()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    Integer  :: nX, iX, n, iUnit
    Real(RP) :: x, dx
    Real(RP), dimension(-5:5) :: funcValues

    x  = 1.D-4 -5.0_RP
    dx = 0.05_RP
    nX = 1000

    open(newunit = iUnit, file = "result.txt", status = "replace")

    do iX = 1, nX

        do n = -5, 5
            funcValues(n) = BesselJ(n, x)
            ! funcValues(n) = BesselY(n, x)
            ! funcValues(n) = BesselK(n, x)
            ! funcValues(n) = BesselI(n, x)
        end do

        write(iUnit, 1001) x, funcValues(-5:5)

        x = x + dx
    end do

    1001 format(99(1pe15.6))

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
