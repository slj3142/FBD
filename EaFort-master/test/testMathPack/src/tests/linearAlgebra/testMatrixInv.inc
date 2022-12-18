!! -------------------------------------------------------------------------- !!
Subroutine testMatrixInv()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    Block
        Real(RP), Dimension(3, 3) :: matA, invA
        Real(RP), Allocatable     :: matA2(:, :), invA2(:, :)
        integer :: i, j

        matA(:, 1) = [ 1.0_RP, 5.0_RP, 12.0_RP]
        matA(:, 2) = [ 0.0_RP, 5.7_RP, 1.2_RP]
        matA(:, 3) = [-3.5_RP, 1.2_RP, 6.0_RP]

        Allocate(matA2(3,3)); matA2(:, :) = matA(:, :)

        Call InvMat3x3( matA, invA )
        Call InvMat( matA2, invA2 )

        do i = 1, 3
            write(*,*) ( invA(i, j), j=1,3 )
        end do
        write(*,*)
        do i = 1, 3
            write(*,*) ( invA2(i, j), j=1,3 )
        end do

        Deallocate(matA2)

    End Block

    Block
        Complex(CP), Dimension(3, 3) :: matA, invA
        Complex(CP), Allocatable     :: matA2(:, :), invA2(:, :)
        integer :: i, j

        matA(:, 1) = [ 1.0_RP, 5.0_RP, 12.0_RP]
        matA(:, 2) = [ 0.0_RP, 5.7_RP, 1.2_RP]
        matA(:, 3) = [-3.5_RP, 1.2_RP, 6.0_RP]

        matA(1, 2) = matA(1, 2) + dcmplx(0.0_RP, 2.0_RP)
        matA(2, 2) = matA(2, 2) + dcmplx(0.0_RP, 1.0_RP)
        matA(3, 2) = matA(3, 2) + dcmplx(0.0_RP, -1.0_RP)

        Allocate(matA2(3,3)); matA2(:, :) = matA(:, :)

        Call InvMat3x3( matA, invA )
        Call InvMat( matA2, invA2 )

        do i = 1, 3
            write(*,*) ( invA(i, j), j=1,3 )
        end do
        write(*,*)
        do i = 1, 3
            write(*,*) ( invA2(i, j), j=1,3 )
        end do

        Deallocate(matA2)

    End Block

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
