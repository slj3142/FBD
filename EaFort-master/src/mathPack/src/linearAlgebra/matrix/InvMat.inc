Subroutine InvMat_RealSquareMatrix(A, invA)
    Implicit None
    Real(RP), allocatable :: A(:, :)
    Real(RP), allocatable :: invA(:, :)
    !!...
    Integer  :: nRowA, nColA
    Real(RP), allocatable :: tmpI(:, :)
    Integer :: ii

    !!... Check the size of A and b
    nRowA = size(A, 1);     nColA = size(A, 2)

    if (nRowA.ne.nColA) then
        Call GURU%Error( &
        &   msg  = "The number rows and column of 'A' is different.", &
        &   head = "InvMat_RealSquareMatrix" )
    end if

    Allocate( tmpI(nRowA, nRowA) ); tmpI(:, :) = 0.D0
    do ii = 1, nRowA
        tmpI(ii, ii) = 1.D0
    end do

    !!... Solver Linear System
    Call SolveSquareMatrix( A, tmpI, invA )

    if (allocated(tmpI)) deallocate(tmpI)

End Subroutine

Subroutine InvMat_ComplexSquareMatrix(A, invA)
    Implicit None
    Complex(CP), allocatable :: A(:, :)
    Complex(CP), allocatable :: invA(:, :)
    !!...
    Integer  :: nRowA, nColA
    Complex(CP), allocatable :: tmpI(:, :)
    Integer :: ii

    !!... Check the size of A and b
    nRowA = size(A, 1);     nColA = size(A, 2)

    if (nRowA.ne.nColA) then
        Call GURU%Error( &
        &   msg  = "The number rows and column of 'A' is different.", &
        &   head = "InvMat_ComplexSquareMatrix" )
    end if

    Allocate( tmpI(nRowA, nRowA) ); tmpI(:, :) = dcmplx(0.D0, 0.D0)
    do ii = 1, nRowA
        tmpI(ii, ii) = dcmplx(1.D0, 0.D0)
    end do

    !!... Solver Linear System
    Call SolveSquareMatrix( A, tmpI, invA )

    if (allocated(tmpI)) deallocate(tmpI)

End Subroutine

Subroutine InvMat3x3_Real(A, invA)
    IMPLICIT NONE
    Real(RP), PARAMETER :: EPS = 1.0D-12
    Real(RP), DIMENSION(3,3), INTENT(IN)  :: A
    Real(RP), DIMENSION(3,3), INTENT(OUT) :: invA
    Real(RP) :: determinant
    Real(RP), DIMENSION(3,3) :: cofactor

    determinant = A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)  &
    &           - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)  &
    &           + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1)

    IF ( dabs(determinant) .LE. EPS ) THEN
        invA(:, :) = 0.0_RP
        Call GURU%Error( &
        &   msg   = "Determinant is zero. Singular",    &
        &   head  = "InvMat3x3_Real",                   &
        &   value = determinant                         )
    END IF

    cofactor(1,1) = +(A(2,2)*A(3,3)-A(2,3)*A(3,2))
    cofactor(1,2) = -(A(2,1)*A(3,3)-A(2,3)*A(3,1))
    cofactor(1,3) = +(A(2,1)*A(3,2)-A(2,2)*A(3,1))
    cofactor(2,1) = -(A(1,2)*A(3,3)-A(1,3)*A(3,2))
    cofactor(2,2) = +(A(1,1)*A(3,3)-A(1,3)*A(3,1))
    cofactor(2,3) = -(A(1,1)*A(3,2)-A(1,2)*A(3,1))
    cofactor(3,1) = +(A(1,2)*A(2,3)-A(1,3)*A(2,2))
    cofactor(3,2) = -(A(1,1)*A(2,3)-A(1,3)*A(2,1))
    cofactor(3,3) = +(A(1,1)*A(2,2)-A(1,2)*A(2,1))

    invA = Transpose(cofactor) / determinant

End Subroutine

Subroutine InvMat3x3_Complex(A, invA)
    IMPLICIT NONE
    Real(RP), PARAMETER :: EPS = 1.0D-12
    Complex(CP), DIMENSION(3,3), INTENT(IN)  :: A
    Complex(CP), DIMENSION(3,3), INTENT(OUT) :: invA
    Complex(CP) :: determinant
    Complex(CP), DIMENSION(3,3) :: COFACTOR

    determinant = A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)  &
    &           - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)  &
    &           + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1)

    IF ( zabs(determinant).LE. EPS ) THEN
        invA(:, :) = 0.0D0
        Call GURU%Error( &
        &   msg   = "Determinant is zero. Singular",    &
        &   head  = "InvMat3x3_Complex",                &
        &   value = determinant                         )
    END IF

    cofactor(1,1) = +(A(2,2)*A(3,3)-A(2,3)*A(3,2))
    cofactor(1,2) = -(A(2,1)*A(3,3)-A(2,3)*A(3,1))
    cofactor(1,3) = +(A(2,1)*A(3,2)-A(2,2)*A(3,1))
    cofactor(2,1) = -(A(1,2)*A(3,3)-A(1,3)*A(3,2))
    cofactor(2,2) = +(A(1,1)*A(3,3)-A(1,3)*A(3,1))
    cofactor(2,3) = -(A(1,1)*A(3,2)-A(1,2)*A(3,1))
    cofactor(3,1) = +(A(1,2)*A(2,3)-A(1,3)*A(2,2))
    cofactor(3,2) = -(A(1,1)*A(2,3)-A(1,3)*A(2,1))
    cofactor(3,3) = +(A(1,1)*A(2,2)-A(1,2)*A(2,1))

    invA = Transpose(cofactor) / determinant

End Subroutine
