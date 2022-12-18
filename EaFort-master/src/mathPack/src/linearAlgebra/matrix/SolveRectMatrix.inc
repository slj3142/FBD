Subroutine SolveRectMatrix_Complex( A, b, x )
    Implicit None
    Complex(CP), allocatable :: A(:, :)
    Complex(CP), allocatable :: b(:, :)
    Complex(CP), allocatable :: x(:, :)
    !!...
    Integer  :: nRowA, nColA
    Integer  :: nRowB, nColB
    Complex(CP), allocatable :: tmpA(:, :)
    complex(CP), allocatable :: WORK(:)
    !!...
    Integer :: lda, ldb, info
    Integer :: LWORK, NB
    Integer, External :: ILAENV

    !!... Check the size of A and b
    nRowA = size(A, 1);     nColA = size(A, 2)
    nRowB = size(B, 1);     nColB = size(B, 2)

    !!...
    If ( nRowA.NE.nRowB) then
        Call GURU%Error( &
        &   msg  = "The number rows of 'A' and 'B' is different.", &
        &   head = "SolveRectMatrix_Complex" )
    end if

    if ( allocated(tmpA) ) deallocate(tmpA)
    allocate(tmpA(nRowA, nColA));  tmpA(:, :) = A(:, :)

    if ( allocated(x) ) deallocate(x)
    allocate(x(nRowB, nColB));  x(:, :) = B(:, :)

    NB    = ILAENV(1, 'ZGETRF', ' ', nRowA, nColA, -1, -1)
    LWORK = NB * nRowA
    allocate( WORK(LWORK) )

    Call ZGELS('N', nRowA, nColA, nColB, tmpA, nRowA, x, nRowB, WORK, LWORK, INFO)

    deallocate( tmpA )
    deallocate( WORK )

    if (INFO.EQ.0) then
        return
    else if (INFO.GT.0) then
        Call GURU%Error( &
        &   msg   = "The i-th diagonal element of the triangular factor of A is zero.", &
        &   head  = "SolveRectMatrix_Complex", &
        &   value = INFO )
    else if (INFO.LT.0) then
        Call GURU%Error( &
        &   msg   = "The i-th argument had an illegal value", &
        &   head  = "SolveRectMatrix_Complex", &
        &   value = -INFO )
    end if

End Subroutine
