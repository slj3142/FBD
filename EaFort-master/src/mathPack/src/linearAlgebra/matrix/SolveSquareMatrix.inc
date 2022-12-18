Subroutine SolveSquareMatrix_Real(A, b, x)
    Implicit None
    Real(RP), allocatable :: A(:, :)
    Real(RP), allocatable :: b(:, :)
    Real(RP), allocatable :: x(:, :)
    !!...
    Integer  :: nRowA, nColA
    Integer  :: nRowB, nColB
    Real(RP), allocatable :: tmpA(:, :)
    !!... Lapack Interface
    Integer  :: n, nrhs
    Integer, allocatable :: IPIV(:)
    Integer :: lda, ldb, info

    !!... Check the size of A and b
    nRowA = size(A, 1);     nColA = size(A, 2)
    nRowB = size(B, 1);     nColB = size(B, 2)

    !!...
    If ( nRowA.NE.nRowB) then
        Call GURU%Error( &
        &   msg  = "The number rows of 'A' and 'B' is different.", &
        &   head = "SolveSquareMatrix_Real" )
    end if

    If ( nRowA.NE.nColA ) then
        Call GURU%Error( &
        &   msg  = "The number rows and column of 'A' is different.", &
        &   head = "SolveSquareMatrix_Real" )
    end if

    n    = nRowA
    LDA  = nColA

    nRHS = nColB
    LDB  = nRowB

    if ( allocated(tmpA) ) deallocate(tmpA)
    allocate(tmpA(nRowA, nColA));  tmpA(:, :) = A(:, :)

    if ( allocated(x) ) deallocate(x)
    allocate(x(nRowB, nColB));  x(:, :) = B(:, :)

    Allocate(IPIV(n))

    !!... Solve Linear Equation
    CALL DGESV( N, NRHS, tmpA, LDA, IPIV, x, LDB, INFO )

    deallocate(tmpA)
    deallocate(IPIV)

    if (INFO.EQ.0) then
        return
    else if (INFO.GT.0) then
        Call GURU%Error( &
        &   msg   = "The i-th argument had an illegal value", &
        &   head  = "SolveSquareMatrix_Real", &
        &   value = INFO )
    else if (INFO.LT.0) then
        Call GURU%Error( &
        &   msg   = "U(i,i) is exactly zero. SINGULAR", &
        &   head  = "SolveSquareMatrix_Real", &
        &   value = -INFO )
    end if

End Subroutine

Subroutine SolveSquareMatrix_Complex(A, b, x)
    Implicit None
    Complex(CP), allocatable :: A(:, :)
    Complex(CP), allocatable :: b(:, :)
    Complex(CP), allocatable :: x(:, :)
    !!...
    Integer  :: nRowA, nColA
    Integer  :: nRowB, nColB
    Complex(CP), allocatable :: tmpA(:, :)
    !!... Lapack Interface
    Integer  :: n, nrhs
    Integer, allocatable :: IPIV(:)
    Integer :: lda, ldb, info

    !!... Check the size of A and b
    nRowA = size(A, 1);     nColA = size(A, 2)
    nRowB = size(B, 1);     nColB = size(B, 2)

    !!...
    If ( nRowA.NE.nRowB) then
        Call GURU%Error( &
        &   msg  = "The number rows of 'A' and 'B' is different.", &
        &   head = "SolveSquareMatrix_Complex" )
    end if

    If ( nRowA.NE.nColA ) then
        Call GURU%Error( &
        &   msg  = "The number rows and column of 'A' is different.", &
        &   head = "SolveSquareMatrix_Complex" )
    end if

    n    = nRowA
    LDA  = nColA

    nRHS = nColB
    LDB  = nRowB

    if ( allocated(tmpA) ) deallocate(tmpA)
    allocate(tmpA(nRowA, nColA));  tmpA(:, :) = A(:, :)

    if ( allocated(x) ) deallocate(x)
    allocate(x(nRowB, nColB));  x(:, :) = B(:, :)

    Allocate(IPIV(n))

    !!... Solve Linear Equation
    CALL ZGESV( N, NRHS, tmpA, LDA, IPIV, x, LDB, INFO )

    deallocate(tmpA)
    deallocate(IPIV)

    if (INFO.EQ.0) then
        return
    else if (INFO.GT.0) then
        Call GURU%Error( &
        &   msg   = "The i-th argument had an illegal value", &
        &   head  = "SolveSquareMatrix_Complex", &
        &   value = INFO )
    else if (INFO.LT.0) then
        Call GURU%Error( &
        &   msg   = "U(i,i) is exactly zero. SINGULAR", &
        &   head  = "SolveSquareMatrix_Complex", &
        &   value = -INFO )
    end if

End Subroutine
