!! -------------------------------------------------------------------------- !!
Subroutine Test_GetVectorValue()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    Block
        Integer, Allocatable :: int1(:), int2(:)
        Real(RP), Allocatable :: real1(:), real2(:)
        Logical, Allocatable :: log1(:), log2(:)
        Real(RP), Allocatable :: val1(:), val2(:)
        Logical :: isError

        write(*,*) "Test: GetIntFromChar"
        Call GetIntVectorFromChar( "1 2 3 4", int1, isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetIntVectorFromChar( "2.0, -1.0, 2.0", int1, isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetIntVectorFromChar( "A, B, 2.0", int1, isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetRealFromChar"
        Call GetRealVectorFromChar( "1, 2, 3, 4", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetRealVectorFromChar( "2.0, -1.0, 2.0", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetRealVectorFromChar( "A, B, 2.0", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetLogicalFromChar"
        Call GetLogicalVectorFromChar( "T,T,F", log1, isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalVectorFromChar( "F T F F", log1, isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalVectorFromChar( "A", log1, isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalVectorFromChar( "tRue False", log1, isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetValueFromChar"
        Call GetValueVectorFromChar( "[1 5 58 12]", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetValueVectorFromChar( "2.0 13.25 58", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetValueVectorFromChar( "A B C", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)
    End Block

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
