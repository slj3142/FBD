!! -------------------------------------------------------------------------- !!
Subroutine test_GetVectorValueOrDefault()
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
        Call GetIntVectorFromCharOrDefault( "1 2 3 4", int1, [1, 2, 3], isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetIntVectorFromCharOrDefault( "{2.0, -1.0, 2.0}", int1, [1, 2, 3], isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetIntVectorFromCharOrDefault( "A, B, 2.0", int1, [1, 2, 3], isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetRealFromChar"
        Call GetRealVectorFromCharOrDefault( "1, 2, 3, 4", real1, [1.0_RP, 2.0_RP, 3.0_RP], isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetRealVectorFromCharOrDefault( "2.0, -1.0, 2.0", real1, [1.0_RP, 2.0_RP, 3.0_RP],isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetRealVectorFromCharOrDefault( "A, B, 2.0", real1, [1.0_RP, 2.0_RP, 3.0_RP],isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetLogicalFromChar"
        Call GetLogicalVectorFromCharOrDefault( "T,T,F", log1, [.TRUE., .FALSE.], isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalVectorFromCharOrDefault( "F T F F", log1, [.TRUE., .FALSE.], isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalVectorFromCharOrDefault( "A", log1, [.TRUE., .FALSE.], isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalVectorFromCharOrDefault( "tRue False", log1, [.TRUE., .FALSE.], isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetValueFromChar"
        Call GetVectorFromCharOrDefault( "[1 5 58 12]", real1, [1.0_RP, 2.0_RP], isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetVectorFromCharOrDefault( "2.0 13.25 58", real1, [1.0_RP, 2.0_RP], isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetVectorFromCharOrDefault( "A B C", real1, [1.0_RP, 2.0_RP], isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

    End Block

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
