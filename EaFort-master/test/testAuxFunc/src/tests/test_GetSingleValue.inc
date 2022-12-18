!! -------------------------------------------------------------------------- !!
Subroutine Test_GetSingleValue()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    Block
        Integer :: int1, int2
        Real(RP) :: real1, re
        Logical :: log1, log2
        Real(RP) :: val1, val2
        Logical :: isError

        write(*,*) "Test: GetIntFromChar"
        Call GetIntFromChar( "1", int1, isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetIntFromChar( "2.0", int1, isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetIntFromChar( "A", int1, isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetRealFromChar"
        Call GetRealFromChar( "1", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetRealFromChar( "2.0", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetRealFromChar( "A", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetLogicalFromChar"
        Call GetLogicalFromChar( "1", log1, isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalFromChar( "2.0", log1, isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalFromChar( "A", log1, isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalFromChar( "tRue", log1, isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetValueFromChar"
        Call GetValueFromChar( "1", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetValueFromChar( "2.0", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetValueFromChar( "A", real1, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)
    End Block



!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
