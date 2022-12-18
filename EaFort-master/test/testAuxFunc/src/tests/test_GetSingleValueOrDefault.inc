!! -------------------------------------------------------------------------- !!
Subroutine Test_GetSingleValueOrDefault()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    Block
        Integer :: int1, int2
        Real(RP) :: real1, real2
        Logical :: log1, log2
        Real(RP) :: val1, val2
        Logical :: isError

        write(*,*) "Test: GetIntFromChar"
        Call GetIntFromCharOrDefault( "1", int1, -1, isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetIntFromCharOrDefault( "2.0", int1, -1,isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetIntFromCharOrDefault( "A", int1, -1,isError )
        write(*,*) " value : ", int1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetRealFromChar"
        Call GetRealFromCharOrDefault( "1", real1, -2.0_RP, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetRealFromCharOrDefault( "2.0", real1, -2.0_RP, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetRealFromCharOrDefault( "A", real1, -2.0_RP, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetLogicalFromChar"
        Call GetLogicalFromCharOrDefault( "1", log1, .TRUE., isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalFromCharOrDefault( "2.0", log1, .TRUE., isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalFromCharOrDefault( "A", log1, .TRUE., isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetLogicalFromCharOrDefault( "tRue", log1, .TRUE., isError )
        write(*,*) " value : ", log1
        write(*,*) " error : ", isError
        write(*,*)

        write(*,*) "Test: GetValueFromChar"
        Call GetValueFromCharOrDefault( "1", real1, -40.0_RP, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetValueFromCharOrDefault( "2.0", real1, -40.0_RP, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)

        Call GetValueFromCharOrDefault( "A", real1, -40.0_RP, isError )
        write(*,*) " value : ", real1
        write(*,*) " error : ", isError
        write(*,*)
    End Block



!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
