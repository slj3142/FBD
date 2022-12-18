!! -------------------------------------------------------------------------- !!
Subroutine Test_String_VectorConversion()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    blkIntegerVector: Block
        Type(typString)   :: str
        Integer, Allocatable :: intVec1(:)
        Integer, Allocatable :: intVec2(:)
        Integer :: iVar, nVar

        write(*,*)
        write(*,"(a)") "Testing Split Functions"

        str = "1 2 3 4 5 6 7"
        Call str%GetIntVector( intVec1 )
        nVar    = size(intVec1)
        do iVar = 1, nVar
            write(*,*) iVar, intVec1(iVar)
        end do
        write(*,*) " "

        !str = "7, 6, 5, 4, 3 2 1"
        str = "sa, asd"
        Call str%GetIntVectorOrDefault( intVec2, [1, 5, 3, 8, 7] )
        nVar    = size(intVec2)
        do iVar = 1, nVar
            write(*,*) iVar, intVec2(iVar)
        end do
        write(*,*) " "

    End Block blkIntegerVector

    blkRealVector: Block
        Type(typString)   :: str
        Real(RP), Allocatable :: intVec1(:)
        Real(RP), Allocatable :: intVec2(:)
        Integer :: iVar, nVar

        write(*,*)
        write(*,"(a)") "Testing Split Functions"

        str = "1 2 3 4 5 6 7"
        Call str%GetRealVector( intVec1 )
        nVar    = size(intVec1)
        do iVar = 1, nVar
            write(*,*) iVar, intVec1(iVar)
        end do
        write(*,*) " "

        !str = "7, 6, 5, 4, 3 2 1"
        str = "sa, asd"
        Call str%GetRealVectorOrDefault( intVec2, [5.0_RP, 3.2_RP, 8.7_RP] )
        nVar    = size(intVec2)
        do iVar = 1, nVar
            write(*,*) iVar, intVec2(iVar)
        end do
        write(*,*) " "

    End Block blkRealVector

    blkDbleVector: Block
        Type(typString)   :: str
        Real(RP), Allocatable :: intVec1(:)
        Real(RP), Allocatable :: intVec2(:)
        Integer :: iVar, nVar

        write(*,*)
        write(*,"(a)") "Testing Split Functions"

        str = "1 2 3 4 5 6 7"
        Call str%GetRealVector( intVec1 )
        nVar    = size(intVec1)
        do iVar = 1, nVar
            write(*,*) iVar, intVec1(iVar)
        end do
        write(*,*) " "

        !str = "7, 6, 5, 4, 3 2 1"
        str = "sa, asd"
        Call str%GetRealVectorOrDefault( intVec2, [1.D0, 5.D0, 3.D0] )
        nVar    = size(intVec2)
        do iVar = 1, nVar
            write(*,*) iVar, intVec2(iVar)
        end do
        write(*,*) " "

    End Block blkDbleVector

    blkLogicalVector: Block
        Type(typString)   :: str
        Logical, Allocatable :: intVec1(:)
        Logical, Allocatable :: intVec2(:)
        Integer :: iVar, nVar

        write(*,*)
        write(*,"(a)") "Testing Split Functions"

        str = "T False F T T"
        Call str%GetLogicalVector( intVec1 )
        nVar    = size(intVec1)
        do iVar = 1, nVar
            write(*,*) iVar, intVec1(iVar)
        end do
        write(*,*) " "

        !str = "7, 6, 5, 4, 3 2 1"
        str = "sa, asd"
        Call str%GetLogicalVectorOrDefault( intVec2, [.TRUE., .FALSE.] )
        nVar    = size(intVec2)
        do iVar = 1, nVar
            write(*,*) iVar, intVec2(iVar)
        end do
        write(*,*) " "

    End Block blkLogicalVector


!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
