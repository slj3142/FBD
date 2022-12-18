!! -------------------------------------------------------------------------- !!
Subroutine Test_String_Split()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    Block
        Type(typString)   :: str
        Integer :: nStr, iStr
        Type(typString), Allocatable :: strSplit1(:)
        Type(typString), Allocatable :: strSplit2(:)

        write(*,*)
        write(*,"(a)") "Testing Split Functions"

        str = "adasd adsd,ahjdi asdsa, ads"
        Call str%Split( strSplit1 )
        nStr = size(strSplit1)
        do iStr = 1, nStr
            write(*,*) strSplit1(iStr)%Chars()
        end do

        write(*,*) " "
        str = "adasd adsd,ahjdi asdsa, ads"
        Call str%SplitSimple( strSplit2 )
        nStr = size(strSplit2)
        do iStr = 1, nStr
            write(*,*) strSplit2(iStr)%Chars()
        end do

    End Block


!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
