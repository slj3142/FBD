!! -------------------------------------------------------------------------- !!
Subroutine Test_CharacterFunctions()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    blkCharEqual: Block
        Logical :: isEqual

        isEqual = IsCharEqual("a", "A")
        write(*,*) "isEqual: ", isEqual

        isEqual = IsCharEqual("a", "A", .TRUE.)
        write(*,*) "isEqual: ", isEqual

    End Block blkCharEqual

    blkCharConvert: Block
        Character(len=:), Allocatable :: char1, char2

        char1 = "aBcDeF"
        char2 = UpperChar(char1)
        write(*,*) "char2: ", char2

        char2 = LowerChar(char1)
        write(*,*) "char2: ", char2
    End Block blkCharConvert

    blkRemove: Block
        Character(len=:), Allocatable :: char1

        char1 = "aBcD\eF"
        Call RemoveBackSlash(char1)
        write(*,*) "char1: ", char1

    End Block blkRemove

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
