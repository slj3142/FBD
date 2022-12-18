!! -------------------------------------------------------------------------- !!
Subroutine Test_VarToStringConv()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    Character(len=:), allocatable :: varChar1, varChar2

    write(*,*) "Int"
    varChar1 = Int2Char( 5 )
    write(*,*) "varChar1: ", varChar1
    varChar2 = Int2Char( [1, 5, 2] )
    write(*,*) "varChar2: ", varChar2
    write(*,*)

    write(*,*) "Real"
    varChar1 = Real2Char( 5.0_RP )
    write(*,*) "varChar1: ", varChar1
    varChar2 = Real2Char( [1.0_RP, 5.0_RP, 2.0_RP] )
    write(*,*) "varChar2: ", varChar2
    write(*,*)

    write(*,*) "Logical"
    varChar1 = Logical2Char( .TRUE. )
    write(*,*) "varChar1: ", varChar1
    varChar2 = Logical2Char( [.FALSE., .TRUE., .FALSE.] )
    write(*,*) "varChar2: ", varChar2
    write(*,*)

    Block
    Type(typString) :: str1, str2
    Type(typString), Allocatable :: strArray(:)
        str1 = "str1"
        str2 = "str23"

        Allocate( strArray(2) )
        strArray(1) = str1
        strArray(2) = str2

        write(*,*) "String"
        varChar1 = String2Char( str1 )
        write(*,*) "varChar1: ", varChar1
        ! varChar2 = String2Char( strArray )
        varChar2 = String2Char( [str1, str2] )
        write(*,*) "varChar2: ", varChar2
        write(*,*)
    End Block


!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
