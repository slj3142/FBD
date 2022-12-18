!! -------------------------------------------------------------------------- !!
Subroutine Test_String_SingleConversion()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Block
        Type(typString) :: str
        Integer(IP)     :: intValue
        Real(RP)        :: realValue

        write(*,"(a)") "Testing Numeric Values"

        str       = 3
        Call str%GetInt(intValue)
        Call str%GetReal(realValue)

        write(*,*) "intValue", intValue
        write(*,*) "realValue", realValue

        write(*,*)
        str       = 4.0_RP
        Call str%GetInt( intValue )
        Call str%GetReal( realValue )

        write(*,*) "intValue", intValue
        write(*,*) "realValue", realValue
    End Block

    Block
        Type(typString) :: str
        Logical         :: logValue

        write(*,*)
        write(*,"(a)") "Testing Logical Values (TRUE)"

        str = 'T'
        Call str%GetLogical( logValue )
        write(*,*) "logValue", logValue

        str = 'TRue'
        Call str%GetLogical( logValue )
        write(*,*) "logValue", logValue

        str = '.TRue.'
        Call str%GetLogical( logValue )
        write(*,*) "logValue", logValue

    End Block

    Block
        Type(typString) :: str
        Logical         :: logValue

        write(*,*)
        write(*,"(a)") "Testing Logical Values (FALSE)"

        str = 'F'
        Call str%GetLogical( logValue )
        write(*,*) "logValue", logValue

        str = 'FALse'
        Call str%GetLogical( logValue )
        write(*,*) "logValue", logValue

        str = '.FALSe.'
        Call str%GetLogical( logValue )
        write(*,*) "logValue", logValue
    End Block

    Block
        Type(typString)   :: str
        Character(len=:), Allocatable :: charValue

        write(*,*)
        write(*,"(a)") "Testing Char Values"

        str = 'asds'
        Call str%GetChar( charValue )
        write(*,*) 'charValue', charValue
    End Block

    Block
        Type(typString)   :: str
        Integer           :: intValue
        Real(RP)          :: realValue
        Logical           :: logValue
        Character(len=:), Allocatable :: charValue

        write(*,*)
        write(*,"(a)") "Testing Get Or Default"

        str       = "tmp"
        Call str%GetIntOrDefault( intValue, 5 )
        write(*,*) "intValue", intValue

        Call str%GetRealOrDefault( realValue, 2.0_RP )
        write(*,*) "realValue", realValue

        Call str%GetLogicalOrDefault( logValue, .TRUE. )
        write(*,*) "logValue", logValue

        Call str%GetCharOrDefault( charValue, 'Tested')
        write(*,*) 'charValue', charValue
    End Block



!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
