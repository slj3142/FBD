!! -------------------------------------------------------------------------- !!
Subroutine Test_ArgGURU()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Initizlize Description
    Call GURU%Initialize_ARG( &
    &   projectName = "Test_GURU_Argument", &
    &   description = "Testing the GURU argment \n&
                    &Test Line1", &
    &   authors     = "Young-Myung CHOI\n \n&
                    &  Dept. Naval Architecture and Ocean Engineering,\n&
                    &  Pusan National University, Republic of Korea.",  &
    &   date        = "09 December 2021",   &
    &   version     = "0.0.1"               )

    !!... Add Arguments
    Call GURU%AddKey( &
    &   key           = "input",              &
    &   singleKey     = "i",                  &
    &   description   = "Input File Path",    &
    &   isRequiredKey = .TRUE.,               &
    &   nRequiredWord = 1                     )

    !!... Add Arguments
    Call GURU%AddKey(             &
    &   key           = "output",             &
    &   singleKey     = "o",                  &
    &   description   = "Output File Path",   &
    &   isRequiredKey = .FALSE.               )

    ! !!... Add Arguments
    ! Call GURU%AddKey(                   &
    ! &   key           = "print",                    &
    ! &   singleKey     = "p",                        &
    ! &   description   = "Test another argument",    &
    ! &   isRequiredKey = .FALSE.                     )

    !!... Update Key
    Call GURU%UpdateAndPrint()

    Block
        Integer :: intValue
        Real(RP) :: realValue
        Logical :: logValue
        Character(len=:), Allocatable :: charValue
        Logical :: isError, isFound

        Call GURU%GetArgInt( "input", intValue, 2, isError )
        write(*,*) " intValue : ", intValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%IsFoundKey( "output", isFound )
        write(*,*) " isFound  : ", isFound
        write(*,*)

        Call GURU%GetArgInt( "output", intValue, 2, isError )
        write(*,*) " intValue : ", intValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%GetArgReal( "output", realValue, 1, isError )
        write(*,*) " realValue : ", realValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%GetArgLogical( "output", logValue, 1, isError )
        write(*,*) " logValue : ", logValue
        write(*,*) " isError  : ", isError
        write(*,*)

        Call GURU%GetArgChar( "output", charValue, 1, isError )
        write(*,*) " charValue : ", charValue
        write(*,*) " isError  : ", isError
        write(*,*)

    End Block

    Block
        Integer :: intValue
        Real(RP) :: realValue
        Logical :: logValue
        Character(len=:), Allocatable :: charValue
        Logical :: isError, isFound

        write(*,*) "Testing GetOrDefault"
        write(*,*)

        Call GURU%ToggleDebugMode(.TRUE.)

        Call GURU%GetArgIntOrDefault( "input", intValue, 2 )
        write(*,*) " intValue : ", intValue
        write(*,*)

        Call GURU%GetArgRealOrDefault( "output", realValue, 2.0_RP )
        write(*,*) " realValue : ", realValue
        write(*,*)

        Call GURU%GetArgLogicalOrDefault( "output", logValue, .true. )
        write(*,*) " logValue : ", logValue
        write(*,*)

        Call GURU%GetArgCharOrDefault( "output", charValue, "a" )
        write(*,*) " charValue : ", charValue
        write(*,*)

    End Block

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
