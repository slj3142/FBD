!! -------------------------------------------------------------------------- !!
Subroutine GetStrFromClass( arg, str, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(*), intent(in) :: arg
    type(string)         :: str
    Logical, Optional    :: isError
!! -------------------------------------------------------------------------- !!

    isError = .FALSE.
    Select Type (arg)

        Type is ( Character(len=*) )
            str = arg
        Type is ( Integer(IP) )
            str = arg
        Type is ( real(RP) )
            str = arg
        Type is ( Logical )
            if (arg.eqv..true.) then
                str = ".true."
            else
                str = ".false."
            end if
        Class default
            str = ""
            isError = .TRUE.
    End Select

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine GetStrFromClassVector( arg, str, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(*), Dimension(:), intent(in) :: arg
    type(string)                       :: str
    Logical, Optional                  :: isError
!! -------------------------------------------------------------------------- !!
    Integer :: nArg, iArg
    Logical :: isLong
    type(string) :: tmpStr

    nArg = size(arg)

    isLong = .FALSE.
    if (nArg.ge.6) isLong = .TRUE.

    isError = .FALSE.
    if (nArg.le.0) isError = .TRUE.

    if (.NOT.isError) then
    Select Type (arg)
        Type is ( Character(len=*) )

            str%raw = "[ "
            if (isLong) then
                tmpStr  = arg(1)
                str%raw = str%raw//tmpStr%raw//", "
                tmpStr  = arg(2)
                str%raw = str%raw//tmpStr%raw//", "
                str%raw = str%raw//" ... , "
                tmpStr  = arg(nArg-1)
                str%raw = str%raw//tmpStr%raw//", "
                tmpStr  = arg(nArg)
                str%raw = str%raw//tmpStr%raw
            else
                do iArg = 1, nArg-1
                    tmpStr  = arg(iArg)
                    str%raw = str%raw//tmpStr%raw//", "
                End do
                tmpStr  = arg(nArg)
                str%raw = str%raw//tmpStr%raw
            end if
            str%raw = str%raw//" ]"

        Type is ( Integer(IP) )

            str%raw = "[ "
            if (isLong) then
                tmpStr  = arg(1)
                str%raw = str%raw//tmpStr%raw//", "
                tmpStr  = arg(2)
                str%raw = str%raw//tmpStr%raw//", "
                str%raw = str%raw//" ... , "
                tmpStr  = arg(nArg-1)
                str%raw = str%raw//tmpStr%raw//", "
                tmpStr  = arg(nArg)
                str%raw = str%raw//tmpStr%raw
            else
                do iArg = 1, nArg-1
                    tmpStr  = arg(iArg)
                    str%raw = str%raw//tmpStr%raw//", "
                End do
                tmpStr  = arg(nArg)
                str%raw = str%raw//tmpStr%raw
            end if
            str%raw = str%raw//" ]"

        Type is ( real(RP) )
            str%raw = "[ "
            if (isLong) then
                tmpStr  = arg(1)
                str%raw = str%raw//tmpStr%raw//", "
                tmpStr  = arg(2)
                str%raw = str%raw//tmpStr%raw//", "
                str%raw = str%raw//" ... , "
                tmpStr  = arg(nArg-1)
                str%raw = str%raw//tmpStr%raw//", "
                tmpStr  = arg(nArg)
                str%raw = str%raw//tmpStr%raw
            else
                do iArg = 1, nArg-1
                    tmpStr  = arg(iArg)
                    str%raw = str%raw//tmpStr%raw//", "
                End do
                tmpStr  = arg(nArg)
                str%raw = str%raw//tmpStr%raw
            end if
            str%raw = str%raw//" ]"
        Type is ( Logical )

            Block
                Logical :: logArg

                str%raw = "[ "
                if (isLong) then
                    logArg  = arg(1)
                    Call GetStrFromClass( logArg, tmpStr, isError )
                    str%raw = str%raw//tmpStr%raw//", "

                    logArg  = arg(2)
                    Call GetStrFromClass( logArg, tmpStr, isError )
                    str%raw = str%raw//tmpStr%raw//", "

                    str%raw = str%raw//" ... , "

                    logArg  = arg(nArg-1)
                    Call GetStrFromClass( logArg, tmpStr, isError )
                    str%raw = str%raw//tmpStr%raw//", "

                    logArg  = arg(nArg)
                    str%raw = str%raw//tmpStr%raw
                else
                    do iArg = 1, nArg-1
                        logArg  = arg(iArg)
                        Call GetStrFromClass( logArg, tmpStr, isError )
                        str%raw = str%raw//tmpStr%raw//", "
                    End do
                    logArg  = arg(nArg)
                    Call GetStrFromClass( logArg, tmpStr, isError )
                    str%raw = str%raw//tmpStr%raw
                end if
                str%raw = str%raw//" ]"

            End Block

        Type is ( string )
            str%raw = "[ "
            if (isLong) then
                tmpStr  = arg(1)
                str%raw = str%raw//tmpStr%raw//", "
                tmpStr  = arg(2)
                str%raw = str%raw//tmpStr%raw//", "
                str%raw = str%raw//" ... , "
                tmpStr  = arg(nArg-1)
                str%raw = str%raw//tmpStr%raw//", "
                tmpStr  = arg(nArg)
                str%raw = str%raw//tmpStr%raw
            else
                do iArg = 1, nArg-1
                    tmpStr  = arg(iArg)
                    str%raw = str%raw//tmpStr%raw//", "
                End do
                tmpStr  = arg(nArg)
                str%raw = str%raw//tmpStr%raw
            end if
            str%raw = str%raw//" ]"
        Class default
            str = ""
            isError = .TRUE.
    End Select
    else
        str = ""
    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
