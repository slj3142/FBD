!! -------------------------------------------------------------------------- !!
Subroutine WriteMessage(msg, unit, color, isAdvance)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in)            :: msg
    Integer, intent(in), optional           :: unit
    Character(len=*), intent(in), optional  :: color
    Logical, intent(in), Optional           :: isAdvance
!! -------------------------------------------------------------------------- !!
    Integer :: unit_
    Character(len=LEN_COLOR) :: color_
    Logical :: isAdvance_
    Character(len=:), Allocatable :: wrtMessage

    unit_      = 6
    color_     = ""
    isAdvance_ = .TRUE.
    If ( Present(unit) )  unit_          = unit
    If ( Present(color) ) color_         = color
    If ( Present(isAdvance) ) isAdvance_ = isAdvance

    If ( unit_.ne.6 ) then
        If ( isAdvance_.EQV..TRUE. ) then
            write(unit_, "(a)" ) msg
        else
            write(unit_, "(a)", advance = "no") msg
        end if
    end if


    if ( present(color) ) then
        wrtMessage = Colorize(msg, color)
    else
        wrtMessage = msg
    end if

    If ( isAdvance_.EQV..TRUE. ) then
        write(*, "(a)" ) wrtMessage
    else
        write(*, "(a)", advance = "no") wrtMessage
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine WritePrefixMessageSingleVector( prefix, prefixColor, msg, head, &
    &   value, refValue, unit, isColor, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in)                 :: prefix
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), intent(in), Optional               :: value
    Class(*), dimension(:), intent(in), Optional :: refValue
    Integer, intent(in), optional                :: unit
    Logical, intent(in), optional                :: isColor
    Character(len=*), optional, intent(in)       :: prefixColor
    Logical, intent(in), optional                :: isStop
!! -------------------------------------------------------------------------- !!
    type(string) :: errStr, refStr
    Logical :: isError

    if ( present(value) ) then
        Call GetStrFromClass( value, errStr, isError )
    end if

    if ( present(refValue) ) then
        Call GetStrFromClassVector( refValue, refStr, isError )
    end if

    if ( present(value).AND.present(refValue) ) then
        Call WritePrefixMessage( &
        &   prefix      = prefix,       &
        &   prefixColor = prefixColor,  &
        &   msg         = msg,          &
        &   head        = head,         &
        &   value    = errStr%raw,   &
        &   refValue    = refStr%raw,   &
        &   unit        = unit,         &
        &   isColor     = isColor,      &
        &   isStop      = isStop        )
    else if ( present(value) )  then
        Call WritePrefixMessage( &
        &   prefix      = prefix,       &
        &   prefixColor = prefixColor,  &
        &   msg         = msg,          &
        &   head        = head,         &
        &   value    = errStr%raw,   &
        &   unit        = unit,         &
        &   isColor     = isColor,      &
        &   isStop      = isStop        )
    else if  ( present(refValue) )  then
        Call WritePrefixMessage( &
        &   prefix      = prefix,       &
        &   prefixColor = prefixColor,  &
        &   msg         = msg,          &
        &   head        = head,         &
        &   refValue    = refStr%raw,   &
        &   unit        = unit,         &
        &   isColor     = isColor,      &
        &   isStop      = isStop        )
    else
        Call WritePrefixMessage( &
        &   prefix      = prefix,       &
        &   prefixColor = prefixColor,  &
        &   msg         = msg,          &
        &   head        = head,         &
        &   unit        = unit,         &
        &   isColor     = isColor,      &
        &   isStop      = isStop        )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine WritePrefixMessageVectorVector( prefix, prefixColor, msg, head, &
    &   value, refValue, unit, isColor, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in)                 :: prefix
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), dimension(:), intent(in), Optional :: value
    Class(*), dimension(:), intent(in), Optional :: refValue
    Integer, intent(in), optional                :: unit
    Logical, intent(in), optional                :: isColor
    Character(len=*), optional, intent(in)       :: prefixColor
    Logical, intent(in), optional                :: isStop
!! -------------------------------------------------------------------------- !!
    type(string) :: errStr, refStr
    Logical :: isError

    if ( present(value) ) then
        Call GetStrFromClassVector( value, errStr, isError )
    end if

    if ( present(refValue) ) then
        Call GetStrFromClassVector( refValue, refStr, isError )
    end if

    if ( present(value).AND.present(refValue) ) then
        Call WritePrefixMessage( &
        &   prefix      = prefix,       &
        &   prefixColor = prefixColor,  &
        &   msg         = msg,          &
        &   head        = head,         &
        &   value    = errStr%raw,   &
        &   refValue    = refStr%raw,   &
        &   unit        = unit,         &
        &   isColor     = isColor,      &
        &   isStop      = isStop        )
    else if ( present(value) )  then
        Call WritePrefixMessage( &
        &   prefix      = prefix,       &
        &   prefixColor = prefixColor,  &
        &   msg         = msg,          &
        &   head        = head,         &
        &   value    = errStr%raw,   &
        &   unit        = unit,         &
        &   isColor     = isColor,      &
        &   isStop      = isStop        )
    else if  ( present(refValue) )  then
        Call WritePrefixMessage( &
        &   prefix      = prefix,       &
        &   prefixColor = prefixColor,  &
        &   msg         = msg,          &
        &   head        = head,         &
        &   refValue    = refStr%raw,   &
        &   unit        = unit,         &
        &   isColor     = isColor,      &
        &   isStop      = isStop        )
    else
        Call WritePrefixMessage( &
        &   prefix      = prefix,       &
        &   prefixColor = prefixColor,  &
        &   msg         = msg,          &
        &   head        = head,         &
        &   unit        = unit,         &
        &   isColor     = isColor,      &
        &   isStop      = isStop        )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!


!! -------------------------------------------------------------------------- !!
Subroutine WritePrefixMessage( prefix, prefixColor, msg, head, &
    &   value, refValue, unit, isColor, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in)            :: prefix
    Character(len=*), intent(in)            :: msg
    Character(len=*), intent(in), Optional  :: head
    Class(*), intent(in), Optional          :: value
    Class(*), intent(in), Optional          :: refValue
    Integer, intent(in), optional           :: unit
    Logical, intent(in), optional           :: isColor
    Character(len=*), optional, intent(in)  :: prefixColor
    Logical, intent(in), optional           :: isStop
!! -------------------------------------------------------------------------- !!
    Logical :: isColor_
    Character(LEN_COLOR) :: prefixColor_, headColor
    Integer :: unit_
    type(string)              :: str, headStr
    type(string), Allocatable :: strVect(:)
    Integer :: nMsgLine, iMsg
    Logical :: isStop_, isEndEmptyLine
    !!...
    Type(string) :: valueStr, refValueStr
    Character(len=:), Allocatable :: valueChar, refValueChar
    Logical :: isError
    !!... Convert values

    valueChar = ""
    if ( present(value) ) then
        Call GetStrFromClass(value, valueStr, isError)
        if (isError.EQV..FALSE. ) valueChar = valueStr%Chars()
    end if

    refValueChar = ""
    if ( present(refValue) ) then
        Call GetStrFromClass(refValue, refValueStr, isError)
        if (isError.EQV..FALSE. ) refValueChar = refValueStr%Chars()
    end if

    isColor_ = .FALSE.
    if ( Present(isColor) ) isColor_ = isColor

    isStop_ = .FALSE.
    If ( Present(isStop) ) isStop_ = isStop

    prefixColor_ = ""
    headColor    = ""
    If (isColor_) then
        headColor   = COLOR_HEAD
        If ( Present(prefixColor) ) prefixColor_ = prefixColor
    end if

    unit_ = 6
    if ( Present(unit) ) unit_ = unit

    str = msg
    Call str%Split( tokens = strVect, sep = lineSep )
    nMsgLine =  size(strVect)

    headStr = ""
    if ( present(head) ) headStr = head

    isEndEmptyLine = .FALSE.
    if (present(value)) then
        if (len_trim(valueChar).ge.1) isEndEmptyLine = .TRUE.
    end if

    if (present(refValue)) then
        if (len_trim(refValueChar).ge.1) isEndEmptyLine = .TRUE.
    end if

    !!----- File
    If ( unit_.ne.6 ) then

        !!... Write Header
        Write(unit_,"(a)") prefix//headStr%Chars()
        write(unit_,"(a)")

        !!... Write Message (Line Separated)
        do iMsg = 1, nMsgLine
            write(unit_,"(a)") INDENT//strVect(iMsg)%Chars()
        end do
        write(unit_,"(a)")

        !!... Write Given Value
        If ( Present(value) ) then
            if (len_trim(valueChar).ge.1) write(unit_,"(a)") INDENT//PREFIX_GIVEN//valueChar

        End if

        !!... Write Reference Value
        If ( Present(refValue) ) then
            if (len_trim(refValueChar).ge.1) write(unit_,"(a)") INDENT//PREFIX_REF//refValueChar
        End if

        if ( isEndEmptyLine ) then
            write(unit_,"(a)")
        end if

    end if

    !!----- Command Windows
    Block

        !!... Write Header
        Write(*,"(a)") Colorize(prefix, prefixColor_)//Colorize(headStr%Chars(), headColor)
        write(*,"(a)")

        !!... Write Message (Line Separated)
        do iMsg = 1, nMsgLine
            write(*,"(a)") INDENT//strVect(iMsg)%Chars()
        end do
        write(*,"(a)")

        !!... Write Given Value
        If ( Present(value) ) then
            if (len_trim(valueChar).ge.1) write(*,"(a)") INDENT//PREFIX_GIVEN//valueChar
        End if

        !!... Write Reference Value
        If ( Present(refValue) ) then
            if (len_trim(refValueChar).ge.1) write(*,"(a)") INDENT//PREFIX_REF//refValueChar
        End if

        if ( isEndEmptyLine ) then
            write(*,"(a)")
        end if

    End Block

    If ( isStop_ ) then
        write(*,"(a)") "Please insert any key to exit."
        read(*,*)
        Stop
    End If

    !!... Free the memory
    Call str%Free()
    Call headStr%Free()
    do iMsg = 1, nMsgLine
        Call strVect%Free()
    end do
    If (Allocated(strVect)) deallocate(strVect)

    If ( Allocated(valueChar) ) Deallocate(valueChar)
    If ( Allocated(refValueChar) ) Deallocate(refValueChar)
    Call valueStr%Free()
    Call refValueStr%Free()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
