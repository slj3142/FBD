!! -------------------------------------------------------------------------- !!
Subroutine Initialize_typCommandArgument( &
    &   this, key, singleKey, description, isRequiredKey, nRequiredWord)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typCommandArgument), intent(inout) :: this
    Character(len=*), intent(in)             :: key
    Character(len=*), Optional, intent(in)   :: singleKey
    Character(len=*), Optional, intent(in)   :: description
    Logical, Optional, intent(in)            :: isRequiredKey
    Integer, Optional, intent(in)            :: nRequiredWord
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: cleanKey
    Logical :: isCorrect

    !!... Set Key
    If (Allocated(cleanKey)) Deallocate(cleanKey)
    Call this%CheckAndCleanKey( &
    &   key       = key,        &
    &   cleanKey  = cleanKey,   &
    &   isCorrect = isCorrect   )

    If (.NOT.isCorrect) then
        Call WritePrefixMessage( &
        &   prefix      = PREIX_ERROR,                              &
        &   prefixColor = COLOR_ERROR,                              &
        &   head        = "Initialize_typCommandArgument",          &
        &   msg         = "Wrong 'key' is given. ",                 &
        &   value    = trim(key),                                &
        &   isColor     = .FALSE.,                                  &
        &   isStop      = .TRUE.                                    )
    End if

    this%key = trim(cleanKey)

    !!... Set Single Key
    If ( present(singleKey) ) then

        If (Allocated(cleanKey)) Deallocate(cleanKey)
        Call this%CheckAndCleanKey( &
        &   key       = singleKey,  &
        &   cleanKey  = cleanKey,   &
        &   isCorrect = isCorrect   )

        If (.NOT.isCorrect) then
            Call WritePrefixMessage( &
            &   prefix      = PREIX_ERROR,                              &
            &   prefixColor = COLOR_ERROR,                              &
            &   head        = "Initialize_typCommandArgument",          &
            &   msg         = "Wrong 'singleKey' is given. ",           &
            &   value    = trim(key),                                &
            &   isColor     = .FALSE.,                                  &
            &   isStop      = .TRUE.                                    )
        elseif (len_trim(cleanKey).GE.2) Then
            Call WritePrefixMessage( &
            &   prefix      = PREIX_ERROR,                              &
            &   prefixColor = COLOR_ERROR,                              &
            &   head        = "Initialize_typCommandArgument",          &
            &   msg         = "The 'singleKey' should be single character.", &
            &   value    = trim(key),                                &
            &   isColor     = .FALSE.,                                  &
            &   isStop      = .TRUE.                                    )
        else
            this%isSingleKeyGiven = .TRUE.
            this%singleKey = cleanKey(1:1)
        End if

    End If

    If (Present(description))   this%description   = description
    If (Present(isRequiredKey)) this%isRequiredKey = isRequiredKey
    If (Present(nRequiredWord)) this%nRequiredWord  = nRequiredWord

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine PrintInfo_typCommandArgument(this, ioUnit, isColor)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typCommandArgument), intent(in) :: this
    Integer, optional                     :: ioUnit
    Logical, optional                     :: isColor
!! -------------------------------------------------------------------------- !!
    Integer :: ioUnit_, iWord
    Character(len=:), Allocatable :: keyToPrint
    Logical :: isColor_
    Character(len=17) :: colorKey, colorFound, colorNotFound

    ioUnit_ = 6
    if ( present(ioUnit) ) ioUnit_ = ioUnit

    isColor_ = .FALSE.
    if ( present(isColor) ) isColor_ = isColor

    colorKey      = ""
    colorFound    = ""
    colorNotFound = ""
    if (isColor_) then
        colorKey      = "Yellow"
        colorFound    = "Green"
        colorNotFound = "Red"
        If (.NOT.this%isRequiredKey) colorNotFound = "MAGENTA"
    end if

    keyToPrint = INDENT2//"--"//Colorize(this%key, colorKey)

    if ( this%isSingleKeyGiven ) then
        keyToPrint = keyToPrint//", --"//Colorize(this%singleKey, colorKey)
    end if

    If ( this%isFound ) then
        keyToPrint = keyToPrint//" ["//Colorize("Found", colorFound)//"]"
    else
        keyToPrint = keyToPrint//" ["//Colorize("NotFound", colorNotFound)//"]"
    end if

    write(ioUnit_, "(a)") keyToPrint
    if ( this%nWord.GE.1 ) then
        do iWord = 1, this%nWord
            write( ioUnit_, "(a,i5,a,a)") INDENT3, iWord, " : ",this%words(iWord)%Chars()
        end do
    end if
    write( ioUnit_, *) ""

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine PrintHelp_typCommandArgument(this, ioUnit, isColor)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typCommandArgument), intent(in) :: this
    Integer, optional                     :: ioUnit
    Logical, optional                     :: isColor
!! -------------------------------------------------------------------------- !!
    Integer :: ioUnit_
    Character(len=:), Allocatable :: keyToPrint
    Logical :: isColor_
    Character(len=17) :: colorKey

    isColor_ = .FALSE.
    if (Present(isColor)) isColor_ = isColor

    colorKey = ""
    If (isColor_) colorKey = "Yellow"

    ioUnit_ = 6
    if ( present(ioUnit) ) ioUnit_ = ioUnit

    keyToPrint = INDENT2//"--"//Colorize(this%key, colorKey)

    If ( this%isSingleKeyGiven ) keyToPrint = keyToPrint//", "//"-"//Colorize(this%singleKey, colorKey)
    write(ioUnit_, "(a)") keyToPrint

    If ( Allocated(keyToPrint) ) Deallocate(keyToPrint)

    !!... Write Description
    blkWriteDescription : Block
        Type(string), Allocatable :: multipleLines(:)
        Integer :: nStr, iStr

        If ( Allocated(this%description%raw) ) then

            !!... Split the description into the multiple lines and print
            Call this%description%Split( multipleLines, "\\" )
            nStr = size( multipleLines )
            If ( nStr.GE.1 ) then
                do iStr = 1, nStr
                    Write(ioUnit_, "(a)") INDENT3//multipleLines(iStr)%Chars()
                end do
            end if
        end if

    End Block blkWriteDescription
    Write(ioUnit_, "(a)") ""

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine Update_typCommandArgument( this )
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typCommandArgument), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Integer             :: nTotalArg, iCh
    Integer             :: iArg, jArg, nChArg, nGivenArg
    Character(len=1024) :: arg
    Integer             :: indStart, indEnd
    Logical             :: isActive, isFound
    Integer             :: nLength

    indStart = -1
    indEnd   = -1

    !!... Get the total number of arguments
    nTotalArg = COMMAND_ARGUMENT_COUNT()

    isActive = .FALSE.
    isFound  = .FALSE.

    this%isFound = .FALSE.

    !!... Find the corresponding keyword
    Do iArg = 0, nTotalArg

        CALL GET_COMMAND_ARGUMENT( iArg,  arg )
        nChArg = len_trim(arg)

        !!... Check the argument correspond to "--key"
        if ( nChArg.GE.3 ) then

            if ( arg(1:2).EQ."--" ) then

                if ( arg(3:nChArg).EQ.this%key ) then
                    indStart = iArg
                    isActive = .TRUE.
                    isFound  = .TRUE.
                else if (isActive) then
                    indEnd   = iArg-1
                    isActive = .FALSE.
                end if

            end if

        end if

        if ( (iArg.EQ.nTotalArg).AND.isActive ) Then
            indEnd   = iArg
            isActive = .FALSE.
        end if

    End do

    !!... If Found, Save the following arguments
    If ( isFound ) Then
        this%isFound = .TRUE.

        nGivenArg = indEnd - indStart

        If (this%isRequiredKey) then
        if (nGivenArg.LT.this%nRequiredWord) then
            Call WritePrefixMessage( &
            &   prefix      = PREIX_ERROR,                              &
            &   prefixColor = COLOR_ERROR,                              &
            &   head        = "Update_typCommandArgument",              &
            &   msg         = "The number of words after keyword is less than the requirement.\n&
                            &* Required number of words is given in the below.", &
            &   value    = trim(this%key),                           &
            &   refValue    = this%nRequiredWord,                       &
            &   isColor     = .FALSE.,                                  &
            &   isStop      = .TRUE.                                    )
        end if
        End If

        If ( nGivenArg.GE.1 ) then
            this%nWord = nGivenArg
            If ( Allocated(this%words) ) Deallocate( this%words )
            Allocate( this%words(this%nWord) )
            jArg = 0
            do iArg = indStart+1, indEnd
                CALL GET_COMMAND_ARGUMENT( iArg,  arg )
                jArg = jArg + 1
                this%words(jArg) = trim(arg)
            end do
        end if

        Return

    End if

    !!... Find the corresponding single keyword
    SingleKeyLoop: Do iArg = 0, nTotalArg

        CALL GET_COMMAND_ARGUMENT( iArg,  arg )
        nChArg = len_trim(arg)

        if ( nChArg.GE.3 ) then

            if ( arg(1:2).EQ."--" ) then

                if (isActive) then
                    indEnd   = iArg - 1
                    isActive = .FALSE.
                    exit SingleKeyLoop
                end if

                if ( arg(3:3).EQ.this%singleKey ) then
                    indStart = iArg
                    isActive = .TRUE.
                    isFound  = .TRUE.
                    this%isFound = .TRUE.
                end if

            end if

        end if

        if ( (iArg.EQ.nTotalArg).AND.isActive ) Then
            indEnd   = iArg
            isActive = .FALSE.
        end if

    End do SingleKeyLoop

    !!... Save
    if (isFound) then

        nGivenArg = indEnd - indStart

        If (this%isRequiredKey) then
        if ( nGivenArg.LT.this%nRequiredWord ) then
            Call WritePrefixMessage( &
            &   prefix      = PREIX_ERROR,                              &
            &   prefixColor = COLOR_ERROR,                              &
            &   head        = "Update_typCommandArgument",              &
            &   msg         = "The number of argument after keyword is less than the requirement.\n&
                            &* Required number of words is given in the below.", &
            &   value    = trim(this%key),                           &
            &   refValue    = this%nRequiredWord,                       &
            &   isColor     = .FALSE.,                                  &
            &   isStop      = .TRUE.                                    )
        end if
        End If

        If ( nGivenArg.GE.1 ) then
            this%nWord = nGivenArg
            If ( Allocated(this%words) ) Deallocate( this%words )
            Allocate( this%words(this%nWord) )
            jArg = 0
            do iArg = indStart+1, indEnd
                CALL GET_COMMAND_ARGUMENT( iArg,  arg )
                jArg = jArg + 1
                this%words(jArg) = trim(arg)
            end do
        end if

    end if


!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!


!! -------------------------------------------------------------------------- !!
Subroutine CheckAndCleanKey_typCommandArgument(this, key, cleanKey, isCorrect)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typCommandArgument), intent(in) :: this
    Character(len=*), intent(in)          :: key
    Character(len=:), Allocatable         :: cleanKey
    Logical                               :: isCorrect
!! -------------------------------------------------------------------------- !!
    Integer                       :: ii, nCh, jj
    Character                     :: tmpChar
    Character(len=:), Allocatable :: savedKey

    cleanKey  = ""
    isCorrect = .FALSE.
    If ( len_trim(key).LE.0 ) Return

    savedKey = trim(key)
    cleanKey = savedKey

    nCh = len(savedKey)

    jj = 0
    do ii = 1, nCh
        tmpChar = savedKey(ii:ii)
        if ( (tmpChar>='a').AND.(tmpChar<='z') ) then
            jj = jj+1
            cleanKey(jj:jj) = tmpChar
        else if ( (tmpChar>='A').AND.(tmpChar<='Z') ) then
            jj = jj+1
            cleanKey(jj:jj) = tmpChar
        else
            isCorrect = .FALSE.
            Return
        end if
    end do

    If (Allocated(savedKey)) Deallocate(savedKey)

    isCorrect = .TRUE.

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine Destroy_typCommandArgument(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typCommandArgument), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Integer :: nArg, iArg

    If ( Allocated(this%key) ) Deallocate( this%key )
    this%singleKey = " "

    If ( Allocated(this%description%raw) ) Deallocate(this%description%raw)
    If ( Allocated(this%words) ) then
        nArg = size( this%words )
        do iArg = 1, nArg
            If ( Allocated(this%words(iArg)%raw) ) Deallocate( this%words(iArg)%raw )
        end do
    end if

    this%nWord          = 0
    this%isRequiredKey  = .FALSE.
    this%nRequiredWord  = 0

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine Copy_typCommandArgument(to, from)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typCommandArgument), intent(inout) :: to
    Class(typCommandArgument), intent(in)    :: from
!! -------------------------------------------------------------------------- !!
    Integer :: nWord, iWord

    !!... Destroy
    Call to%Destroy()

    If ( Allocated(from%key) ) to%key = from%key
    to%isSingleKeyGiven = from%isSingleKeyGiven
    to%singleKey        = from%singleKey
    to%description      = from%description%Chars()

    to%isRequiredKey = from%isRequiredKey
    to%isFound       = from%isFound

    to%nRequiredWord = from%nRequiredWord
    to%nWord         = from%nWord

    If ( Allocated(from%words) ) then
        nWord = size( from%words )
        Allocate( to%words(nWord) )
        do iWord = 1, nWord
            If ( Allocated(from%words(iWord)%raw) ) Then
                to%words(iWord)%raw = from%words(iWord)%raw
            End if
        end do
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
