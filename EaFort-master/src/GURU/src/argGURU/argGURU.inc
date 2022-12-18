!! -------------------------------------------------------------------------- !!
Subroutine ARG_Initialize_typGURU( &
    &   this, projectName, description, authors, date, version)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(inout)          :: this
    Character(len=*), intent(in)           :: projectName
    Character(len=*), intent(in), Optional :: description
    Character(len=*), intent(in), Optional :: authors
    Character(len=*), intent(in), Optional :: date
    Character(len=*), intent(in), Optional :: version
!! -------------------------------------------------------------------------- !!

    if ( len_trim(projectName).LE.0 ) then
        Call WritePrefixMessage( &
        &   prefix      = PREIX_WARN,                                       &
        &   prefixColor = COLOR_WARN,                                       &
        &   head        = "ARG_Initialize_typGURU",                         &
        &   msg         = "The length of 'projectName' is less than 1.",    &
        &   isColor     = this%isColor_,                                    &
        &   isStop      = .TRUE.                                            )
    end if

    this%projectName = projectName

    if ( present(description) ) this%description = description
    if ( present(authors) )     this%authors     = authors
    if ( present(date) )        this%date        = date
    if ( present(version) )     this%version     = version

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_IsFoundKey_typGURU( this, key, isFound, ikey )
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(in)     :: this
    Character(len=*), intent(in)   :: key
    Logical, intent(out)           :: isFound
    Integer, intent(out), optional :: ikey
!! -------------------------------------------------------------------------- !!
    Integer :: iArg, ikey_
    isFound = .FALSE.

    ikey_ = 0
    loopFind: do iArg = 1, this%nArguments
        if ( this%arguments(iArg)%key.EQ.key ) then
            if ( this%arguments(iArg)%isFound ) then
                ikey_  = iArg
                isFound = .TRUE.
                exit loopFind
            end if
        end if
    end do loopFind

    if (present(ikey)) ikey = ikey_

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function ARG_GetNKey_typGURU(this) result( nKey )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in) :: this
    Integer :: nKey
!! -------------------------------------------------------------------------- !!
    nKey = this%nArguments
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetKey_typGURU( this, ikey, key )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)    :: this
    Integer, intent(in)           :: ikey
    Character(len=:), Allocatable :: key
!! -------------------------------------------------------------------------- !!
    !!... Check index
    if ( (ikey.le.0).or.(ikey.gt.this%nArguments) ) then
        Call this%Error( &
        &   msg      = "Given 'ikey' is less than 0 or larger than the number of arguments.", &
        &   head     = "ARG_GetKey_typGURU", &
        &   value    = ikey, &
        &   refValue = this%nArguments )
    end if
    key = this%arguments(ikey)%key
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function ARG_GetNWord_Key_typGURU(this, key) result( nWord )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)   :: this
    Character(len=*), intent(in) :: key
    Integer                      :: nWord
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Integer :: ikey

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    if (isFound_) then
        nWord = this%GetNWord_iKey(ikey)
    else
        nWord = 0
        Call this%Error( &
        &   msg      = "Given 'key' is not found.", &
        &   head     = "ARG_GetNWord_Key_typGURU", &
        &   value    = key )
    endif
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function ARG_GetNWord_iKey_typGURU(this, ikey) result( nWord )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in) :: this
    Integer, intent(in)        :: ikey
    Integer                    :: nWord
!! -------------------------------------------------------------------------- !!
    !!... Check index
    if ( (ikey.le.0).or.(ikey.gt.this%nArguments) ) then
        Call this%Error( &
        &   msg      = "Given 'ikey' is less than 0 or larger than the number of arguments.", &
        &   head     = "ARG_GetKey_typGURU", &
        &   value    = ikey, &
        &   refValue = this%nArguments )
    end if
    !!... Return nWord
    nWord = this%arguments(ikey)%nWord
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgInt_typGURU(this, key, value, index, isError)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)    :: this
    Character(len=*), intent(in)  :: key
    Integer, intent(out)          :: value
    Integer, intent(in), optional :: index
    Logical, optional             :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_, isError_
    Integer :: ikey, index_

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    isError_ = .TRUE.
    index_   = 1
    If ( isFound_ ) then
        if ( present(index) ) index_ = index

        If ( (this%arguments(ikey)%nWord.ge.1).and.&
        &    (index_.le.this%arguments(ikey)%nWord) ) then

            Call GetIntFromChar( &
            &   char    = this%arguments(ikey)%words(index_)%raw , &
            &   value   = value ,&
            &   isError = isError_ )

        endif
    end if

    if (isError_) value   = 0

    if ( present(isError) ) then
        isError = isError_
    else
        Call this%Error( &
        &   msg      = "Failed to get integer.", &
        &   head     = "ARG_GetArgInt_typGURU", &
        &   value    = key )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgReal_typGURU(this, key, value, index, isError)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)    :: this
    Character(len=*), intent(in)  :: key
    Real(RP), intent(out)         :: value
    Integer, intent(in), optional :: index
    Logical, optional             :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_, isError_
    Integer :: ikey, index_

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    isError_ = .TRUE.
    index_   = 1
    If ( isFound_ ) then
        if ( present(index) ) index_ = index

        If ( (this%arguments(ikey)%nWord.ge.1).and.&
        &    (index_.le.this%arguments(ikey)%nWord) ) then

            Call GetRealFromChar( &
            &   char    = this%arguments(ikey)%words(index_)%raw , &
            &   value   = value ,   &
            &   isError = isError_  )

        endif
    end if

    if (isError_) value = 0.D0

    if ( present(isError) ) then
        isError = isError_
    else
        Call this%Error( &
        &   msg      = "Failed to get double precision.", &
        &   head     = "ARG_GetArgReal_typGURU", &
        &   value    = key )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgLogical_typGURU(this, key, value, index, isError)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)    :: this
    Character(len=*), intent(in)  :: key
    Logical, intent(out)          :: value
    Integer, intent(in), optional :: index
    Logical, optional             :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_, isError_
    Integer :: ikey, index_

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    isError_ = .TRUE.
    index_   = 1
    If ( isFound_ ) then
        if ( present(index) ) index_ = index

        If ( (this%arguments(ikey)%nWord.ge.1).and.&
        &    (index_.le.this%arguments(ikey)%nWord) ) then

            Call GetLogicalFromChar( &
            &   char    = this%arguments(ikey)%words(index_)%raw , &
            &   value   = value ,   &
            &   isError = isError_  )

        endif
    end if

    if (isError_) value = .FALSE.

    if ( present(isError) ) then
        isError = isError_
    else
        Call this%Error( &
        &   msg      = "Failed to get logical.", &
        &   head     = "ARG_GetArgLogical_typGURU", &
        &   value    = key )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgChar_typGURU(this, key, value, index, isError)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)    :: this
    Character(len=*), intent(in)  :: key
    Character(len=:), Allocatable, intent(out) :: value
    Integer, intent(in), optional :: index
    Logical, optional             :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_, isError_
    Integer :: ikey, index_

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    if (Allocated(value)) deallocate(value)

    isError_ = .TRUE.
    index_   = 1
    If ( isFound_ ) then
        if ( present(index) ) index_ = index

        If ( (this%arguments(ikey)%nWord.ge.1).and.&
        &    (index_.le.this%arguments(ikey)%nWord) ) then
            if (len(this%arguments(ikey)%words(index_)%raw).ge.1) then
                value    = this%arguments(ikey)%words(index_)%raw
                isError_ = .FALSE.
            end if
        endif

    end if

    if (isError_) value   = ""

    if ( present(isError) ) then
        isError = isError_
    else
        Call this%Error( &
        &   msg      = "Failed to get character.", &
        &   head     = "ARG_GetArgChar_typGURU", &
        &   value    = key )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgIntOrDefault_typGURU(this, key, value, default, index, isError)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)    :: this
    Character(len=*), intent(in)  :: key
    Integer, intent(out)          :: value
    Integer, intent(in)           :: default
    Integer, intent(in), optional :: index
    Logical, optional             :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Call this%GetArgInt( key, value, index, isError_ )
    if ( isError_ ) then
        value = default
        Call this%Debug( &
        &   msg      = "Failed to get value, 'default' is returned.", &
        &   head     = "ARG_GetArgIntOrDefault_typGURU", &
        &   value    = key, &
        &   refValue = default )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgRealOrDefault_typGURU(this, key, value, default, index, isError)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)    :: this
    Character(len=*), intent(in)  :: key
    Real(RP), intent(out)         :: value
    Real(RP), intent(in)          :: default
    Integer, intent(in), optional :: index
    Logical, optional             :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Call this%GetArgReal( key, value, index, isError_ )
    if ( isError_ ) then
        value = default
        Call this%Debug( &
        &   msg      = "Failed to get value, 'default' is returned.", &
        &   head     = "ARG_GetArgRealOrDefault_typGURU", &
        &   value    = key, &
        &   refValue = default )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgLogicalOrDefault_typGURU(this, key, value, default, index, isError)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)    :: this
    Character(len=*), intent(in)  :: key
    Logical, intent(out)          :: value
    Logical, intent(in)           :: default
    Integer, intent(in), optional :: index
    Logical, optional             :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Call this%GetArgLogical( key, value, index, isError_ )
    if ( isError_ ) then
        value = default
        Call this%Debug( &
        &   msg      = "Failed to get value, 'default' is returned.", &
        &   head     = "ARG_GetArgLogicalOrDefault_typGURU", &
        &   value    = key, &
        &   refValue = default )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgCharOrDefault_typGURU(this, key, value, default, index, isError)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                 :: this
    Character(len=*), intent(in)               :: key
    Character(len=:), allocatable, intent(out) :: value
    Character(len=*), intent(in)               :: default
    Integer, intent(in), optional :: index
    Logical, optional             :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Call this%GetArgChar( key, value, index, isError_ )
    if ( isError_ ) then
        value = default
        Call this%Debug( &
        &   msg      = "Failed to get value, 'default' is returned.", &
        &   head     = "ARG_GetArgLogicalOrDefault_typGURU", &
        &   value    = key, &
        &   refValue = default )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgIntVector_typGURU( this, key, value, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)     :: this
    Character(len=*), intent(in)   :: key
    Integer, allocatable           :: value(:)
    Logical, optional              :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_, isError_
    Integer :: iWord, ikey
    Integer :: elemValue

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    isError_ = .TRUE.
    if ( allocated(value) ) deallocate(value)
    If ( isFound_ ) then
        If ( this%arguments(ikey)%nWord.ge.1 ) then
            Allocate( value(this%arguments(ikey)%nWord) );  value(:) = 0
            loopWord: do iWord = 1, this%arguments(ikey)%nWord
                Call GetIntFromChar( &
                &   char    = this%arguments(ikey)%words(iWord)%raw, &
                &   value   = elemValue,    &
                &   isError = isError_      )
                if (isError_) exit loopWord
                value(iWord) = elemValue
            end do loopWord
        end if
    End if

    if (isError_) then
        if ( allocated(value) ) deallocate(value)
    end if

    if ( present(isError) ) then
        isError = isError_
    else
        Call this%Error( &
        &   msg      = "Failed to get integer vector.", &
        &   head     = "ARG_GetArgIntVector_typGURU", &
        &   value    = key )
    end if
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgRealVector_typGURU( this, key, value, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)     :: this
    Character(len=*), intent(in)   :: key
    Real(RP), allocatable          :: value(:)
    Logical, optional              :: isError
!! -------------------------------------------------------------------------- !!
    Logical  :: isFound_, isError_, isStop_
    Integer  :: iWord, ikey
    Real(RP) :: elemValue

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    isError_ = .TRUE.
    if ( allocated(value) ) deallocate(value)
    If ( isFound_ ) then
        If ( this%arguments(ikey)%nWord.ge.1 ) then
            Allocate( value(this%arguments(ikey)%nWord) );  value(:) = 0
            loopWord: do iWord = 1, this%arguments(ikey)%nWord
                Call GetRealFromChar( &
                &   char    = this%arguments(ikey)%words(iWord)%raw, &
                &   value   = elemValue,    &
                &   isError = isError_      )
                if (isError_) exit loopWord
                value(iWord) = elemValue
            end do loopWord
        end if
    End if

    if (isError_) then
        if ( allocated(value) ) deallocate(value)
    end if

    if ( present(isError) ) then
        isError = isError_
    else
        Call this%Error( &
        &   msg      = "Failed to get double precision vector.", &
        &   head     = "ARG_GetArgRealVector_typGURU", &
        &   value    = key )
    end if
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgLogicalVector_typGURU( this, key, value, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)     :: this
    Character(len=*), intent(in)   :: key
    Logical, allocatable           :: value(:)
    Logical, optional              :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_, isError_
    Integer :: iWord, ikey
    Logical :: elemValue

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    isError_ = .TRUE.
    if ( allocated(value) ) deallocate(value)
    If ( isFound_ ) then
        If ( this%arguments(ikey)%nWord.ge.1 ) then
            Allocate( value(this%arguments(ikey)%nWord) );  value(:) = .FALSE.
            loopWord: do iWord = 1, this%arguments(ikey)%nWord
                Call GetLogicalFromChar( &
                &   char    = this%arguments(ikey)%words(iWord)%raw, &
                &   value   = elemValue,    &
                &   isError = isError_      )
                if (isError_) exit loopWord
                value(iWord) = elemValue
            end do loopWord
        end if
    End if

    if (isError_) then
        if ( allocated(value) ) deallocate(value)
    end if

    if ( present(isError) ) then
        isError = isError_
    else
        Call this%Error( &
        &   msg      = "Failed to get logical vector.", &
        &   head     = "ARG_GetArgLogicalVector_typGURU", &
        &   value    = key )
    end if
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgCharVector_typGURU( this, key, value, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                  :: this
    Character(len=*), intent(in)                :: key
    Character(len=:), allocatable, dimension(:) :: value
    Logical, optional                           :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_, isError_
    Integer :: iWord, ikey, nCharMax, nChar
    Character(len=:), allocatable :: elemValue

    !!... Check
    Call this%IsFoundKey( key = key, isFound = isFound_, ikey = ikey )

    isError_ = .FALSE.
    if ( allocated(value) ) deallocate(value)
    If ( isFound_ ) then
        If ( this%arguments(ikey)%nWord.ge.1 ) then

            !!... Find nMax
            nCharMax = 0
            loopFindMaxChar: do iWord = 1,this%arguments(ikey)%nWord
                nChar = this%arguments(ikey)%words(iWord)%Len()

                if (nChar.le.0) then
                    isError_ = .TRUE.
                    exit loopFindMaxChar
                end if
                If ( nCharMax.le.nChar ) then
                    nCharMax = nChar
                End If
            end do loopFindMaxChar

            if (.NOT.isError_) then
                Allocate( character(nCharMax) :: value(this%arguments(ikey)%nWord) )
                loopWord: do iWord = 1, this%arguments(ikey)%nWord
                    value(iWord) = this%arguments(ikey)%words(iWord)%raw
                end do loopWord
            end if
        end if
    End if

    if ( present(isError) ) then
        isError = isError_
    else
        Call this%Error( &
        &   msg      = "Failed to get character vector.", &
        &   head     = "ARG_GetArgCharVector_typGURU", &
        &   value    = key )
    end if
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!


!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgIntVectorOrDefault_typGURU( this, key, value, default, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)        :: this
    Character(len=*), intent(in)      :: key
    Integer, allocatable              :: value(:)
    Integer, intent(in), dimension(:) :: default
    Logical, optional                 :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Integer :: nElem
    Call this%GetArgIntVector( key, value, isError_ )
    If ( isError_ ) then
        if ( Allocated(value) ) Deallocate(value)
        nElem = size(default)
        if (nElem.ge.1) Allocate( value, source = default )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgRealVectorOrDefault_typGURU( this, key, value, default, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)         :: this
    Character(len=*), intent(in)       :: key
    Real(RP), allocatable              :: value(:)
    Real(RP), intent(in), dimension(:) :: default
    Logical, optional                  :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Integer :: nElem
    Call this%GetArgRealVector( key, value, isError_ )
    If ( isError_ ) then
        if ( Allocated(value) ) Deallocate(value)
        nElem = size(default)
        if (nElem.ge.1) Allocate( value, source = default )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgLogicalVectorOrDefault_typGURU( this, key, value, default, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)        :: this
    Character(len=*), intent(in)      :: key
    Logical, allocatable              :: value(:)
    Logical, intent(in), dimension(:) :: default
    Logical, optional                 :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Integer :: nElem
    Call this%GetArgLogicalVector( key, value, isError_ )
    If ( isError_ ) then
        if ( Allocated(value) ) Deallocate(value)
        nElem = size(default)
        if (nElem.ge.1) Allocate( value, source = default )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_GetArgCharVectorOrDefault_typGURU( this, key, value, default, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                  :: this
    Character(len=*), intent(in)                :: key
    Character(len=:), allocatable, dimension(:) :: value
    Character(len=:), allocatable, dimension(:) :: default
    Logical, optional                           :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_
    Integer :: nElem
    Call this%GetArgCharVector( key, value, isError_ )
    If ( isError_ ) then
         if ( Allocated(value) ) Deallocate(value)
         ! nElem = size(default)
         ! if (nElem.ge.1) Allocate( value, source = default )
    end if
    if ( present(isError) ) isError = isError_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_Destroy_typGURU(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Integer :: nArg, iArg
    Call this%projectName%Free()
    Call this%description%Free()
    Call this%authors%Free()
    Call this%date%Free()
    Call this%version%Free()

    this%nArguments = 0
    If ( Allocated(this%arguments) ) then
        nArg = size(this%arguments)
        do iArg =1, nArg
            Call this%arguments(iArg)%Destroy()
        end do
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_UpdateAndPrint_typGURU( this )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Logical :: isFound

    !!... Update Key
    Call this%Update_ARG()

    !!... Print Description
    Call this%PrintDescription()
    if ( this%isLogFile ) Call this%PrintDescription( this%logUnit )

    Call this%IsFoundKey( key = "help", isFound = isFound )

    !!... If --help is given, print help message and stop
    If ( isFound ) then
        Call this%PrintHelp()
        if ( this%isLogFile ) Call this%PrintHelp( this%logUnit )
        Call this%Stop()
    end if

    !!... Print Given Key Information
    Call this%PrintGivenKey()
    if ( this%isLogFile ) Call this%PrintGivenKey( this%logUnit )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_PrintDescription_typGURU( this, ioUnit )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(in)   :: this
    Integer, optional            :: ioUnit
!! -------------------------------------------------------------------------- !!
    Logical :: isColor_
    Integer :: ioUnit_

    !!... Set Print Mode
    ioUnit_ = 6
    if ( present(ioUnit) ) ioUnit_ = ioUnit

    isColor_ = this%isColor_

    if (ioUnit_.NE.6) isColor_ = .FALSE.

    !!... Write Project Name
    blkWriteProjectName : Block
        Character(len=17) :: color1, color2
        color1 = ""
        color2 = ""
        If ( isColor_ ) then
            color1 = "Green"
            color2 = "Yellow"
        end if

        Write(ioUnit_, "(a)") ""
        Write(ioUnit_, "(a)") Colorize("Project", color1)// " : "//Colorize(this%projectName%Chars(), color2)
        Write(ioUnit_, "(a)") ""

    End Block blkWriteProjectName

    !!... Write Description
    blkWriteDescription : Block
        Type(string), Allocatable :: multipleLines(:)
        Integer :: nStr, iStr
        Character(len=17) :: color1

        If ( Allocated(this%description%raw) ) then

            !!... Split the description into the multiple lines and print
            Call this%description%Split( multipleLines, lineSep )
            nStr = size( multipleLines )

            color1 = ""
            If ( isColor_ ) color1 = "CYAN"
            Write(ioUnit_, "(a)") INDENT//Colorize("Description", color1)
            Write(ioUnit_, "(a)") ""

            If ( nStr.GE.1 ) then
                do iStr = 1, nStr
                    Write(ioUnit_, "(a)") INDENT2//multipleLines(iStr)%Chars()
                end do
            end if
            Write(ioUnit_, "(a)") ""

        end if

    End Block blkWriteDescription

    !!... Write Authors
    blkWriteAuthors : Block
        Type(string), Allocatable :: multipleLines(:)
        Integer :: nStr, iStr
        Character(len=17) :: color1

        If ( Allocated(this%authors%raw) ) then

            !!... Split the description into the multiple lines and print
            Call this%authors%Split( multipleLines, lineSep )
            nStr = size( multipleLines )

            color1 = ""
            If ( isColor_ ) color1 = "CYAN"
            Write(ioUnit_, "(a)") INDENT//Colorize("Authors", color1)
            Write(ioUnit_, "(a)") ""

            If ( nStr.GE.1 ) then
                do iStr = 1, nStr
                    Write(ioUnit_, "(a)") INDENT2//multipleLines(iStr)%Chars()
                end do
            end if
            Write(ioUnit_, "(a)") ""

        end if

    End Block blkWriteAuthors

    !!... Write Version & Date
    blkWriteVersionDate : Block
        Character(len=17) :: color1

        color1 = ""
        If ( isColor_ ) color1 = "CYAN"

        If ( Allocated(this%version%raw) ) then
            Write(ioUnit_, "(a)") INDENT//Colorize("Version", color1)//" : "//this%version%Chars()
            Write(ioUnit_, "(a)") ""
        End if

        If ( Allocated(this%date%raw) ) then
            Write(ioUnit_, "(a)") INDENT//Colorize("Date   ", color1)//" : "//this%date%Chars()
            Write(ioUnit_, "(a)") ""
        end if

    End Block blkWriteVersionDate

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_PrintGivenKey_typGURU( this, ioUnit )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(in) :: this
    Integer, optional          :: ioUnit
!! -------------------------------------------------------------------------- !!
    Logical :: isColor_
    Integer :: ioUnit_

    !!... Set Print Mode
    ioUnit_ = 6
    if ( present(ioUnit) ) ioUnit_ = ioUnit

    isColor_ = this%isColor_
    if (ioUnit_.NE.6) isColor_ = .FALSE.

    !!... Write Required Key Information
    blkWriteRequiredKey : Block
        Integer           :: iArg, nArg
        Character(len=17) :: color

        If ( this%nArguments.GE.1 ) then

            nArg = 0
            do iArg = 1, this%nArguments
                If ( this%arguments(iArg)%isRequiredKey ) nArg = nArg + 1
            End do

            if ( nArg.GE.1 ) then

                color = ""
                If ( isColor_ ) color = "MAGENTA_INTENSE"
                Write(ioUnit_, "(a)") INDENT//Colorize("Required", color)
                Write(ioUnit_, "(a)") ""

                do iArg = 1, this%nArguments
                    If ( this%arguments(iArg)%isRequiredKey ) then
                        Call this%arguments(iArg)%PrintInfo(ioUnit, isColor_)
                    end if
                end do
            end if

        End If

    End Block blkWriteRequiredKey

    !!... Write Optional Key Information
    blkWriteOptionalKey : Block
        Integer :: iArg, nArg
        Character(len=17) :: color

        if ( this%nArguments.GE.1 ) then

            nArg = 0
            do iArg = 1, this%nArguments
                If ( .NOT.this%arguments(iArg)%isRequiredKey ) nArg = nArg + 1
            End do

            if ( nArg.GE.1 ) then

                color = ""
                If ( isColor_ ) color = "MAGENTA_INTENSE"
                Write(ioUnit_, "(a)") INDENT//Colorize("Optional", color)
                Write(ioUnit_, "(a)") ""

                do iArg = 1, this%nArguments
                    If ( .NOT.this%arguments(iArg)%isRequiredKey ) then
                        Call this%arguments(iArg)%PrintInfo(ioUnit, isColor_)
                    end if
                end do
            end if

        end if

    End Block blkWriteOptionalKey

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_Update_ARG_typGURU(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Integer :: iArg

    do iArg = 1, this%nArguments
        Call this%arguments(iArg)%Update()
    end do

    do iArg = 1, this%nArguments
        if ( this%arguments(iArg)%isRequiredKey.AND.&
        &    (.NOT.this%arguments(iArg)%isFound) ) then
            Call GURU%Error( &
            &   msg   = "Required key is not given.", &
            &   head  = "Update_typArgGURU", &
            &   value = this%arguments(iArg)%key )
        end if
    end do

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_AddKey_typGURU( &
    &   this, key, singleKey, description, isRequiredKey, nRequiredWord )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(inout)         :: this
    Character(len=*), intent(in)             :: key
    Character(len=*), Optional, intent(in)   :: singleKey
    Character(len=*), Optional, intent(in)   :: description
    Logical, intent(in), Optional            :: isRequiredKey
    Integer, intent(in), Optional            :: nRequiredWord
!! -------------------------------------------------------------------------- !!
    Logical :: isRequiredKey_
    Integer :: iArg, nPreviousArg
    Type(typCommandArgument), allocatable :: tmpArguments(:)

    isRequiredKey_ = .TRUE.
    If (present(isRequiredKey)) isRequiredKey_ = isRequiredKey

    !!... Check key is already exist

    !!... Required Key
    nPreviousArg  = this%nArguments
    If ( nPreviousArg.GE.1 ) then
        do iArg = 1, nPreviousArg

            !!... Check key already exists
            if ( this%arguments(iArg)%key.EQ.key ) then
                Call WritePrefixMessage( &
                &   prefix      = PREIX_ERROR,                              &
                &   prefixColor = COLOR_ERROR,                              &
                &   head        = "ARG_AddKey_typGURU",                     &
                &   msg         = "Given 'key' exist already.",             &
                &   value       = trim(singleKey),                          &
                &   isColor     = this%isColor_,                            &
                &   isStop      = .TRUE.                                    )
            end if

            !!... Check singleKey already exists
            If ( Present(singleKey) ) then
                if ( this%arguments(iArg)%singleKey.EQ.singleKey ) then
                    Call WritePrefixMessage( &
                    &   prefix      = PREIX_ERROR,                              &
                    &   prefixColor = COLOR_ERROR,                              &
                    &   head        = "ARG_AddKey_typGURU",                     &
                    &   msg         = "Given 'singleKey' exist already.",       &
                    &   value       = trim(singleKey),                          &
                    &   isColor     = this%isColor_,                            &
                    &   isStop      = .TRUE.                                    )
                end if
            End if

        end do
    end if

    !!... Copy into temporal array
    If ( nPreviousArg.GE.1 ) then
        Allocate( tmpArguments(nPreviousArg) )
        do iArg = 1, nPreviousArg
            tmpArguments(iArg) = this%arguments(iArg)
            Call this%arguments(iArg)%Destroy()
        end do
        Deallocate( this%arguments )
    End if

    !!... Set a new set
    this%nArguments = this%nArguments + 1
    Allocate( this%arguments(this%nArguments) )

    !!... Copy previous keys
    Do iArg = 1, nPreviousArg
        this%arguments(iArg) = tmpArguments(iArg)
    End Do

    !!... Delete temporal keys
    If ( nPreviousArg.GE.1 ) then
        do iArg = 1, nPreviousArg
            Call tmpArguments(iArg)%Destroy()
        End do
        Deallocate( tmpArguments )
    End If

    !!... Add new key
    Call this%arguments(this%nArguments)%Initialize( &
    &   key           = key,              &
    &   singleKey     = singleKey,        &
    &   description   = description,      &
    &   isRequiredKey = isRequiredKey,    &
    &   nRequiredWord = nRequiredWord     )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine ARG_PrintHelp_typGURU(this, ioUnit)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(in) :: this
    Integer, optional          :: ioUnit
!! -------------------------------------------------------------------------- !!
    Logical :: isColor_
    Integer :: ioUnit_, iArg

    !!... Set Print Mode
    ioUnit_ = 6
    isColor_ = this%isColor_
    if ( present(ioUnit) ) then
        if (ioUnit.EQ.6) then
            isColor_ = this%isColor_
        else
            isColor_ = .FALSE.
        end if
        ioUnit_ = ioUnit
    end if

    blkWriteRequiredKey: Block
        Integer :: iArg, nArg
        Character(len=17) :: color1

        If ( this%nArguments.GE.1 ) Then

            nArg = 0
            do iArg = 1, this%nArguments
                If ( this%arguments(iArg)%isRequiredKey ) nArg = nArg + 1
            End do

            if ( nArg.GE.1 ) then

                color1 = ""
                If ( isColor_ ) color1 = "GREEN"
                Write(ioUnit_, "(a)") INDENT//Colorize("Required", color1)
                Write(ioUnit_, "(a)") ""

                do iArg = 1, this%nArguments
                    If ( this%arguments(iArg)%isRequiredKey ) then
                        Call this%arguments(iArg)%PrintHelp( ioUnit, isColor_ )
                    End if
                end do

            end if

        End if

    End Block blkWriteRequiredKey

    blkWriteOptionalKey: Block
        Integer :: iArg, nArg
        Character(len=17) :: color1

        If ( this%nArguments.GE.1 ) Then

            nArg = 0
            do iArg = 1, this%nArguments
                If ( .NOT.this%arguments(iArg)%isRequiredKey ) nArg = nArg + 1
            End do

            color1 = ""
            If ( isColor_ ) color1 = "GREEN"
            Write(ioUnit_, "(a)") INDENT//Colorize("Optional", color1)
            Write(ioUnit_, "(a)") ""

            do iArg = 1, this%nArguments
                If ( .NOT.this%arguments(iArg)%isRequiredKey ) then
                    Call this%arguments(iArg)%PrintHelp( ioUnit, isColor_ )
                End if
            end do

        End if

    End Block blkWriteOptionalKey

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
