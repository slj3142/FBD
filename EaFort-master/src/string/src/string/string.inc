!!... Get Integer
Subroutine GetInt_typString(this, value, isError)
    Implicit None
    Class(typString), intent(in) :: this
    Integer(IP)                  :: value
    Logical, Optional            :: isError
    !!... Local Variables
    Real(RP)                     :: numValue
    integer                      :: ios
    Logical                      :: isError_

    isError_ = .FALSE.
    read( this%raw, *, iostat = ios ) numValue
    value = idnint(numValue)

    if ( ios.NE.0 ) isError_ = .TRUE.

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%Error( &
        &   msg   = "Conversion to integer variable fails.", &
        &   head  = "GetInt_typString", &
        &   value = this%raw )
    end if

End Subroutine

!!... Get Real
Subroutine GetReal_typString(this, value, isError)
    Implicit None
    Class(typString), intent(in) :: this
    Real(RP)                     :: value
    Logical, Optional            :: isError
    !!... Local Variables
    integer :: ios
    Logical :: isError_

    isError_ = .FALSE.
    read( this%raw, *, iostat = ios ) value
    if ( ios.NE.0 ) isError_ = .TRUE.

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%Error( &
        &   msg   = "Conversion to real variable fails.", &
        &   head  = "GetReal_typString", &
        &   value = this%raw )
    end if

End Subroutine

!!... Get Logical
Subroutine GetLogical_typString(this, value, isError)
    Implicit None
    Class(typString), intent(in) :: this
    Logical                      :: value
    Logical, Optional            :: isError
    !!... Local Variables
    Logical :: isError_

    Call GetLogicalFromChar( &
    &   char = this%raw, value = value, isError = isError_)

    if ( present(isError) ) isError = isError_

    if ( (isError_).AND.(.NOT.present(isError)) ) then
        Call GURU%Error( &
        &   msg   = "Conversion to logical variable fails.", &
        &   head  = "GetLogical_typString", &
        &   value = this%raw )
    end if

End Subroutine

Subroutine GetChar_typString(this, value, isError)
    Implicit None
    Class(typString), intent(in)           :: this
    Character(kind=CK, len=:), allocatable :: value
    Logical, Optional                      :: isError
    Logical :: isError_

    value = ''

    isError_   = .FALSE.
    if ( allocated(this%raw).AND.(len(this%raw).ge.1) ) then
        value = this%raw
    else
        isError_ = .TRUE.
    endif

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%Error( &
        &   msg   = "Conversion to char variable fails.", &
        &   head  = "GetChar_typString", &
        &   value = this%raw )
    end if

End Subroutine

!!... Get Integer
Subroutine GetIntOrDefault_typString(this, value, default, isError)
    Implicit None
    Class(typString), intent(in) :: this
    Integer, intent(out)         :: value
    Integer, intent(in)          :: default
    Logical, Optional            :: isError
    !!... Local Variables
    Logical                      :: isError_

    Call this%GetInt( value, isError_ )
    if ( isError_ ) then
        value = default
        Call GURU%Debug( &
        &   msg      = "Conversion fails. 'default' is used.", &
        &   head     = "GetIntOrDefault_typString", &
        &   value    = this%raw, &
        &   refValue = default )
    end if
    if (present(isError)) isError = isError_

End Subroutine

!!... Get Real
Subroutine GetRealOrDefault_typString(this, value, default, isError)
    Implicit None
    Class(typString), intent(in) :: this
    Real(RP), intent(out)        :: value
    Real(RP), intent(in)         :: default
    Logical, Optional            :: isError
    !!... Local Variables
    Logical                      :: isError_

    Call this%GetReal( value, isError_ )
    if ( isError_ ) then
        value = default
        Call GURU%Debug( &
        &   msg      = "Conversion fails. 'default' is used.", &
        &   head     = "GetRealOrDefault_typString", &
        &   value    = this%raw, &
        &   refValue = default )
    end if

    if (present(isError)) isError = isError_

End Subroutine

!!... Get Logical
Subroutine GetLogicalOrDefault_typString(this, value, default, isError)
    Implicit None
    Class(typString), intent(in) :: this
    Logical, intent(out)         :: value
    Logical, intent(in)          :: default
    Logical, Optional            :: isError
    !!... Local Variables
    Logical                      :: isError_

    Call this%GetLogical( value, isError_ )

    if ( isError_ ) then
        value = default
        Call GURU%Debug( &
        &   msg      = "Conversion fails. 'default' is used.", &
        &   head     = "GetLogicalOrDefault_typString", &
        &   value    = this%raw, &
        &   refValue = default )
    end if

    if ( Present(isError) ) isError = isError_

End Subroutine

Subroutine GetCharOrDefault_typString(this, value, default, isError)
    Implicit None
    Class(typString), intent(in)           :: this
    Character(kind=CK, len=:), allocatable :: value
    Character(len=*), intent(in)           :: default
    Logical, Optional                      :: isError
    !!... Local Variables
    Logical                      :: isError_

    Call this%GetChar( value, isError_ )
    if ( isError_ ) then
        value = default
        Call GURU%Debug( &
        &   msg      = "Conversion fails. 'default' is used.", &
        &   head     = "GetCharOrDefault_typString", &
        &   value    = this%raw, &
        &   refValue = default )
    end if

    If ( Present(isError) ) isError = isError_

End Subroutine

Subroutine Split_typString(this, vector, sep )
    Implicit None
    Class(typString), intent(in)              :: this
    Type(typString), Allocatable, intent(out) :: vector(:)
    Character(kind=CK, len=*), Optional       :: sep
    !!... Local Variables
    Type(string), Allocatable :: stringVector(:)
    Integer :: nString, iString

    !!... Call Parent's Subroutine
    Call this%string%Split( tokens = stringVector, sep = sep )

    !!... Set vector
    nString = size( stringVector )
    if ( nString.GE.1 ) then
        Allocate( vector(nString) )
        do iString = 1, nString
            vector(iString) = stringVector(iString)%raw
        end do
    end if

    !!... Destroy string
    If ( Allocated(stringVector) ) Then
        do iString = 1, nString
            If ( Allocated(stringVector(iString)%raw) ) then
                Deallocate( stringVector(iString)%raw )
            end if
        end do
        Deallocate( stringVector )
    end if

End Subroutine

Subroutine SplitSimple_typString(this, vector)
    Implicit None
    Class(typString), intent(in)              :: this
    Type(typString), Allocatable, intent(out) :: vector(:)
    !!... Local Variables
    Type(string)    :: replacedStrOrg
    Type(typString) :: replacedStr

    !!... Replace ',' into ' ' (blank)
    replacedStrOrg = this%string%Replace( old = ",", new = " ")
    replacedStr    = replacedStrOrg%raw

    !!... Split
    Call replacedStr%Split_typString( vector = vector )

    If ( Allocated(replacedStrOrg%raw) ) Deallocate( replacedStrOrg%raw )
    If ( Allocated(replacedStr%raw) ) Deallocate( replacedStr%raw )

End Subroutine

Subroutine GetIntVector_typString( this, value, isError )
    Implicit None
    Class(typString), intent(in) :: this
    Integer, Allocatable         :: value(:)
    Logical, Optional            :: isError
    !!... Local Variables
    Logical :: isError_

    !!... Split into string vector
    Call GetIntVectorFromChar( &
    &   char = this%raw, value = value, isError = isError_)

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%Error( &
        &   msg      = "Conversion to integer vector fails.", &
        &   head     = "GetIntVector_typString", &
        &   value    = this%raw )
    end if

End Subroutine

Subroutine GetRealVector_typString( this, value, isError )
    Implicit None
    Class(typString), intent(in) :: this
    Real(RP), Allocatable        :: value(:)
    Logical, Optional            :: isError
    !!... Local Variables
    Logical :: isError_

    !!... Split into string vector
    Call GetRealVectorFromChar( &
    &   char = this%raw, value = value, isError = isError_)

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%Error( &
        &   msg      = "Conversion to real vector fails.", &
        &   head     = "GetRealVector_typString", &
        &   value    = this%raw )
    end if

End Subroutine

Subroutine GetLogicalVector_typString( this, value, isError )
    Implicit None
    Class(typString), intent(in) :: this
    Logical, Allocatable         :: value(:)
    Logical, Optional            :: isError
    !!... Local Variables
    Logical :: isError_

    !!... Split into string vector
    Call GetLogicalVectorFromChar( &
    &   char = this%raw, value = value, isError = isError_)

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%Error( &
        &   msg      = "Conversion to logical vector fails.", &
        &   head     = "GetLogicalVector_typString", &
        &   value    = this%raw )
    end if

End Subroutine

Subroutine GetStringVector_typString( this, value, isError )
    Implicit None
    Class(typString), intent(in) :: this
    Type(typString), Allocatable :: value(:)
    Logical, Optional            :: isError
    !!... Local Variables
    Logical :: isError_

    !!... Split into string vector
    ! Call GetLogicalVectorFromChar( &
    ! &   char = this%raw, value = value, isError = isError_)
    isError_ = .FALSE.
    if ( .NOT.Allocated(this%raw) ) then
        isError_ = .TRUE.
    else if (len_trim(adjustl(this%raw)).le.0 ) then
        isError_ = .TRUE.
    end if

    Call this%SplitSimple( vector = value )

    if ( present(isError) ) isError = isError_

    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%Error( &
        &   msg      = "Conversion to string vector fails.", &
        &   head     = "GetStringVector_typString", &
        &   value    = this%raw )
    end if

End Subroutine

Subroutine GetIntVectorOrDefault_typString( this, value, default, isError )
    Implicit None
    Class(typString), intent(in)      :: this
    Integer, Allocatable, intent(out) :: value(:)
    Integer, Dimension(:), intent(in) :: default
    Logical, intent(out), Optional    :: isError
    !!... Local Variables
    Logical :: isError_

    !!... Split into string vector
    Call GetIntVectorFromCharOrDefault( &
    &   char = this%raw, value = value, refValue = default, isError = isError_)

    if ( present(isError) ) isError = isError_
    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%DebugSingleVector( &
        &   msg      = "Conversion fails. 'default' is used.", &
        &   head     = "GetIntVectorOrDefault_typString",      &
        &   value    = this%raw,                               &
        &   refValue = default                                 )
    end if

End Subroutine

Subroutine GetRealVectorOrDefault_typString( this, value, default, isError )
    Implicit None
    Class(typString), intent(in)       :: this
    Real(RP), Allocatable, intent(out) :: value(:)
    Real(RP), Dimension(:), intent(in) :: default
    Logical, intent(out), Optional     :: isError
    !!... Local Variables
    Logical :: isError_

    !!... Split into string vector
    Call GetRealVectorFromCharOrDefault( &
    &   char = this%raw, value = value, refValue = default, isError = isError_)

    if ( present(isError) ) isError = isError_
    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%DebugSingleVector( &
        &   msg      = "Conversion fails. 'default' is used.", &
        &   head     = "GetRealVectorOrDefault_typString",     &
        &   value    = this%raw,                               &
        &   refValue = default                                 )
    end if

End Subroutine

Subroutine GetLogicalVectorOrDefault_typString( this, value, default, isError )
    Implicit None
    Class(typString), intent(in)      :: this
    Logical, allocatable, intent(out) :: value(:)
    Logical, dimension(:), intent(in) :: default
    Logical, intent(out), Optional    :: isError
    !!... Local Variables
    Logical :: isError_

    !!... Split into string vector
    Call GetLogicalVectorFromCharOrDefault( &
    &   char = this%raw, value = value, refValue = default, isError = isError_)

    if ( present(isError) ) isError = isError_
    if ( isError_.AND.( .NOT.present(isError) ) ) Then
        Call GURU%DebugSingleVector( &
        &   msg      = "Conversion fails. 'default' is used.", &
        &   head     = "GetLogicalVectorOrDefault_typString",  &
        &   value    = this%raw,                               &
        &   refValue = default                                 )
    end if

End Subroutine

Subroutine GetStringVectorOrDefault_typString( this, value, default, isError )
    Implicit None
    Class(typString), intent(in)              :: this
    Type(typString), allocatable, intent(out) :: value(:)
    Type(typString), dimension(:), intent(in) :: default
    Logical, intent(out), Optional    :: isError
    !!... Local Variables
    Logical :: isError_
    Integer :: nArg, iArg

    Call this%GetStringVector( value = value, isError = isError_)

    if ( isError_ ) then
        if (Allocated(value)) then
            nArg = size(value)
            do iArg = 1, nArg
                Call value(iArg)%Free()
            end do
            deallocate(value)
        end if

        nArg = size(default)
        If (nArg.ge.1) then
            Allocate(value(nArg))
            do iArg = 1, nArg
                value(iArg) = default(iArg)
            end do
        else
            Call GURU%Error( &
            &   msg  = "The size of given 'default' is less than 1.", &
            &   head = "GetStringVectorOrDefault_typString")
        end if

    end if

    if (present(isError)) isError = isError_

End Subroutine

Subroutine Destroy_typString(this)
    Implicit None
    Class(typString), intent(inout) :: this
    Call this%Free()
End Subroutine
