!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetParent(json, parent)
    Implicit None
    type(typJSON), pointer :: json
    type(typJSON), pointer :: parent
!! -------------------------------------------------------------------------- !!
    !!... Local Variables
    character(len=:), Allocatable :: parentName
!! -------------------------------------------------------------------------- !!
    Call jsonCore%Get_Parent( json, parent )
    if ( associated(parent).EQV..FALSE. ) then
        Call jsonCore%info( p = json, name = parentName )
        Call GURU%Warn( &
            msg   = "Obtained parent is not associated.", &
            head  = "JSON_GetParent", &
            value = trim(parentName) )
    end if
    if (Allocated(parentName)) Deallocate(parentName)
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetChild(json, name, child, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: name
    type(typJSON), pointer       :: child
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    !!... Local Variables
    Logical          :: isFound_
    Character(len=5) :: info

    info = INFO_ERROR
    if ( present(isFound) ) info = INFO_DEBUG

    child => null()
    isFound_ = .FALSE.

    if ( Associated(json) ) then

        Call jsonCore%Get_Child( p = json, name = name, child = child, found = isFound_ )

        if (.NOT.isFound_) then
            Call GURU%Info( &
            &   info  = info, &
            &   msg   = "Failed to get child. A null point is returned.", &
            &   head  = "JSON_GetChild", &
            &   value = name )
            child => null()
        end if

    end if

    if ( present(isFound) ) then
        isFound = isFound_
    else
        if (.NOT.isFound_) Call GURU%Stop()
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetChildByIndex(json, index, child, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer :: json
    Integer, intent(in)    :: index
    type(typJSON), pointer :: child
    Logical, optional      :: isFound
!! -------------------------------------------------------------------------- !!
    !!... Local Variables
    Logical          :: isFound_
    Character(len=5) :: info

    info = INFO_ERROR
    if ( present(isFound) ) info = INFO_DEBUG

    child => null()
    isFound_ = .FALSE.

    if ( Associated(json) ) then
        Call jsonCore%Get_Child( p = json, idx = index, child = child, found = isFound_ )
        if (.NOT.isFound_) then
            Call GURU%Info( &
            &   info  = info, &
            &   msg   = "Failed to get child. A null point is returned.", &
            &   head  = "JSON_GetChild", &
            &   value = index )
            child => null()
        end if
    else

    end if

    if ( present(isFound) ) then
        isFound = isFound_
    else
        if (.NOT.isFound_) Call GURU%Stop()
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetChildIfFound(json, name, child, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: name
    type(typJSON), pointer       :: child
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    !!... Local Variables
    Logical          :: isFound_

    child => null()
    isFound_ = .FALSE.

    if ( Associated(json) ) then
        Call jsonCore%Get_Child( p = json, name = name, child = child, found = isFound_ )
        if (.NOT.isFound_) then
            Call GURU%Debug( &
            &   msg   = "Failed to get child. A null point is returned.", &
            &   head  = "JSON_GetChild", &
            &   value = name )
            child => null()
        end if
    end if

    if ( present(isFound) ) isFound = isFound_

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetChildByIndexIfFound(json, index, child, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer :: json
    Integer, intent(in)    :: index
    type(typJSON), pointer :: child
    Logical, optional      :: isFound
!! -------------------------------------------------------------------------- !!
    !!... Local Variables
    Logical          :: isFound_

    child => null()
    isFound_ = .FALSE.

    if ( Associated(json) ) then
        Call jsonCore%Get_Child( p = json, idx = index, child = child, found = isFound_ )
        if (.NOT.isFound_) then
            Call GURU%Debug( &
            &   msg   = "Failed to get child. A null point is returned.", &
            &   head  = "JSON_GetChild", &
            &   value = index )
            child => null()
        end if
    else

    end if

    if ( present(isFound) ) isFound = isFound_

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetENUM(json, enum)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer :: json
    integer,intent(out)    :: enum
!! -------------------------------------------------------------------------- !!
    Call jsonCore%info(p = json, var_type = enum)
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetInt( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSON), Pointer       :: json
    Character(len=*), intent(in) :: path
    Integer, intent(out)         :: value
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Call jsonCore%Get( json, trim(path), value, isFound_ )
    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetInt", &
        &   value = path )
    End if
    if ( present(isFound) ) isFound = isFound_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetReal( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSON), Pointer       :: json
    Character(len=*), intent(in) :: path
    Real(RP), intent(out)         :: value
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Call jsonCore%Get( json, trim(path), value, isFound_ )
    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetReal", &
        &   value = path )
    End if
    if ( present(isFound) ) isFound = isFound_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetLogical( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSON), Pointer       :: json
    Character(len=*), intent(in) :: path
    Logical, intent(out)         :: value
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Call jsonCore%Get( json, trim(path), value, isFound_ )
    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetLogical", &
        &   value = path )
    End if
    if ( present(isFound) ) isFound = isFound_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetChar( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSON), Pointer                     :: json
    Character(len=*), intent(in)               :: path
    Character(len=:), Allocatable, intent(out) :: value
    Logical, optional                          :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Call jsonCore%Get( json, trim(path), value, isFound_ )
    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetChar", &
        &   value = path )
    End if
    if ( present(isFound) ) isFound = isFound_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetString( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSON), Pointer       :: json
    Character(len=*), intent(in) :: path
    Type(typString), intent(out) :: value
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: tmpChar
    Logical :: isFound_
    Call jsonCore%Get( json, trim(path), tmpChar, isFound_ )
    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetString", &
        &   value = path )
    End if

    Call value%Destroy()
    If (isFound_) value = tmpChar

    if ( present(isFound) ) isFound = isFound_
    If ( Allocated(tmpChar) ) Deallocate(tmpChar)
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetIntVector( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Integer, allocatable         :: value(:)
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    If ( Allocated(value) ) Deallocate(value)
    Call jsonCore%Get( json, trim(path), value, isFound_)
    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetIntVector", &
        &   value = path )
    End if
    if ( present(isFound) ) isFound = isFound_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetRealVector( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Real(RP), allocatable        :: value(:)
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    If ( Allocated(value) ) Deallocate(value)
    Call jsonCore%Get( json, trim(path), value, isFound_)
    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetRealVector", &
        &   value = path )
    End if
    if ( present(isFound) ) isFound = isFound_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetLogicalVector( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Logical, allocatable         :: value(:)
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    If ( Allocated(value) ) Deallocate(value)
    Call jsonCore%Get( json, trim(path), value, isFound_)
    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetLogicalVector", &
        &   value = path )
    End if
    if ( present(isFound) ) isFound = isFound_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetCharVector( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer                    :: json
    Character(len=*), intent(in)              :: path
    character(len=:),dimension(:),allocatable :: value
    Logical, optional                         :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    integer, dimension(:), allocatable  :: ilen
    Integer :: nStr

    if (allocated(ilen)) deallocate(ilen)
    if (allocated(value)) deallocate(value)

    Call jsonCore%Get( json, path, value, ilen, isFound_ )

    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetCharVector", &
        &   value = path )
    End if

    If ( isFound_ ) then
        nStr = size( value )
        If (nStr.le.0) then
            Call GURU%Error( &
            &   msg   = "The size of character vector of given 'path' is less than 1.", &
            &   head  = "JSON_GetCharVector", &
            &   value = path )
        End if
    end if

    if (allocated(ilen)) deallocate(ilen)

    if ( present(isFound) ) isFound = isFound_

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_GetStringVector( json, path, value, isFound )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Type(typString), allocatable :: value(:)
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    integer, dimension(:), allocatable                       :: ilen
    character(kind=json_CK,len=:), allocatable, dimension(:) :: tmpCharVector
    Integer :: nStr, iStr

    !!... Get Character Vector
    if (allocated(ilen)) deallocate(ilen)
    if (allocated(value)) deallocate(value)
    Call jsonCore%Get( json, path, tmpCharVector, ilen, isFound_ )

    if ( (.NOT.isFound_).AND.(present(isFound).EQV..FALSE.) ) then
        Call GURU%Error( &
        &   msg   = "Failed to get value for given 'path'.", &
        &   head  = "JSON_GetStringVector", &
        &   value = path )
    End if

    If ( isFound_ ) then
        nStr = size( tmpCharVector )
        If (nStr.le.0) then
            Call GURU%Error( &
            &   msg   = "The size of character vector of given 'path'. is less than 1.", &
            &   head  = "JSON_GetStringVector", &
            &   value = path )
        End if

        if (allocated(value)) deallocate(value)
        allocate( value(nStr) )

        do iStr = 1, nStr
            value(istr) = tmpCharVector(iStr)
        end do

    end if

    if (allocated(ilen)) deallocate(ilen)

    if ( present(isFound) ) isFound = isFound_
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetIntOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Integer, intent(out)         :: value
    Integer, intent(in)          :: default
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Call JSON_GetInt( json = json, path = path, value = value, isFound = isFound_)
    if (.NOT.isFound_) then
        value = default
        Call GURU%Debug( &
        &   msg      = "Failed to get 'value'. 'default' is returned.", &
        &   head     = "JSON_GetIntOrDefault", &
        &   value    = path, &
        &   refValue = default )
    end if
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetRealOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Real(RP), intent(out)        :: value
    Real(RP), intent(in)         :: default
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Call JSON_GetReal( json = json, path = path, value = value, isFound = isFound_)
    if (.NOT.isFound_) then
        value = default
        Call GURU%Debug( &
        &   msg      = "Failed to get 'value'. 'default' is returned.", &
        &   head     = "JSON_GetRealOrDefault", &
        &   value    = path, &
        &   refValue = default )
    end if
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetLogicalOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Logical, intent(out)         :: value
    Logical, intent(in)          :: default
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Call JSON_GetLogical( json = json, path = path, value = value, isFound = isFound_)
    if (.NOT.isFound_) then
        value = default
        Call GURU%Debug( &
        &   msg      = "Failed to get 'value'. 'default' is returned.", &
        &   head     = "JSON_GetLogicalOrDefault", &
        &   value    = path, &
        &   refValue = default )
    end if
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetCharOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer                     :: json
    Character(len=*), intent(in)               :: path
    Character(len=:), Allocatable, intent(out) :: value
    Class(*), intent(in)                       :: default
    Logical, optional                          :: isFound
!! -------------------------------------------------------------------------- !!
    Logical      :: isFound_, isError
    type(string) :: str

    Call JSON_GetChar( json = json, path = path, value = value, isFound = isFound_)
    if (.NOT.isFound_) then

        isError = .FALSE.
        str = ""
        Select Type (default)

            Type is ( Character(len=*) )
                str = default
            Type is ( Integer )
                str = default
            Type is ( Real(RP) )
                str = default
            Type is ( Logical )
                if (default.eqv..true.) then
                    str = ".true."
                else
                    str = ".false."
                end if
            Type is ( typString )
                str%raw = default%raw
            Type is ( string )
                str%raw = default%raw
            Class Default
                isError = .TRUE.
        End Select

        value = str%raw
        if ( isError ) then
            Call GURU%Error( &
            &   msg      = "Failed to get 'value'. Conversion of 'default' is failed.", &
            &   head     = "JSON_GetCharOrDefault", &
            &   value    = path )
        else
            Call GURU%Debug( &
            &   msg      = "Failed to get 'value'. 'default' is returned.", &
            &   head     = "JSON_GetCharOrDefault", &
            &   value    = path, &
            &   refValue = value )
        end if

        Call str%Free()

    end if
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetStringOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Type(typString), intent(out) :: value
    Class(*), intent(in)         :: default
    Logical, optional            :: isFound
!! -------------------------------------------------------------------------- !!
    Logical      :: isFound_, isError
    type(string) :: str

    Call JSON_GetString( json = json, path = path, value = value, isFound = isFound_)
    if (.NOT.isFound_) then

        isError = .FALSE.
        str = ""
        Select Type (default)

            Type is ( Character(len=*) )
                str = default
            Type is ( Integer )
                str = default
            Type is ( real(RP) )
                str = default
            Type is ( Logical )
                if (default.eqv..true.) then
                    str = ".true."
                else
                    str = ".false."
                end if
            Type is ( typString )
                str%raw = default%raw
            Type is ( string )
                str%raw = default%raw
            Class Default
                isError = .TRUE.
        End Select

        value%raw = str%raw

        if ( isError ) then
            Call GURU%Error( &
            &   msg      = "Failed to get 'value'. Conversion of 'default' is failed.", &
            &   head     = "JSON_GetStringOrDefault", &
            &   value    = path )
        else
            Call GURU%Debug( &
            &   msg      = "Failed to get 'value'. 'default' is returned.", &
            &   head     = "JSON_GetStringOrDefault", &
            &   value    = path, &
            &   refValue = value )
        end if

        Call str%Free()

    end if
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetIntVectorOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer            :: json
    Character(len=*), intent(in)      :: path
    Integer, Allocatable              :: value(:)
    Integer, dimension(:), intent(in) :: default
    Logical, optional                 :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Integer :: nArg

    Call JSON_GetIntVector( json = json, path = path, value = value, isFound = isFound_)

    if (.NOT.isFound_) then

        nArg = size(default)

        If ( nArg.ge.1 ) then
            Allocate( value(nArg) ); value(:) = 0
            value(:) = default(:)
            Call GURU%DebugSingleVector( &
            &   msg      = "Conversion fails. 'default' is used.", &
            &   head     = "JSON_GetIntVectorOrDefault",           &
            &   value    = path,                                   &
            &   refValue = default                                 )
        else
            Call GURU%Error( &
            &   msg      = "The size of 'default' is less than 0", &
            &   head     = "JSON_GetIntVectorOrDefault",           &
            &   value    = path                                    )
        end if

    end if
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!
!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetRealVectorOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer             :: json
    Character(len=*), intent(in)       :: path
    Real(RP), Allocatable              :: value(:)
    Real(RP), dimension(:), intent(in) :: default
    Logical, optional                  :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Integer :: nArg

    Call JSON_GetRealVector( json = json, path = path, value = value, isFound = isFound_)

    if (.NOT.isFound_) then

        nArg = size(default)

        If ( nArg.ge.1 ) then
            Allocate( value(nArg) ); value(:) = 0.D0
            value(:) = default(:)
            Call GURU%DebugSingleVector( &
            &   msg      = "Conversion fails. 'default' is used.", &
            &   head     = "JSON_GetRealVectorOrDefault",          &
            &   value    = path,                                   &
            &   refValue = default                                 )
        else
            Call GURU%Error( &
            &   msg      = "The size of 'default' is less than 0", &
            &   head     = "JSON_GetRealVectorOrDefault",          &
            &   value    = path                                    )
        end if

    end if
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetLogicalVectorOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer            :: json
    Character(len=*), intent(in)      :: path
    Logical, Allocatable              :: value(:)
    Logical, dimension(:), intent(in) :: default
    Logical, optional                 :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_
    Integer :: nArg

    Call JSON_GetLogicalVector( json = json, path = path, value = value, isFound = isFound_)

    if (.NOT.isFound_) then

        nArg = size(default)

        If ( nArg.ge.1 ) then
            Allocate( value(nArg) ); value(:) = .FALSE.
            value(:) = default(:)
            Call GURU%DebugSingleVector( &
            &   msg      = "Conversion fails. 'default' is used.", &
            &   head     = "JSON_GetLogicalVectorOrDefault",       &
            &   value    = path,                                   &
            &   refValue = default                                 )
        else
            Call GURU%Error( &
            &   msg      = "The size of 'default' is less than 0", &
            &   head     = "JSON_GetLogicalVectorOrDefault",       &
            &   value    = path                                    )
        end if

    end if
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetCharVectorOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer                    :: json
    Character(len=*), intent(in)              :: path
    Character(len=:),allocatable,dimension(:) :: value
    Class(*),dimension(:), intent(in)         :: default
    Logical, optional                         :: isFound
!! -------------------------------------------------------------------------- !!
    Logical      :: isFound_, isError
    Integer      :: nArg, iArg, max_len
    type(string), Allocatable :: tmpStr(:)

    Call JSON_GetCharVector( json = json, path = path, value = value, isFound = isFound_ )

    if (.NOT.isFound_) then

         nArg = size(default)

        If ( nArg.ge.1 ) then

            if ( allocated(tmpStr) ) deallocate(tmpStr)
            Allocate(tmpStr(nArg))

            isError = .FALSE.
            Select Type (default)

                Type is ( Character(len=*) )
                    do iArg = 1, nArg
                        tmpStr(iArg) = default(iArg)
                    end do
                Type is ( Integer )
                    do iArg = 1, nArg
                        tmpStr(iArg) = default(iArg)
                    end do
                Type is ( real(RP) )
                    do iArg = 1, nArg
                        tmpStr(iArg) = default(iArg)
                    end do
                Type is ( Logical )
                    do iArg = 1, nArg
                        if (default(iArg).eqv..true.) then
                            tmpStr(iArg) = ".true."
                        else
                            tmpStr(iArg) = ".false."
                        end if
                    end do
                Type is ( typString )
                    do iArg = 1, nArg
                        tmpStr(iArg)%raw = default(iArg)%raw
                    end do
                Type is ( string )
                    do iArg = 1, nArg
                        tmpStr(iArg)%raw = default(iArg)%raw
                    end do
                Class Default
                    isError = .TRUE.
            End Select

            if (isError) then
                Call GURU%Error( &
                &   msg      = "Failed to conver 'default' into character array.",  &
                &   head     = "JSON_GetCharVectorOrDefault",                       &
                &   value    = path                                                 )
            end if

            max_len = 0
            do iArg = 1, nArg
                if ( tmpStr(iArg)%Len().gt.max_len) max_len = tmpStr(iArg)%Len()
            end do

            If (max_len.le.0) then
                Call GURU%Error( &
                &   msg      = "Maximum character length of 'default' is less than 0.",  &
                &   head     = "JSON_GetCharVectorOrDefault",                            &
                &   value    = path                                                      )
            end if

            if ( allocated(value) ) deallocate(value)
            Allocate( character(max_len)::value(nArg) )
            do iArg = 1, nArg
                value(iArg) = tmpStr(iArg)%raw
            end do

            Call GURU%DebugSingleVector( &
            &   msg      = "Conversion fails. 'default' is used.", &
            &   head     = "JSON_GetLogicalVectorOrDefault",       &
            &   value    = path,                                   &
            &   refValue = value                                  )

            if ( allocated(tmpStr) ) deallocate(tmpStr)

        else
            Call GURU%Error( &
            &   msg      = "The size of 'default' is less than 0", &
            &   head     = "JSON_GetCharVectorOrDefault",          &
            &   value    = path                                    )
        end if

    end if

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_GetStringVectorOrDefault( json, path, value, default, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer              :: json
    Character(len=*), intent(in)        :: path
    type(typString),allocatable         :: value(:)
    Class(*),dimension(:), intent(in)   :: default
    Logical, optional                   :: isFound
!! -------------------------------------------------------------------------- !!
    Logical      :: isFound_, isError
    Integer      :: nArg, iArg, max_len
    type(string), Allocatable :: tmpStr(:)

    Call JSON_GetStringVector( json = json, path = path, value = value, isFound = isFound_ )

    if (.NOT.isFound_) then

         nArg = size(default)

        If ( nArg.ge.1 ) then

            if ( allocated(tmpStr) ) deallocate(tmpStr)
            Allocate(tmpStr(nArg))

            isError = .FALSE.
            Select Type (default)

                Type is ( Character(len=*) )
                    do iArg = 1, nArg
                        tmpStr(iArg) = default(iArg)
                    end do
                Type is ( Integer )
                    do iArg = 1, nArg
                        tmpStr(iArg) = default(iArg)
                    end do
                Type is ( real(RP) )
                    do iArg = 1, nArg
                        tmpStr(iArg) = default(iArg)
                    end do
                Type is ( Logical )
                    do iArg = 1, nArg
                        if (default(iArg).eqv..true.) then
                            tmpStr(iArg) = ".true."
                        else
                            tmpStr(iArg) = ".false."
                        end if
                    end do
                Type is ( typString )
                    do iArg = 1, nArg
                        tmpStr(iArg)%raw = default(iArg)%raw
                    end do
                Type is ( string )
                    do iArg = 1, nArg
                        tmpStr(iArg)%raw = default(iArg)%raw
                    end do
                Class Default
                    isError = .TRUE.
            End Select

            if (isError) then
                Call GURU%Error( &
                &   msg      = "Failed to conver 'default' into character array.",  &
                &   head     = "JSON_GetCharVectorOrDefault",                       &
                &   value    = path                                                 )
            end if

            if ( allocated(value) ) deallocate(value)
            Allocate( value(nArg) )
            do iArg = 1, nArg
                value(iArg) = tmpStr(iArg)%raw
            end do

            Call GURU%DebugSingleVector( &
            &   msg      = "Conversion fails. 'default' is used.", &
            &   head     = "JSON_GetLogicalVectorOrDefault",       &
            &   value    = path,                                   &
            &   refValue = value                                  )

            if ( allocated(tmpStr) ) deallocate(tmpStr)

        else
            Call GURU%Error( &
            &   msg      = "The size of 'default' is less than 0", &
            &   head     = "JSON_GetCharVectorOrDefault",          &
            &   value    = path                                    )
        end if

    end if

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!
