!! -------------------------------------------------------------------------- !!
Subroutine JSON_Update( json, path, value )
!! -------------------------------------------------------------------------- !!
Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Class(*), intent(in)         :: value
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: path_, charVar, error_msg
    Logical :: isError, status_ok, found

    if ( .NOT.associated(json) ) then
        Call GURU%Error( &
        &   msg  = "Given 'json' is null pointer. Failed to add 'value'.", &
        &   head = "JSON_Update")
    end if

    path_ = trim(adjustl(path))
    if ( len(path_).le.0 ) then
        Call GURU%Error( &
        &   msg  = "Given 'path' is empty. Failed to add 'value'.", &
        &   head = "JSON_Update")
    end if

    isError = .FALSE.
    Select Type (value)
        Type is ( Character(len=*) )
            Call jsonCore%Update( json, path_, value, found)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Type is ( Integer )
            Call jsonCore%Update( json, path_, value, found)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Type is ( real(RP) )
            Call jsonCore%Update( json, path_, value, found)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Type is ( Logical )
            Call jsonCore%Update( json, path_, value, found)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Type is ( typString )
            charVar = value%raw
            Call jsonCore%Update( json, path_, charVar, found)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Type is ( string )
            charVar = value%raw
            Call jsonCore%Update( json, path_, charVar, found)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Class Default
            isError   = .TRUE.
            error_msg = "Failed to update 'value' for given 'path'. Please check variable type."
    End Select

    if (isError) then
        Call GURU%Error( &
        &   msg   = error_msg, &
        &   head  = "JSON_Add", &
        &   value = path_)
    end if

    if (Allocated(path_)) Deallocate(path_)
    if (Allocated(charVar)) Deallocate(charVar)
    if (Allocated(error_msg)) Deallocate(error_msg)

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_UpdateVector( json, path, value )
!! -------------------------------------------------------------------------- !!
Implicit None
    type(typJSON), pointer             :: json
    Character(len=*), intent(in)       :: path
    Class(*), Dimension(:), intent(in) :: value
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: path_, error_msg
    character(len=:), Allocatable, Dimension(:) :: charVar
    Logical :: isError, status_ok, found
    Integer :: maxNChar, iArg, nArg, sLen

    if ( .NOT.associated(json) ) then
        Call GURU%Error( &
        &   msg  = "Given 'json' is null pointer. Failed to add 'value'.", &
        &   head = "JSON_UpdateVector" )
    end if

    path_ = trim(adjustl(path))
    if ( len(path_).le.0 ) then
        Call GURU%Error( &
        &   msg  = "Given 'path' is empty. Failed to add 'value'.", &
        &   head = "JSON_UpdateVector" )
    end if

    nArg = size(value)
    if (nArg.le.0) then
        Call GURU%Error( &
        &   msg  = "The size of 'value' is less than 1.", &
        &   head = "JSON_UpdateVector" )
    end if

    Call JSON_Remove(json, path_)

    isError = .FALSE.
    Select Type (value)

        !!... Add Character Vector
        Type is ( Character(len=*) )
            Call jsonCore%Add( json, path_, value)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Type is ( Integer )
            Call jsonCore%Add( json, path_, value)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Type is ( real(RP) )
            Call jsonCore%Add( json, path_, value)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.

        Type is ( Logical )
            Call jsonCore%Add( json, path_, value)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.

        !!... Add typString Vector
        Type is ( typString )
            maxNChar = 0
            do iArg = 1, nArg
                sLen = value(iArg)%Len()
                if ( sLen.ge.iArg ) maxNChar = sLen
            end do
            Allocate( character(maxNChar)::charVar(nArg) )
            do iArg = 1, nArg
                charVar(iArg) = value(iArg)%raw
            end do
            Call jsonCore%Add( json, path_, charVar)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.

        !!... Add string Vector
        Type is ( string )
            maxNChar = 0
            do iArg = 1, nArg
                sLen = value(iArg)%Len()
                if ( sLen.ge.iArg ) maxNChar = sLen
            end do
            Allocate( character(maxNChar)::charVar(nArg) )
            do iArg = 1, nArg
                charVar(iArg) = value(iArg)%raw
            end do
            Call jsonCore%Add( json, path_, charVar)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.

        Class Default
            isError = .TRUE.
            error_msg = "Failed to update 'value' for given 'path'. Please check variable type."
    End Select

    if (isError) then
        Call GURU%Error( &
        &   msg   = error_msg, &
        &   head  = "JSON_UpdateVector", &
        &   value = path_)
    end if

    if (Allocated(path_)) Deallocate(path_)
    if (Allocated(charVar)) Deallocate(charVar)
    if (Allocated(error_msg)) Deallocate(error_msg)

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_UpdateChild(json, child, isFound)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer         :: json
    type(typJSON), pointer         :: child
    Logical, intent(out), Optional :: isFound
!! -------------------------------------------------------------------------- !!
    Logical :: isFound_, status_ok
    Character(len=:), Allocatable :: childName, error_msg
    type(typJSON), pointer :: orgChild

    !!... Get Child Name
    Call JSON_GetName( json = child, name = childName )
    Call JSON_GetChildIFFound( &
    &   json = json, name = childName, child = orgChild, isFound = isFound_ )

    !!... Replace or Add Child
    if ( isFound_ ) then
        Call jsonCore%Replace( orgChild, child )
    else
        Call jsonCore%Add( json, child )
    end if

    !!... Check Error
    Call jsonCore%Check_for_errors(status_ok, error_msg)
    if (.NOT.status_ok) then
        Call GURU%Error( &
        &   msg   = error_msg, &
        &   head  = "JSON_UpdateChild", &
        &   value = childName )
    end if

    If (Allocated(childName)) deallocate(childName)
    If (Allocated(error_msg)) deallocate(error_msg)
    orgChild => null()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
