!! -------------------------------------------------------------------------- !!
Subroutine JSON_Add( json, path, value )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: path
    Class(*), intent(in)         :: value
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: path_, charVar, error_msg
    Logical :: isError, status_ok

    if ( .NOT.associated(json) ) then
        Call GURU%Error( &
        &   msg  = "Given 'json' is null pointer. Failed to add 'value'.", &
        &   head = "JSON_Add")
    end if

    path_ = trim(adjustl(path))
    if ( len(path_).le.0 ) then
        Call GURU%Error( &
        &   msg  = "Given 'path' is empty. Failed to add 'value'.", &
        &   head = "JSON_Add")
    end if

    isError = .FALSE.
    Select Type (value)
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
        Type is ( typString )
            charVar = value%raw
            Call jsonCore%Add( json, path_, charVar)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Type is ( string )
            charVar = value%raw
            Call jsonCore%Add( json, path_, charVar)
            Call jsonCore%Check_for_errors(status_ok, error_msg)
            if (.NOT. status_ok) isError = .TRUE.
        Class Default
            isError = .TRUE.
            error_msg = "Failed to add 'value ' for given 'path'. Please check variable type."
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
Subroutine JSON_AddVector( json, path, value )
!! -------------------------------------------------------------------------- !!
Implicit None
    type(typJSON), pointer             :: json
    Character(len=*), intent(in)       :: path
    Class(*), Dimension(:), intent(in) :: value
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: path_, error_msg
    character(len=:), Allocatable, Dimension(:) :: charVar
    Logical :: isError, status_ok
    Integer :: maxNChar, iArg, nArg, sLen

    if ( .NOT.associated(json) ) then
        Call GURU%Error( &
        &   msg  = "Given 'json' is null pointer. Failed to add 'value'.", &
        &   head = "JSON_AddVector")
    end if

    path_ = trim(adjustl(path))
    if ( len(path_).le.0 ) then
        Call GURU%Error( &
        &   msg  = "Given 'path' is empty. Failed to add 'value'.", &
        &   head = "JSON_AddVector")
    end if

    nArg = size(value)
    if (nArg.le.0) then
        Call GURU%Error( &
        &   msg  = "The size of 'value' is less than 1.", &
        &   head = "JSON_AddVector")
    end if

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
            error_msg = "Failed to add 'value ' for given 'path'. Please check variable type."

    End Select

    if (isError) then
        Call GURU%Error( &
        &   msg   = error_msg, &
        &   head  = "JSON_AddVector", &
        &   value = path_)
    end if

    if (Allocated(path_)) Deallocate(path_)
    if (Allocated(charVar)) Deallocate(charVar)
    if (Allocated(error_msg)) Deallocate(error_msg)

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_AddChild( json, child )
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer :: json
    type(typJSON), pointer :: child
!! -------------------------------------------------------------------------- !!
    Logical :: status_ok
    Character(len=:), allocatable :: error_msg, childName

    Call jsonCore%Add( json, child )
    Call jsonCore%Check_for_errors(status_ok, error_msg)

    if (.NOT. status_ok) then
        Call jsonCore%Info(child, name = childName)
        Call GURU%Error( &
        &   msg   = error_msg, &
        &   head  = "JSON_AddChild", &
        &   value = childName )
    End if

    if (Allocated(error_msg)) deallocate(error_msg)
    if (Allocated(childName)) deallocate(childName)

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!
