!! -------------------------------------------------------------------------- !!
Subroutine GetNewFileUnit( unit )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Integer, intent(out) :: unit
!! -------------------------------------------------------------------------- !!
    Logical :: isFileOpen

    unit = F_UNIT_BEGIN
    inquire( unit = unit, opened = isFileOpen )
    if ( isFileOpen ) unit = F_UNIT_CURRENT

    loopFid: do
        unit = unit + 1
        inquire( unit = unit, opened = isFileOpen )
        if (.NOT.isFileOpen) exit loopFid
    end do loopFid

    F_UNIT_CURRENT = unit

    if ( unit.eq.F_UNIT_IN_DEFAULT ) then
        Call GURU%Error( &
        &   msg  = " 'unit' is default unit for input.", &
        &   head = "GetNewFileUnit" )
    else if ( unit.eq.F_UNIT_OUT_DEFAULT ) then
        Call GURU%Error( &
        &   msg  = " 'unit' is default unit for output.", &
        &   head = "GetNewFileUnit" )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
