!! -------------------------------------------------------------------------- !!
Subroutine Initialize_typGURU(this, projectName, description, authors, date, version )
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

    !!... Initialize arguments variables
    Call this%Initialize_ARG( &
    &   projectName = projectName,  &
    &   description = description,  &
    &   authors     = authors,      &
    &   date        = date,         &
    &   version     = version       )

    !!... Add Default Keys
    Call this%AddKey( &
    &   key           = "help",                   &
    &   description   = "Print help messages"     )

    !!... Log File
    Call this%AddKey( &
    &   key           = "log",           &
    &   description   = "Log file path", &
    &   nRequiredWord = 1                )

    !!... Color Function
    Call this%AddKey( &
    &   key           = "color",              &
    &   description   = "Enable color mode",  &
    &   isRequiredKey = .FALSE.               )

    !!... Color Function
    Call this%AddKey( &
    &   key           = "debug",              &
    &   description   = "Enable debug mode",  &
    &   isRequiredKey = .FALSE.               )

    !!... Update Basic arguments
    Call this%Update_ARG()

    !!... Get Color Mode & Debug Mode, Log File
    blkSetMode: Block
        Logical :: isFound, isColor, isDebug, isLogFile
        Character(len=:), Allocatable :: logFilePath

        isColor   = .FALSE.
        isDebug   = .FALSE.
        isLogFile = .FALSE.

        !!... Color Mode
        Call this%IsFoundKey( key = "color", isFound = isFound )
        If ( isFound.EQV..TRUE. ) isColor = .TRUE.

        !!... Debug Mode
        Call this%IsFoundKey( key = "debug", isFound = isFound )
        If ( isFound.EQV..TRUE. ) isDebug = .TRUE.

        Call this%IsFoundKey( key = "log", isFound = isFound )

        If ( isFound.EQV..TRUE. ) then
            isLogFile = .TRUE.
            Call this%GetArgChar( key = "log", value = logFilePath )
            if (len_trim(adjustl(logFilePath)).le.0) then
                Call this%Error( &
                &   msg  = "Given 'logFilePath' after --log is invalid.", &
                &   head = "Initialize_typGURU"  )
            End if

            !!... Log File
            Call this%Initialize_LOG( &
            &   logFilePath = trim(adjustl(logFilePath)) )

        End if

        !!... Toggle Color and Debug Mode
        Call this%ToggleColorMode( isColor = isColor )
        Call this%ToggleDebugMode( isDebug = isDebug )

    End Block blkSetMode

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
