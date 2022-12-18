!! -------------------------------------------------------------------------- !!
Subroutine LOG_Initialize_typGURU( &
    & this, logFilePath, isDebug, isColor, MPI_isMaster )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(inout)        :: this
    Character(len=*), intent(in), Optional  :: logFilePath
    Logical, intent(in), Optional           :: isDebug
    Logical, intent(in), Optional           :: isColor
    Logical, intent(in), Optional           :: MPI_isMaster
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: baseDir
    character(len=:), Allocatable :: baseName
    character(len=:), Allocatable :: ext
    Logical :: isBaseDirExist, isError, isCreate

    !!... Set Debug Mode & Color Mode
    If ( Present(isDebug) ) this%isDebug_ = isDebug
    If ( Present(isColor) ) this%isColor_ = isColor

    !!... TODO: Set the MPI Envi
    !this%MPI_isMaster = .FALSE.
    if ( present(MPI_isMaster) ) this%MPI_isMaster_ = MPI_isMaster

    If ( this%MPI_isMaster_ ) then

    !!... Set log file
    this%isLogFile = .FALSE.

    !!... Deallocate log file path
    If (Allocated(this%logFilePath_)) Deallocate(this%logFilePath_)

    !!... Set log file path if it is given.
    If ( Present(logFilePath) ) then

        !!... Set a new log file path
        this%logFilePath_ = trim(adjustl(logFilePath))

        If ( len_trim(this%logFilePath_).GE.1 ) then
            this%logFilePath_ = logFilePath
        else
            Call WritePrefixMessage( &
            &   prefix      = PREIX_ERROR,                              &
            &   prefixColor = COLOR_ERROR,                              &
            &   head        = "LOG_Initialize_typGURU",                 &
            &   msg         = "Given 'logFilePath' is less than 0.",    &
            &   isColor     = this%isColor_,                            &
            &   isStop      = .TRUE.                                    )
        End if

        this%isLogFile = .TRUE.

    end If

    if ( this%isLogFile ) then

        !!... Open Log file & Ready to write log
        blcCreateLogFile: Block
            Logical :: isFileOpen, isFileExist
            Logical :: isPrevFileExist

            !!... Check file exist already or opened by other program
            Inquire( file   = this%logFilePath_, &
            &        exist  = isFileExist,       &
            &        opened = isFileOpen         )

            isPrevFileExist = .FALSE.
            If ( isFileExist.EQV..TRUE. ) isPrevFileExist = .TRUE.

            If ( isFileOpen.EQV..TRUE. ) then
                Call WritePrefixMessage( &
                &   prefix      = PREIX_ERROR,              &
                &   prefixColor = COLOR_ERROR,              &
                &   head        = "LOG_Initialize_typGURU",  &
                &   msg         = "Given log file is already opened.", &
                &   isColor     = this%isColor_,            &
                &   isStop      = .TRUE.                    )
            end if

            !!... Separate Path
            Call SeparatePath( this%logFilePath_, baseDir, baseName, ext)

            If ( len_trim(baseDir).ge.1 ) then

                isBaseDirExist = IsExistDir( baseDir )

                If (isBaseDirExist.EQV..FALSE.) then

                    Call CreateDir( &
                    &   dirPath  = baseDir, &
                    &   isError  = isError, &
                    &   isCreate = isCreate )

                    If ( (isError.EQV..TRUE.).OR.(isCreate.EQV..FALSE.) ) then
                        Call WritePrefixMessage( &
                        &   prefix      = PREIX_ERROR,              &
                        &   prefixColor = COLOR_ERROR,              &
                        &   head        = "LOG_Initialize_typGURU",  &
                        &   msg         = "Failed to create the directory for log file.", &
                        &   value    = baseDir,                  &
                        &   isColor     = this%isColor_,            &
                        &   isStop      = .TRUE.                    )
                    End if

                End if
            end if

            !!... If Prev Log file exist, rename them
            If ( isPrevFileExist ) then
                blkMoveFiles: Block
                    !type(string) :: filePath
                    !!...
                    Logical :: isExist, isOpened, isOrgExist
                    Integer :: iFile, ios
                    Character(len=:), Allocatable :: prevFilePath
                    Character(len=30) :: iFileChar


                    ! type(string) :: baseDir, baseName, ext
                    Integer :: iStr1, iStr2, iStr3, nChar
                    Integer :: iStrSep, iStrExt
                    Integer :: iStrBase1, iStrBase2
                    Logical :: isDirSep, isExtension

                    !!... Check log file exist and move !
                    Inquire( &
                    &   file   = this%logFilePath_, &
                    &   exist  = isExist,           &
                    &   opened = isOpened           )

                    if ( isOpened.EQV..TRUE. ) then
                        Call WritePrefixMessage( &
                        &   prefix      = PREIX_ERROR,              &
                        &   prefixColor = COLOR_ERROR,              &
                        &   head        = "LOG_Initialize_typGURU",  &
                        &   msg         = "Given 'logFilePath' is already opened.", &
                        &   value    = this%logFilePath_,        &
                        &   isColor     = this%isColor_,            &
                        &   isStop      = .TRUE.                    )
                    end if

                    isOrgExist = .FALSE.
                    if ( isExist.EQV..TRUE. ) isOrgExist = .TRUE.

                    If ( isOrgExist ) then

                        !!... Check existing log file index
                        do iFile = 1, N_MAX_LOG_FILE

                            Write(iFileChar, *, iostat = ios) iFile

                            If (ios.ne.0) then
                                Call WritePrefixMessage( &
                                &   prefix      = PREIX_ERROR,              &
                                &   prefixColor = COLOR_ERROR,              &
                                &   head        = "LOG_Initialize_typGURU",  &
                                &   msg         = "Failed to convert the integer.", &
                                &   isColor     = this%isColor_,            &
                                &   isStop      = .TRUE.                    )
                            end if

                            prevFilePath = baseDir//baseName//"_"//trim(adjustl(iFileChar))//ext

                            Inquire( &
                            &   file   = prevFilePath, &
                            &   exist  = isExist,      &
                            &   opened = isOpened      )

                            If ( (isExist.EQV..FALSE.).AND.(isOpened.EQV..FALSE.) ) then
                                exit
                            End if

                            If ( iFIle.EQ.N_MAX_LOG_FILE ) then
                                Call WritePrefixMessage( &
                                &   prefix      = PREIX_ERROR,              &
                                &   prefixColor = COLOR_ERROR,              &
                                &   head        = "LOG_Initialize_typGURU",  &
                                &   msg         = "Too many log files. Please delete them.", &
                                &   isColor     = this%isColor_,            &
                                &   isStop      = .TRUE.                    )
                            End If

                        end do

                        !!... Move the file
                        Call RenameDirFile( this%logFilePath_, prevFilePath )

                    end if

                    If ( Allocated(prevFilePath) )  Deallocate(prevFilePath)
                    !If ( Allocated(iFileChar) )     Deallocate(iFileChar)

                End Block blkMoveFiles
            End if

            !!... Create Log File
            Open( &
            &   newunit = this%logUnit,     &
            &   file    = this%logFilePath_, &
            &   status  = "replace"          )

            !!... Write Log File
            this%isLogFile = .TRUE.

        End Block blcCreateLogFile

    End if

    If ( Allocated(baseDir) )       Deallocate(baseDir)
    If ( Allocated(baseName) )      Deallocate(baseName)
    If ( Allocated(ext) )           Deallocate(ext)

    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_ToggleDebugMode_typGURU( this, isDebug )
    Implicit None
    Class(typGURU), intent(inout) :: this
    Logical, intent(in)              :: isDebug
    this%isDebug_ = isDebug
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_ToggleColorMode_typGURU( this, isColor )
    Implicit None
    Class(typGURU), intent(inout) :: this
    Logical, intent(in)              :: isColor
    this%isColor_ = isColor
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_ToggleIsMaster_typGURU( this, isMaster )
    Implicit None
    Class(typGURU), intent(inout) :: this
    Logical, intent(in)           :: isMaster
    this%MPI_isMaster_ = isMaster
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function LOG_IsDebug_typGURU( this ) result(isDebug)
!! -------------------------------------------------------------------------- !!
    Implicit none
    Class(typGURU), intent(in) :: this
    Logical :: isDebug
!! -------------------------------------------------------------------------- !!
    isDebug = this%isDebug_
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function LOG_IsColor_typGURU( this ) result(IsColor)
!! -------------------------------------------------------------------------- !!
    Implicit none
    Class(typGURU), intent(in) :: this
    Logical :: IsColor
!! -------------------------------------------------------------------------- !!
    IsColor = this%IsColor_
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function LOG_IsMaster_typGURU( this ) result(isMaster)
!! -------------------------------------------------------------------------- !!
    Implicit none
    Class(typGURU), intent(in) :: this
    Logical :: isMaster
!! -------------------------------------------------------------------------- !!
    isMaster = this%MPI_isMaster_
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_Write_typGURU( this, msg, color, nSpace, isAdvance, masterOnly )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)          :: this
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: color
    Integer, intent(in), Optional          :: nSpace
    Logical, intent(in), Optional          :: isAdvance
    Logical, intent(in), Optional          :: masterOnly
!! -------------------------------------------------------------------------- !!
    type(string)              :: str
    type(string), Allocatable :: strVect(:)
    Character(len=:), Allocatable :: prefix, tmpMsg, color_
    Logical :: isSpace, isWrite
    Integer :: iMsg, nMsgLine
    Integer :: logUnit

    !!... Check Master Only
    isWrite = .TRUE.
    if ( present(masterOnly) ) then
        if ( masterOnly.eqv..true. ) then
            if (this%MPI_isMaster_.EQV..FALSE. )  isWrite = .FALSE.
        end if
    end if

    If (isWrite) then

    !!... Separate message with line separator
    str = msg
    Call str%Split( tokens = strVect, sep = lineSep )
    nMsgLine =  size(strVect)

    !!... Set space before writing the message
    isSpace = .FALSE.
    if ( present(nSpace) ) then
        if (nSpace.ge.1) then
            Allocate( character(nSpace) :: prefix )
            prefix(:) = " "
            isSpace   = .TRUE.
        end if
    end if

    color_ = ""
    If (this%isColor_) then
        If (Present(color)) color_ = color
    End if

    !!... Set log Unit
    logUnit = 6
    If ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

    !!... Write each lines
    do iMsg = 1, nMsgLine

        tmpMsg = strVect(iMsg)%Chars()
        if ( isSpace ) tmpMsg = prefix//tmpMsg

        if ( iMsg.lt.nMsgLine ) then
            Call WriteMessage( &
            &   msg       = tmpMsg,         &
            &   unit      = logUnit,        &
            &   color     = color_          )
        else
            Call WriteMessage( &
            &   msg       = tmpMsg,         &
            &   unit      = logUnit,        &
            &   color     = color_,         &
            &   isAdvance = isAdvance       )
        end if

        If (Allocated(tmpMsg)) Deallocate(tmpMsg)

    end do

    If (Allocated(prefix)) Deallocate(prefix)

    !!... Free the memory
    Call str%Free()
    do iMsg = 1, nMsgLine
        Call strVect%Free()
    end do
    If (Allocated(strVect)) deallocate(strVect)

    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_WriteDebug_typGURU( this, msg, color, nSpace, isAdvance, masterOnly )
!! -------------------------------------------------------------------------- !!
Implicit None
Class(typGURU), intent(in)          :: this
Character(len=*), intent(in)           :: msg
Character(len=*), intent(in), Optional :: color
Integer, intent(in), Optional          :: nSpace
Logical, intent(in), Optional          :: isAdvance
Logical, intent(in), Optional          :: masterOnly
!! -------------------------------------------------------------------------- !!
    If ( this%isDebug_ ) then
        Call this%Write( &
        &   msg       = msg,        &
        &   color     = color,      &
        &   nSpace    = nSpace,     &
        &   isAdvance = isAdvance,  &
        &   masterOnly = masterOnly  )
    end if
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_Info_typGURU( this, info, msg, head, value, refValue, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)             :: this
    Character(len=*), intent(in)           :: info
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: head
    Class(*), intent(in), Optional         :: value
    Class(*), intent(in), Optional         :: refValue
    Logical, intent(in), Optional          :: isStop
!! -------------------------------------------------------------------------- !!

    if ( IsCharEqual(info, "error") ) then
        Call this%Error( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue,    &
        &   isStop   = isStop       )
    else if ( IsCharEqual(info, "warn") ) then
        Call this%Warn( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue     )
    else if ( IsCharEqual(info, "debug") ) then
        Call this%Debug( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue     )
    else
        Call this%Error( &
        &   msg      = "Given 'info' should be 'error', 'warn' or 'debug'.",    &
        &   head     = "LOG_Info_typGURU",          &
        &   value    = info                         )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_InfoSingleVector_typGURU( this, info, msg, head, value, refValue, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                   :: this
    Character(len=*), intent(in)                 :: info
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), intent(in), Optional               :: value
    Class(*), Dimension(:), intent(in), Optional :: refValue
    Logical, intent(in), Optional                :: isStop
!! -------------------------------------------------------------------------- !!

    if ( IsCharEqual(info, "error") ) then
        Call this%ErrorSingleVector( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue,    &
        &   isStop   = isStop       )
    else if ( IsCharEqual(info, "warn") ) then
        Call this%WarnSingleVector( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue     )
    else if ( IsCharEqual(info, "debug") ) then
        Call this%DebugSingleVector( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue     )
    else
        Call this%Error( &
        &   msg      = "Given 'info' should be 'error', 'warn' or 'debug'.",    &
        &   head     = "LOG_InfoSingleVector_typGURU",          &
        &   value    = info                         )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_InfoVectorVector_typGURU( this, info, msg, head, value, refValue, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                   :: this
    Character(len=*), intent(in)                 :: info
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), Dimension(:), intent(in), Optional :: value
    Class(*), Dimension(:), intent(in), Optional :: refValue
    Logical, intent(in), Optional                :: isStop
!! -------------------------------------------------------------------------- !!

    if ( IsCharEqual(info, "error") ) then
        Call this%ErrorVectorVector( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue,    &
        &   isStop   = isStop       )
    else if ( IsCharEqual(info, "warn") ) then
        Call this%WarnVectorVector( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue     )
    else if ( IsCharEqual(info, "debug") ) then
        Call this%DebugVectorVector( &
        &   msg      = msg,         &
        &   head     = head,        &
        &   value    = value,       &
        &   refValue = refValue     )
    else
        Call this%Error( &
        &   msg      = "Given 'info' should be 'error', 'warn' or 'debug'.",    &
        &   head     = "LOG_InfoVectorVector_typGURU",          &
        &   value    = info                         )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_Error_typGURU( this, msg, head, value, refValue, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)             :: this
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: head
    Class(*), intent(in), Optional         :: value
    Class(*), intent(in), Optional         :: refValue
    Logical, intent(in), Optional          :: isStop
!! -------------------------------------------------------------------------- !!
    Logical :: isStop_
    Logical :: isColor
    Integer :: logUnit

    isStop_ = .true.
    if (present(isStop)) isStop_ = isStop

    isColor = this%isColor_
    logUnit = 6
    if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

    !!... Write Message
    Call WritePrefixMessage( &
    &   prefix      = PREIX_ERROR,      &
    &   prefixColor = COLOR_ERROR,      &
    &   msg         = msg,              &
    &   head        = head,             &
    &   value       = value,            &
    &   refValue    = refValue,         &
    &   unit        = logUnit,          &
    &   isColor     = isColor,          &
    &   isStop      = isStop_           )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_ErrorSingleVector_typGURU( this, msg, head, value, refValue, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                   :: this
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), intent(in), Optional               :: value
    Class(*), Dimension(:), intent(in), Optional :: refValue
    Logical, intent(in), Optional                :: isStop
!! -------------------------------------------------------------------------- !!
    Logical :: isStop_
    Logical :: isColor
    Integer :: logUnit

    isStop_ = .true.
    if (present(isStop)) isStop_ = isStop

    isColor = this%isColor_
    logUnit = 6
    if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

    !!... Write Message
    Call WritePrefixMessageSingleVector( &
    &   prefix      = PREIX_ERROR,      &
    &   prefixColor = COLOR_ERROR,      &
    &   msg         = msg,              &
    &   head        = head,             &
    &   value       = value,            &
    &   refValue    = refValue,         &
    &   unit        = logUnit,          &
    &   isColor     = isColor,          &
    &   isStop      = isStop_           )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_ErrorVectorVector_typGURU( this, msg, head, value, refValue, isStop )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                   :: this
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), Dimension(:), intent(in), Optional :: value
    Class(*), Dimension(:), intent(in), Optional :: refValue
    Logical, intent(in), Optional                :: isStop
!! -------------------------------------------------------------------------- !!
    Logical :: isStop_
    Logical :: isColor
    Integer :: logUnit

    isStop_ = .true.
    if (present(isStop)) isStop_ = isStop

    isColor = this%isColor_
    logUnit = 6
    if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

    !!... Write Message
    Call WritePrefixMessageVectorVector( &
    &   prefix      = PREIX_ERROR,      &
    &   prefixColor = COLOR_ERROR,      &
    &   msg         = msg,              &
    &   head        = head,             &
    &   value       = value,            &
    &   refValue    = refValue,         &
    &   unit        = logUnit,          &
    &   isColor     = isColor,          &
    &   isStop      = isStop_           )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_Warn_typGURU( this, msg, head, value, refValue )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)             :: this
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: head
    Class(*), intent(in), Optional         :: value
    Class(*), intent(in), Optional         :: refValue
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: logUnit

    isColor = this%isColor_
    logUnit = 6
    if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

    !!... Write Message
    Call WritePrefixMessage( &
    &   prefix      = PREIX_WARN,       &
    &   prefixColor = COLOR_WARN,       &
    &   msg         = msg,              &
    &   head        = head,             &
    &   value       = value,            &
    &   refValue    = refValue,         &
    &   unit        = logUnit,          &
    &   isColor     = isColor,          &
    &   isStop      = .FALSE.           )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_WarnSingleVector_typGURU( this, msg, head, value, refValue )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                   :: this
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), intent(in), Optional               :: value
    Class(*), Dimension(:), intent(in), Optional :: refValue
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: logUnit

    isColor = this%isColor_
    logUnit = 6
    if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

    !!... Write Message
    Call WritePrefixMessageSingleVector( &
    &   prefix      = PREIX_WARN,       &
    &   prefixColor = COLOR_WARN,       &
    &   msg         = msg,              &
    &   head        = head,             &
    &   value       = value,            &
    &   refValue    = refValue,         &
    &   unit        = logUnit,          &
    &   isColor     = isColor,          &
    &   isStop      = .FALSE.           )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_WarnVectorVector_typGURU( this, msg, head, value, refValue )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                   :: this
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), Dimension(:), intent(in), Optional :: value
    Class(*), Dimension(:), intent(in), Optional :: refValue
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: logUnit

    isColor = this%isColor_
    logUnit = 6
    if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

    !!... Write Message
    Call WritePrefixMessageVectorVector( &
    &   prefix      = PREIX_WARN,       &
    &   prefixColor = COLOR_WARN,       &
    &   msg         = msg,              &
    &   head        = head,             &
    &   value       = value,            &
    &   refValue    = refValue,         &
    &   unit        = logUnit,          &
    &   isColor     = isColor,          &
    &   isStop      = .FALSE.           )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_Debug_typGURU( this, msg, head, value, refValue )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)          :: this
    Character(len=*), intent(in)           :: msg
    Character(len=*), intent(in), Optional :: head
    Class(*), intent(in), Optional         :: value
    Class(*), intent(in), Optional         :: refValue
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: logUnit

    If ( this%isDebug_ ) then
        isColor = this%isColor_
        logUnit = 6
        if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

        !!... Write Message
        Call WritePrefixMessage( &
        &   prefix      = PREIX_DEBUG,      &
        &   prefixColor = COLOR_DEBUG,      &
        &   msg         = msg,              &
        &   head        = head,             &
        &   value       = value,            &
        &   refValue    = refValue,         &
        &   unit        = logUnit,          &
        &   isColor     = isColor,          &
        &   isStop      = .FALSE.           )

    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_DebugSingleVector_typGURU( this, msg, head, value, refValue )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                   :: this
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), intent(in), Optional               :: value
    Class(*), Dimension(:), intent(in), Optional :: refValue
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: logUnit

    If ( this%isDebug_ ) then
        isColor = this%isColor_
        logUnit = 6
        if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

        !!... Write Message
        Call WritePrefixMessageSingleVector( &
        &   prefix      = PREIX_DEBUG,      &
        &   prefixColor = COLOR_DEBUG,      &
        &   msg         = msg,              &
        &   head        = head,             &
        &   value       = value,            &
        &   refValue    = refValue,         &
        &   unit        = logUnit,          &
        &   isColor     = isColor,          &
        &   isStop      = .FALSE.           )

    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine LOG_DebugVectorVector_typGURU( this, msg, head, value, refValue )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in)                   :: this
    Character(len=*), intent(in)                 :: msg
    Character(len=*), intent(in), Optional       :: head
    Class(*), Dimension(:), intent(in), Optional :: value
    Class(*), Dimension(:), intent(in), Optional :: refValue
!! -------------------------------------------------------------------------- !!
    Logical :: isColor
    Integer :: logUnit

    If ( this%isDebug_ ) then
        isColor = this%isColor_
        logUnit = 6
        if ( (this%MPI_isMaster_).AND.(this%isLogFile) ) logUnit = this%logUnit

        !!... Write Message
        Call WritePrefixMessageVectorVector( &
        &   prefix      = PREIX_DEBUG,      &
        &   prefixColor = COLOR_DEBUG,      &
        &   msg         = msg,              &
        &   head        = head,             &
        &   value       = value,            &
        &   refValue    = refValue,         &
        &   unit        = logUnit,          &
        &   isColor     = isColor,          &
        &   isStop      = .FALSE.           )

    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!\

!! -------------------------------------------------------------------------- !!
    Subroutine LOG_Flush_typGURU(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typGURU), intent(in) :: this
!! -------------------------------------------------------------------------- !!
    if ( this%isLogFile ) Flush(this%logUnit)
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine LOG_Stop_typGURU(this)
    Implicit None
    Class(typGURU), intent(in) :: this
!! -------------------------------------------------------------------------- !!
    Stop
!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine LOG_Destroy_typGURU(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Class(typGURU), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Call this%Flush()   !!... Write Log file

    !!... Write the message to the log file
    this%isLogFile = .FALSE.

    !!... Log File Path
    this%logFilePath_ = ""

    !!... Default Output
    this%logUnit = 11

    !!... Is Debug Mode (Write the message when the debug mode is enabled)
    this%isDebug_ = .FALSE.

    !!... Is Color Mode (Write the message when the debug mode is enabled)
    this%isColor_ = .FALSE.

    !!... MPI Variable representing the master
    this%MPI_isMaster_ = .TRUE.

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!
