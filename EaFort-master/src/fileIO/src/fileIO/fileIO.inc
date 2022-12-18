!! -------------------------------------------------------------------------- !!
Subroutine Initialize_typFileIO( this, filePath, status, form )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(inout)        :: this
    Character(len=*), intent(in)           :: filePath
    Character(len=*), intent(in)           :: status
    Character(len=*), intent(in), Optional :: form
!! -------------------------------------------------------------------------- !!

    this%filePath = trim(adjustl(filePath))
    this%status   = trim(adjustl(status))
    this%form     = "formatted"

    if (present(form)) this%form = form

    !!... Check file path
    if ( len(this%filePath).le.0 ) then
        Call GURU%Error( &
        &   msg  = " Given 'filePath' is empty.", &
        &   head = "Initialize_typFileIO"         )
    end if

    !!... Check status
    if ( len(this%status).le.0 ) then
        Call GURU%Error( &
        &   msg  = " Given 'status' is empty.", &
        &   head = "Initialize_typFileIO"         )
    else if ( isStringEqual( this%status, "old").OR.     &
        &     isStringEqual( this%status, "new").OR.     &
        &     isStringEqual( this%status, "replace").OR. &
        &     isStringEqual( this%status, "scratch").OR. &
        &     isStringEqual( this%status, "unknown")     ) then
    else
        Call GURU%Error( &
        &   msg   = " Given 'status' is not supported.", &
        &   head  = "Initialize_typFileIO", &
        &   value = this%status )
    end if

    !!... Check form
    if ( len(this%form).le.0 ) then
        Call GURU%Error( &
        &   msg  = " Given 'form' is empty.", &
        &   head = "Initialize_typFileIO"         )
    else if ( isStringEqual( this%form, "formatted").OR.     &
        &     isStringEqual( this%form, "unformatted") ) then
    else
        Call GURU%Error( &
        &   msg   = " Given 'form' is not supported.", &
        &   head  = "Initialize_typFileIO", &
        &   value = this%form )
    end if

    !!... Separate the file path
    Call SeparatePath( this%filePath, this%baseDir, this%fileName, this%ext)

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine OpenFile_typFileIO(this, unit)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(inout) :: this
    Integer, intent(out), optional  :: unit
!! -------------------------------------------------------------------------- !!
    Logical :: isFileExist, isFileOpen
    integer :: ios

    if ( .NOT.this%isOpen ) then

        Call GetNewFileUnit( this%unit )

        !!... Check again
        inquire(file   = trim(this%filePath),   &
                exist  = isFileExist,           &
                opened = isFileOpen             )

        !!... Check file is already used in other processor
        if ( isFileOpen ) then
            Call GURU%Error( &
            &   msg   = "File is already opened.", &
            &   head  = "OpenFile_typFileIO",       &
            &   value = trim(this%filePath)         )
        end if

        !!... Check file exist and adjust the environment
        if ( isFileExist ) then
            if ( isStringEqual( this%status, "new") ) then
                Call GURU%Error( &
                &   msg   = "File already exists.", &
                &   head  = "OpenFile_typFileIO",       &
                &   value = trim(this%filePath)         )
            end if
        else

        end if

        !!... Create the directory if not exist
        if ( isStringEqual( this%status,"new").OR.     &
        &    isStringEqual( this%status,"replace").OR. &
        &    isStringEqual( this%status,"scratch").OR. &
        &    isStringEqual( this%status,"unknown")     ) then
            !!... Check base Dir exist
            Call CreateBaseDir( filePath = this%filePath )
        end if

        !!... Open File
        Open(unit   = this%unit,            &
             file   = trim(this%filePath),  &
             status = trim(this%status),    &
             form   = trim(this%form),      &
             iostat = ios  )

        if ( ios.NE.0 ) then
            Call GURU%Error( &
            &   msg  = "Failed to open the file.\n \n "//&
            &"  - path   : "//this%filePath//&
            &"  - unit   : "//Int2Char(this%unit)//&
            &"  - status : "//trim(this%status)//&
            &"  - iostat : "//Int2Char(ios), &
            &   head = "OpenFile_typFileIO")
        End if

        this%isOpen = .TRUE.

    end if

    If (Present(unit)) unit = this%unit

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine FlushFile_typFileIO(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in) :: this
!! -------------------------------------------------------------------------- !!
    if ( this%isOpen ) Call Flush( this%unit )
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine CloseFile_typFileIO(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in) :: this
!! -------------------------------------------------------------------------- !!
    Logical :: isFileOpen
    inquire(unit = this%unit, opened = isFileOpen)
    if ( isFileOpen ) Close( this%unit )
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine DeleteFile_typFileIO(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in) :: this
!! -------------------------------------------------------------------------- !!
    Logical :: isFileExist, isFileOpen, isDelete

    Inquire(file   = trim(this%filePath),   &
    &       exist  = isFileExist,           &
    &       opened = isFileOpen )

    If ( isFileOpen ) then
        Call GURU%Warn( &
        &   msg   = "Failed to delete the file. It is used in another processor.", &
        &   head  = "DeleteFile_typFileIO", &
        &   value = this%filePath )
        return
    end if

    If ( .NOT.isFileExist ) then
        Call GURU%Warn( &
        &   msg   = "Failed to delete the file. The file do not exist.", &
        &   head  = "DeleteFile_typFileIO", &
        &   value = this%filePath )
        return
    end if

    Call DeleteFile( filePath = this%filePath, isDelete = isDelete )

    write(*,*) this%filePath
    write(*,*) isDelete

    If (.NOT.isDelete) then
        Call GURU%Warn( &
        &   msg   = "Failed to delete the file.", &
        &   head  = "DeleteFile_typFileIO", &
        &   value = this%filePath )
    end if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!


!! -------------------------------------------------------------------------- !!
Function IsFileOpen_typFileIO(this) result( isFileOpen )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in) :: this
    Logical :: isFileOpen
!! -------------------------------------------------------------------------- !!
    Inquire(file   = trim(this%filePath),   &
    &       opened = isFileOpen )
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function IsFileExist_typFileIO(this) result( isFileExist )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in) :: this
    Logical :: isFileExist
!! -------------------------------------------------------------------------- !!
    Inquire(file   = trim(this%filePath),   &
    &       exist  = isFileExist )
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function GetFilePath_typFileIO(this) result(filePath)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in)  :: this
    Character(len=:), Allocatable :: filePath
!! -------------------------------------------------------------------------- !!
    filePath = this%filePath
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function GetBaseDir_typFileIO(this) result(baseDir)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in)  :: this
    Character(len=:), Allocatable :: baseDir
!! -------------------------------------------------------------------------- !!
    baseDir = this%baseDir
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function GetFileName_typFileIO(this) result(fileName)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in)  :: this
    Character(len=:), Allocatable :: fileName
!! -------------------------------------------------------------------------- !!
    fileName = this%fileName
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function GetExt_typFileIO(this) result(ext)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in)  :: this
    Character(len=:), Allocatable :: ext
!! -------------------------------------------------------------------------- !!
    ext = this%ext
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function GetUnit_typFileIO(this) result(unit)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in) :: this
    Integer                      :: unit
!! -------------------------------------------------------------------------- !!
    unit = this%unit
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function GetStatus_typFileIO(this) result(status)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in)  :: this
    Character(len=:), Allocatable :: status
!! -------------------------------------------------------------------------- !!
    status = trim(adjustl(this%status))
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function GetForm_typFileIO(this) result(form)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class(typFileIO), intent(in)  :: this
    Character(len=:), Allocatable :: form
!! -------------------------------------------------------------------------- !!
    form = trim(adjustl(this%form))
!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!
