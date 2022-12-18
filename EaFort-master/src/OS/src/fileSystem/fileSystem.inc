!! -------------------------------------------------------------------------- !!
Subroutine CreateDir( dirPath, isError, isExist, isCreate )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in) :: dirPath
    Logical, Optional            :: isError
    Logical, Optional            :: isExist
    Logical, Optional            :: isCreate
!! -------------------------------------------------------------------------- !!
    Logical :: isError_, isExist_, isCreate_

    isError_  = .FALSE.
    isExist_  = .FALSE.
    isCreate_ = .FALSE.

    !!... Check the given 'dirPath' has a length
    if ( len_trim(dirPath).GE.1 ) Then

        !!... Check the directory exists
        isExist_ = IsExistDir( dirPath )

        !!... Create directory if not exist
        If ( .NOT.isExist_ ) then
#ifdef __OS_LINUX__
            Call System( "mkdir -p "//trim(dirPath) )
#else
            Call System( "mkdir "//trim(dirPath) )
#endif
            !!... Check the directory is created
            isCreate_ = IsExistDir( dirPath )
        End if
    else
        isError_ = .TRUE.
    end if

    If ( Present(isError) ) isError = isError_
    If ( Present(isExist) ) isExist = isExist_
    If ( Present(isCreate) ) isCreate = isCreate_

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine DeleteDir( dirPath, isDelete )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in) :: dirPath
    Logical, Optional            :: isDelete
!! -------------------------------------------------------------------------- !!
    Logical :: isExist_, isDelete_

    isDelete_ = .TRUE.
    if ( len_trim(dirPath).GE.1 ) Then

        !!... Check the directory exists
        isExist_ = IsExistDir( dirPath )

        If ( isExist_ ) then
#ifdef __OS_LINUX__
            Call System( "rm -rf "//trim(dirPath) )
#else
            Call System( "del /s "//trim(dirPath) )
#endif
            !!... Check the directory exists
            isExist_ = IsExistDir( dirPath )
        end if

        isDelete_ = .NOT.isExist_

    end if

    If ( Present(isDelete) ) isDelete = isDelete_

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine DeleteFile( filePath, isDelete )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in) :: filePath
    Logical, Optional            :: isDelete
!! -------------------------------------------------------------------------- !!
    Logical :: isExist_, isDelete_

    isDelete_ = .TRUE.
    if ( len_trim(filePath).GE.1 ) Then

        !!... Check the directory exists
        isExist_ = IsExistFile( filePath )

        If ( isExist_ ) then
#ifdef __OS_LINUX__
            Call System( "rm -rf "//trim(filePath) )
#else
            Call System( "del /s /q "//trim(filePath) )
#endif
            !!... Check the directory exists
            isExist_ = IsExistFile( filePath )
        end if

        isDelete_ = .NOT.isExist_

    end if

    If ( Present(isDelete) ) isDelete = isDelete_

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine CopyDirFile( orgPath, newPath )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in) :: orgPath
    Character(len=*), intent(in) :: newPath
!! -------------------------------------------------------------------------- !!

    if ( len_trim(orgPath).LE.0 ) Then
        write(*, "(a)") "[ERROR] CopyDirFile"
        write(*, "(a)") "  Given 'orgPath' is blank."
        write(*,*)
        stop
    end if

    if ( len_trim(newPath).LE.0 ) Then
        write(*, "(a)") "[ERROR] CopyDirFile"
        write(*, "(a)") "  Given 'newPath' is blank."
        write(*,*)
        stop
    end if

    If ( IsExistDir( orgPath ).OR.IsExistFile( orgPath ) ) then
#ifdef __OS_LINUX__
        Call System( "cp -rf "//trim(orgPath)//" "//trim(newPath) )
#else
        Call System( "xcopy  /Y "//trim(orgPath)//" "//trim(newPath) )
#endif
    else
        write(*, "(a)") "[ERROR] CopyDirFile"
        write(*, "(a)") "  Given 'orgPath' exist not."
        write(*,*)
        stop
    end if

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine RenameDirFile( orgPath, newPath )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in) :: orgPath
    Character(len=*), intent(in) :: newPath
!! -------------------------------------------------------------------------- !!

    if ( len_trim(orgPath).LE.0 ) Then
        write(*, "(a)") "[ERROR] RenameDirFile"
        write(*, "(a)") "  Given 'orgPath' is blank."
        write(*,*)
        stop
    end if

    if ( len_trim(newPath).LE.0 ) Then
        write(*, "(a)") "[ERROR] RenameDirFile"
        write(*, "(a)") "  Given 'newPath' is blank."
        write(*,*)
        stop
    end if

    If ( IsExistDir( orgPath ).OR.IsExistFile( orgPath ) ) then
#ifdef __OS_LINUX__
        Call System( "mv "//trim(orgPath)//" "//trim(newPath) )
#else
        Call System( "rename "//trim(orgPath)//" "//trim(newPath) )
#endif
    else
        write(*, "(a)") "[ERROR] RenameDirFile"
        write(*, "(a)") "  Given 'orgPath' exist not."
        write(*,*)
        stop
    end if

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function IsExistDir( dirPath ) result( isExist )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in) :: dirPath
    Logical                      :: isExist
!! -------------------------------------------------------------------------- !!

    if ( len_trim(dirPath).LE.0 ) Then
        write(*, "(a)") "[ERROR] IsExistDir"
        write(*, "(a)") "  Given 'dirPath' is blank."
        write(*,*)
        stop
    end if

#ifdef __INTEL_FORTRAN__
        Inquire( directory = trim(dirPath), exist = isExist )
#else
        Inquire( file = trim(dirPath)//"/.", exist = isExist )
#endif

!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Function IsExistFile( filePath ) result( isExist )
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Character(len=*), intent(in) :: filePath
    Logical                      :: isExist
!! -------------------------------------------------------------------------- !!

    if ( len_trim(filePath).LE.0 ) Then
        write(*, "(a)") "[ERROR] IsExistFile"
        write(*, "(a)") "  Given 'filePath' is blank."
        write(*,*)
        stop
    end if

#ifdef __INTEL_FORTRAN__
        Inquire( file = trim(filePath), exist = isExist )
#else
        Inquire( file = trim(filePath), exist = isExist )
#endif

!! -------------------------------------------------------------------------- !!
End Function
!! -------------------------------------------------------------------------- !!
