!! -------------------------------------------------------------------------- !!
Subroutine CreateBaseDir( filePath )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in) :: filePath
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: filePath_, baseDir, fileName, ext
    Logical                       :: isBaseDirExist
    Logical                       :: isCreate, isError

    filePath_ = trim(adjustl(filePath))

    !!... Separate path
    Call SeparatePath( filePath_, baseDir, fileName, ext)

    If ( len_trim(baseDir).ge.1 ) then

        isBaseDirExist = IsExistDir( baseDir )

        if (.NOT.isBaseDirExist) then
            Call CreateDir( &
            &   dirPath  = baseDir, &
            &   isError  = isError, &
            &   isCreate = isCreate )

            If ( (isError.EQV..TRUE.).OR.(isCreate.EQV..FALSE.) ) then
                Call GURU%Error( &
                &   msg   = "Failed to create the base directory.", &
                &   head  = "CreateBaseDir",                        &
                &   value = trim(filePath_)                         )
            End if

        end if

    End if

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
