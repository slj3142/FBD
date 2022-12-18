!! -------------------------------------------------------------------------- !!
Subroutine SeparatePath( path, baseDir, baseName, ext)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    character(len=*), Intent(in)  :: path
    character(len=:), Allocatable :: baseDir
    character(len=:), Allocatable :: baseName
    character(len=:), Allocatable :: ext
!! -------------------------------------------------------------------------- !!
    type(string) :: filePath
    Integer :: iStr3, nChar
    Integer :: iStrSep, iStrExt
    Integer :: iStrBase1, iStrBase2
    Logical :: isDirSep, isExtension
!! -------------------------------------------------------------------------- !!

    If (Allocated(baseDir)) Deallocate(baseDir)
    If (Allocated(baseName)) Deallocate(baseName)
    If (Allocated(ext))      Deallocate(ext)

    filePath = path
    nChar = filePath%Len_Trim()

    !!... BaseDir: Set the latest / \ position
    isDirSep = .FALSE.

    !!... Directory Separation is different in Windows and Linux
#ifdef __OS_WINDOWS__
    Block
        Integer :: iStr1, iStr2
        iStr1 = filePath%Scan("/", back=.true.)
        iStr2 = filePath%Scan("\", back=.true.)

        If ( (iStr1.GE.1).OR.(iStr2.GE.1) ) then
            isDirSep = .TRUE.
            iStrSep = iStr1
            If (iStr1.EQ.0) iStrSep = iStr2
        End if
    End Block
#else
    Block
        Integer :: iStr
        iStr = filePath%Scan("/", back=.true.)
        If (iStr.GE.1) then
            isDirSep = .TRUE.
            iStrSep = iStr
        End if
    End Block
#endif

    baseDir = ""
    if (isDirSep.EQV..TRUE.) baseDir = filePath%raw(1:iStrSep)

    !!... Ext: Set the latest . position if it is placed after / or \.
    iStr3 = filePath%Scan(".", back=.true.)
    isExtension = .FALSE.
    iStrExt = 0
    if (isExtension.EQV..TRUE.) then
        if ( (iStr3.GT.0).AND.(iStr3.GT.iStrSep) ) then
            isExtension = .TRUE.
            iStrExt = iStr3
        end if
    else
        if (iStr3.GT.0) then
            isExtension = .TRUE.
            iStrExt = iStr3
        end if
    end if

    ext = ""
    if (isExtension.EQV..TRUE.) ext = filePath%raw(iStrExt:nChar)

    !!... Set BaseName
    iStrBase1 = 1
    iStrBase2 = nChar

    If (isDirSep.EQV..TRUE.)    iStrBase1 = iStrSep + 1
    If (isExtension.EQV..TRUE.) iStrBase2 = iStrExt - 1

    baseName = filePath%raw(iStrBase1:iStrBase2)


!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
