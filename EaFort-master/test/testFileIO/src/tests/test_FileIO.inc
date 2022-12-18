!! -------------------------------------------------------------------------- !!
Subroutine Test_FileIO()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Type(typFileIO)               :: file1
    Integer                       :: unit1, unit2
    Logical                       :: IsFileOpen, IsFileExist
    Character(len=:), Allocatable :: filePath, baseDir, fileName, ext

    !!... Initialize
    Call file1%Initialize( &
    &   filePath = "tmp/file.test", &
    &   status   = "replace"    )

    isFileOpen  = file1%IsFileOpen()
    IsFileExist = file1%IsFileExist()
    write(*,*) "isFileOpen  : ", isFileOpen
    write(*,*) "IsFileExist : ", IsFileExist
    write(*,*)

    filePath = file1%GetFilePath()
    baseDir  = file1%GetBaseDir()
    fileName = file1%GetFileName()
    ext      = file1%GetExt()
    write(*,*) "filePath : ", filePath
    write(*,*) "baseDir  : ", baseDir
    write(*,*) "fileName : ", fileName
    write(*,*) "ext      : ", ext
    write(*,*)

    !!... Open File
    Call file1%OpenFile( unit = unit1 )
    write(*,*) "unit1        : ", unit1
    unit2 = file1%GetUnit()
    write(*,*) "unit1(Check) : ", unit2
    write(*,*)
    isFileOpen  = file1%IsFileOpen()
    IsFileExist = file1%IsFileExist()
    write(*,*) "isFileOpen  : ", isFileOpen
    write(*,*) "IsFileExist : ", IsFileExist
    write(*,*)
    write(unit1, *) "A"
    Call file1%FlushFile()
    Call file1%CloseFile()
    isFileOpen  = file1%IsFileOpen()
    IsFileExist = file1%IsFileExist()
    write(*,*) "isFileOpen  : ", isFileOpen
    write(*,*) "IsFileExist : ", IsFileExist
    write(*,*)

    Call file1%DeleteFile()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
