!! -------------------------------------------------------------------------- !!
Type :: typFileIO
!! -------------------------------------------------------------------------- !!

    Private

    !!... File Path
    Character(len=:), Allocatable :: filePath

    !!... Base path
    Character(len=:), Allocatable :: baseDir

    !!... File Name
    Character(len=:), Allocatable :: fileName

    !!... Extension
    Character(len=:), Allocatable :: ext

    !!... File index
    Integer :: unit = -1

    !!... File Status = ("replace", "old", "new", ...)
    Character(len=30) :: status

    !1... File Format
    Character(len=30) :: form

    !!... File is already opened
    Logical :: isOpen = .FALSE.

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

    !!... Initialize the class
    Procedure, Public :: Initialize => Initialize_typFileIO

    !!... Open File
    Procedure, Public :: OpenFile => OpenFile_typFileIO

    !!... Flush File
    Procedure, Public :: FlushFile => FlushFile_typFileIO

    !!... Close File
    Procedure, Public :: CloseFile => CloseFile_typFileIO

    !!... Delete File
    Procedure, Public :: DeleteFile => DeleteFile_typFileIO

    !!... Check File is already open
    Procedure, Public :: IsFileOpen => IsFileOpen_typFileIO

    !!... Check File already exist
    Procedure, Public :: IsFileExist => IsFileExist_typFileIO

    !!... Get file path
    Procedure, Public :: GetFilePath => GetFilePath_typFileIO

    !!... Get file path
    Procedure, Public :: GetBaseDir => GetBaseDir_typFileIO

    !!... Get file path
    Procedure, Public :: GetFileName => GetFileName_typFileIO

    !!... Get file path
    Procedure, Public :: GetExt => GetExt_typFileIO

    !!... Get FID
    Procedure, Public :: GetUnit => GetUnit_typFileIO

    !!... Get file path
    Procedure, Public :: GetStatus => GetStatus_typFileIO

    !!... Get file path
    Procedure, Public :: GetForm => GetForm_typFileIO

!! -------------------------------------------------------------------------- !!
End Type
!! -------------------------------------------------------------------------- !!
