!! -------------------------------------------------------------------------- !!
Subroutine Test_OS_fileSystem()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    Character(len=:), Allocatable :: dirPath, dirPath2, dirPath3
    Logical :: isExist

    dirPath  = "a"
    dirPath2 = "b"
    dirPath3 = "c"

    Block
        Logical :: isError, isExist, isCreate
        Call CreateDir( &
        &   dirPath  = dirPath, &
        &   isError  = isError, &
        &   isExist  = isExist, &
        &   isCreate = isCreate )
    End Block

    block
        integer :: ioUnit
        Open(newunit = ioUnit, file = dirPath//"/file1", status = "replace")
        close( ioUnit )
    end block

    Call CopyDirFile( dirPath, dirPath2 )
    isExist = IsExistDir( dirPath );     write(*,*) "isExist1 : ", isExist
    isExist = IsExistDir( dirPath2 );    write(*,*) "isExist2 : ", isExist
    isExist = IsExistDir( dirPath3 );    write(*,*) "isExist3 : ", isExist
    write(*,*)

    block
        integer :: ioUnit
        Open(newunit = ioUnit, file = dirPath2//"/file2", status = "replace")
        close( ioUnit )
    end block

    Call RenameDirFile( dirPath2, dirPath3 )
    isExist = IsExistDir( dirPath );     write(*,*) "isExist1 : ", isExist
    isExist = IsExistDir( dirPath2 );    write(*,*) "isExist2 : ", isExist
    isExist = IsExistDir( dirPath3 );    write(*,*) "isExist3 : ", isExist
    write(*,*)

    Call DeleteDir( dirPath )
    Call DeleteDir( dirPath2 )
    Call DeleteDir( dirPath3 )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
