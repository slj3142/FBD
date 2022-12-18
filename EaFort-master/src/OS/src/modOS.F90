!                                   EaFort:String                             !!
!! -------------------------------------------------------------------------- !!
!
!   OS module to control dir/files
!
!   author: Young-Myung Choi
!   date: 2021-10-20
!
!! -------------------------------------------------------------------------- !!
Module modOS
!! -------------------------------------------------------------------------- !!

    Use modEaFort       !!... Global variables of EaFort
    Use stringifor      !!... Third Party String Module

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

#ifdef __OS_LINUX__
Character(len=1), Parameter :: CHAR_DIR_SEP = "/"
#else
Character(len=1), Parameter :: CHAR_DIR_SEP = "\"
#endif

!!... Control the file system procedure definitions
#include "fileSystem/fileSystem.proc"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Control the file system
#include "fileSystem/fileSystem.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
