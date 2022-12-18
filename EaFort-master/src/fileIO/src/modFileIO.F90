!                                   EaFort:FileIO                             !!
!! -------------------------------------------------------------------------- !!
!   File IO Module
!
!    - File I/O Handling
!
!   author: Young-Myung Choi
!   date: 2021-03-07
!
!! -------------------------------------------------------------------------- !!
Module modFileIO
!! -------------------------------------------------------------------------- !!

    Use modEaFort       !!... Global variables of EaFort
    Use modGURU         !!... GURU Module
    Use modString       !!... String Module
    Use modOS           !!... OS Module

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... file index (input port)
    Integer, Parameter :: F_UNIT_IN_DEFAULT = 5

    !!... file index (output port)
    Integer, Parameter :: F_UNIT_OUT_DEFAULT = 6

    !!... file index to begin
    Integer, Parameter :: F_UNIT_BEGIN = 1317

    Integer :: F_UNIT_CURRENT

#include "auxilary/GetNewFileUnit.proc"

#include "auxilary/createBaseDir.proc"

!!... File IO Class definitions
#include "fileIO/fileIO.typ"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

#include "auxilary/GetNewFileUnit.inc"

#include "auxilary/createBaseDir.inc"

!!... File IO Class functions
#include "fileIO/fileIO.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
