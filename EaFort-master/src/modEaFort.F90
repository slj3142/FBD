!! -------------------------------------------------------------------------- !!
!                                   EaFort:Global                             !!
!! -------------------------------------------------------------------------- !!
!   Wrapper Module of JSON-Fortran for easy use.
!
!   author: Young-Myung Choi
!   date: 2021-10-20
!
!! -------------------------------------------------------------------------- !!
Module modEaFort
!! -------------------------------------------------------------------------- !!

    !!... String Module
    Use stringifor              !!... Third Party String Module

    !!... IEEE Module
    Use IEEE_ARITHMETIC

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Integer Precision
    Integer, Parameter :: IP = KIND(1)

    !!... Real Precision
    Integer, Parameter :: RP = KIND(1.D0)

    !!... Complex Precision
    Integer, Parameter :: CP = RP


!!... Get String from arbirary class
#include "auxilary/GetStrFromClass.proc"

!!... Character Function
#include "auxilary/CharacterFunctions.proc"

!!... Convert from character to variables
#include "auxilary/GetValueFromChar.proc"

!!... Separate file path
#include "auxilary/SeparatePath.proc"

!!... IEEE Functions
#include "IEEE/ieee_functions.proc"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Get String from arbirary class
#include "auxilary/GetStrFromClass.inc"

!!... Character Function
#include "auxilary/CharacterFunctions.inc"

!!... Convert from character to variables
#include "auxilary/GetValueFromChar.inc"

!!... Separate file path
#include "auxilary/SeparatePath.inc"

!!... IEEE Functions
#include "IEEE/ieee_functions.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
