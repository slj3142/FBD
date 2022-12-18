!! -------------------------------------------------------------------------- !!
!! EaFort:testAuxFuncModule
!! -------------------------------------------------------------------------- !!
!! Testing Program of Auxilary functions
!!
!!   Author: Young-Myung Choi
!!   Date  : 2021-12-09
!!
!! -------------------------------------------------------------------------- !!
Module testAuxFuncModule
!! -------------------------------------------------------------------------- !!

    Use pkgEaFort

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Test Chracter Functions
#include "tests/test_CharacterFunctions.inc"

!!... Test Get Single Value functions
#include "tests/test_GetSingleValue.inc"

!!... Test Get Single Value functions Or Default
#include "tests/test_GetSingleValueOrDefault.inc"

!!... Test Get Vector Value Functions
#include "tests/test_GetVectorValue.inc"

!!... Test Get Vector Value Functions Or Default
#include "tests/test_GetVectorValueOrDefault.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
