!! -------------------------------------------------------------------------- !!
!! EaFort:testGURU
!! -------------------------------------------------------------------------- !!
!! Testing Program of InfoFortran Module
!!
!!   Author: Young-Myung Choi
!!   Date  : 2021-12-09
!!
!! -------------------------------------------------------------------------- !!
Program testAuxFunc
!! -------------------------------------------------------------------------- !!

    Use testAuxFuncModule

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Test Character Functions
    !Call Test_CharacterFunctions()

    !!... Test Single Value Function
    !Call Test_GetSingleValue()

    !!... Test Single Value Function Or Default
    ! Call Test_GetSingleValueOrDefault()

    !!... Test Vector Value Function
    ! Call Test_GetVectorValue()

    !!... Test Vector Value Function
    Call test_GetVectorValueOrDefault()

!! -------------------------------------------------------------------------- !!
End Program
!! -------------------------------------------------------------------------- !!
