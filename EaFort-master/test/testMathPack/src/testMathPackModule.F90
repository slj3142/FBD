!! -------------------------------------------------------------------------- !!
!! EaFort:testMathPackModule
!! -------------------------------------------------------------------------- !!
!! Testing Program of MathPack Module
!!
!!   Author: Young-Myung Choi
!!   Date  : 2021-12-03
!!
!! -------------------------------------------------------------------------- !!
Module testMathPackModule
!! -------------------------------------------------------------------------- !!

    Use pkgEaFort

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

#include "tests/basicFunctions/testFactorial.inc"

#include "tests/basicFunctions/testBinomialCoef.inc"

#include "tests/specialFunctions/testBesselFunc.inc"

#include "tests/specialFunctions/testBesselFuncDeriv.inc"

#include "tests/linearAlgebra/testCrossProduct3.inc"

#include "tests/linearAlgebra/testMatrixInv.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
