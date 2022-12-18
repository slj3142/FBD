!                                   EaFort:GURU                               !!
!! -------------------------------------------------------------------------- !!
!   Math Package
!
!    - Special Packages
!    - Linear Algebra (Interface with Math Kernel Library)
!
!   author: Young-Myung Choi
!   date: 2022-05-01
!
!! -------------------------------------------------------------------------- !!
Module modMathPack_BasicFunc
!! -------------------------------------------------------------------------- !!

    Use modMathPack     !!... Global variables of Math Package

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

!!... Factorial Functions
#include "factorial/factorial.proc"

!!... Binomial Coefficient
#include "binomialCoef/binomialCoef.proc"

!!... Complex Hyperbolic Functions
#include "hyperBolicFunctions/hyperBolicFunctions.proc"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Factorial Functions
#include "factorial/factorial.inc"

!!... Binomial Coefficient
#include "binomialCoef/binomialCoef.inc"

!!... Complex Hyperbolic Functions
#include "hyperBolicFunctions/hyperBolicFunctions.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
