!! -------------------------------------------------------------------------- !!
!! EaFort:testMathPack
!! -------------------------------------------------------------------------- !!
!! Testing Program of MathPack Module
!!
!!   Author: Young-Myung Choi
!!   Date  : 2021-11-05
!!
!! -------------------------------------------------------------------------- !!
Program testMathPack
!! -------------------------------------------------------------------------- !!

    Use testMathPackModule

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Test Factorial
    ! Call testFactorial()

    !!... Test Binomial Coefficient
    Call testBinomialCoef()

    !!... Test Bessel Function
    ! Call testBesselFunc()

    !!... Test Derivatives of Bessel Function
    ! Call testBesselFuncDeriv()

    !!... Test Cross Product
    ! Call testCrossProduct3()

    !!... Test Matrix Inverse
    ! Call testMatrixInv()

!! -------------------------------------------------------------------------- !!
End Program
!! -------------------------------------------------------------------------- !!
