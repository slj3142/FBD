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
Module modMathPack_LinearAlgebra
!! -------------------------------------------------------------------------- !!

    Use modMathPack     !!... Global variables of Math Package

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

!!... Vector Operation
#include "vector/crossProduct.proc"

#include "matrix/SolveSquareMatrix.proc"

#include "matrix/SolveRectMatrix.proc"

#include "matrix/InvMat.proc"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Vector Operation
#include "vector/crossProduct.inc"

#include "matrix/SolveSquareMatrix.inc"

#include "matrix/SolveRectMatrix.inc"

#include "matrix/InvMat.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
