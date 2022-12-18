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
Module modMathPack_SpecialFunc
!! -------------------------------------------------------------------------- !!

    Use modMathPack     !!... Global variables of Math Package

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

!!... Bessel Function of the first kind
#include "besselFunc/bessel_J.proc"

!!... Bessel Function of the second kind
#include "besselFunc/bessel_Y.proc"

!!... Modified Bessel Function of the first kind
#include "besselFunc/bessel_I.proc"

!!... Modified Bessel Function of the second kind
#include "besselFunc/bessel_K.proc"

!!... Hankel Functions
#include "besselFunc/hankel.proc"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Bessel Function of the first kind
#include "besselFunc/bessel_J.inc"

!!... Bessel Function of the second kind
#include "besselFunc/bessel_Y.inc"

!!... Modified Bessel Function of the first kind
#include "besselFunc/bessel_I.inc"

!!... Modified Bessel Function of the second kind
#include "besselFunc/bessel_K.inc"

!!... Hankel Functions
#include "besselFunc/hankel.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
