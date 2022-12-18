!! -------------------------------------------------------------------------- !!
!! EaFort:testString
!! -------------------------------------------------------------------------- !!
!! Testing Program of String Module
!!
!!   Author: Young-Myung Choi
!!   Date  : 2021-11-05
!!
!! -------------------------------------------------------------------------- !!
Program testString
!! -------------------------------------------------------------------------- !!

    Use testStringModule

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!
    ! Integer, Allocatable :: intVec(:)
    ! Real(4), Allocatable :: realVec(:)
    ! Real(8), Allocatable :: dbleVec(:)
    ! Logical, Allocatable :: logVec(:)
    ! type(string)         :: str
    ! Logical :: isError
    !
    ! ! Allocate(char(10)(2))
    ! Allocate(intVec(8))
    ! Allocate(realVec(2))
    ! Allocate(dbleVec(2))
    ! Allocate(logVec(2))
    !
    ! intVec(1) = -1
    ! intVec(2) = -53
    !
    ! Call GetStrFromClassVector( arg = intVec, str = str, isError = isError)
    ! write(*,*) str%raw
    !
    ! Call GetStrFromClassVector( arg = realVec, str = str, isError = isError)
    ! write(*,*) str%raw
    !
    ! Call GetStrFromClassVector( arg = dbleVec, str = str, isError = isError)
    ! write(*,*) str%raw
    !
    ! Call GetStrFromClassVector( arg = logVec, str = str, isError = isError)
    ! write(*,*) str%raw

    ! !!... Test Single Conversion
    ! Call Test_String_SingleConversion()
    !
    ! !!... Test Split String
    ! Call Test_String_Split()
    !
    !!... Test Vector Conversion
    ! Call Test_String_VectorConversion()

    Call Test_VarToStringConv()

!! -------------------------------------------------------------------------- !!
End Program
!! -------------------------------------------------------------------------- !!
