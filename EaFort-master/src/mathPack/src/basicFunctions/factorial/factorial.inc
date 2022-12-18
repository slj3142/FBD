Function Factorial_Real(n) result( funcValue )
    Implicit None
    Real(RP), intent(in) :: n
    Real(RP)             :: funcValue
    Integer              :: ii

    If ( n.lt.0.0_RP ) then
        Call GURU%Error( &
        &   msg   = "Given 'n' should be larger or equal to zero.", &
        &   head  = "Factorial_Real", &
        &   value = n)
    end if

    if ( mod(n, 1.0_RP).ge.1.D-1 ) then
        Call GURU%Error( &
        &   msg   = "Given 'n' should be an integer", &
        &   head  = "Factorial_Real", &
        &   value = n)
    end if

    funcValue = 1.0_RP
    do ii = 1, nint(n)
        funcValue = funcValue * real(ii, RP)
    end do

    If ( .NOT.IsFinite(funcValue) ) then
        Call GURU%Error( &
        &   msg   = "The result of 'Factorial' is too large. Please increase the precision.", &
        &   head  = "Factorial_Real", &
        &   value = nint(n) )
    End if

End Function

Function Factorial_Int(n) result( funcValue )
    Implicit None
    Integer, intent(in) :: n
    Real(RP)            :: funcValue
    Integer :: ii

    If ( n.lt.0 ) then
        Call GURU%Error( &
        &   msg   = "Given 'n' should be larger or equal to zero.", &
        &   head  = "Factorial_Int", &
        &   value = n)
    end if

    funcValue = 1.0_RP
    do ii = 1, n
        funcValue = funcValue * real(ii, RP)
    end do

    If ( .NOT.IsFinite(funcValue) ) then
        Call GURU%Error( &
        &   msg   = "The result of 'Factorial' is too large. Please increase the precision.", &
        &   head  = "Factorial_Int", &
        &   value = n )
    End if

End Function
