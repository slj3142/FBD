Function BinomialCoef(n, k) result( funcValue )
    Implicit None
    Integer, intent(in) :: n, k
    Real(RP)            :: funcValue
    Integer :: ii

    If ( n.lt.0 ) then
        Call GURU%Error( &
        &   msg   = "Given 'n' should be larger or equal to zero.", &
        &   head  = "BinomialCoef", &
        &   value = n)
    end if

    If ( k.lt.0 ) then
        Call GURU%Error( &
        &   msg   = "Given 'k' should be larger or equal to zero.", &
        &   head  = "BinomialCoef", &
        &   value = n)
    end if

    If ( k.gt.n ) then
        Call GURU%Error( &
        &   msg   = "Given 'k' should be equal or less than 'n'. [n, k]", &
        &   head  = "BinomialCoef", &
        &   value = "n : "//Int2Char(n)//", k : "//Int2Char(k) )
    end if

    funcValue = 1.0_RP
    do ii = 1, k
        funcValue = funcValue * real(n+1-ii, RP) / real(ii, RP)
    end do

    If ( .NOT.IsFinite(funcValue) ) then
        Call GURU%Error( &
        &   msg   = "The result of 'BinomialCoef' is too large. Please increase the precision. [n,k]", &
        &   head  = "BinomialCoef", &
        &   value = "n : "//Int2Char(n)//", k : "//Int2Char(k) )
    End if

End Function
