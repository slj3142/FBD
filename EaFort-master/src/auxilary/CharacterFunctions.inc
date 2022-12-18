Logical Function IsCharEqual( char1, char2, isStrong ) result(isEqual)
    Implicit None
    character(len=*), intent(in) :: char1, char2
    Logical, optional :: isStrong
    !!... Local variables
    Logical :: isStrong_

    !!... Check
    isStrong_ = .FALSE.
    if (present(isStrong)) isStrong_ = isStrong

    !!... Compare two characters
    isEqual = .FALSE.
    if ( isStrong_ ) then
        if (char1.eq.char2) isEqual = .TRUE.
    else
        if (trim(adjustl(lowerChar(char1))).eq.trim(adjustl(lowerChar(char2)))) isEqual = .TRUE.
    end if

End Function

FUNCTION UpperChar(s1)  RESULT (s2)
    Implicit None
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i

    DO i = 1,LEN(s1)
        ch = s1(i:i)
        IF (ch >= 'a'.AND.ch <= 'z') ch = CHAR(ICHAR(ch)+DUC)
        s2(i:i) = ch
    END DO
END FUNCTION

FUNCTION LowerChar(s1)  RESULT (s2)
    Implicit None
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i

    DO i = 1,LEN(s1)
        ch = s1(i:i)
        IF (ch >= 'A'.AND.ch <= 'Z') ch = CHAR(ICHAR(ch)-DUC)
        s2(i:i) = ch
    END DO
END FUNCTION

Subroutine RemovebkslChar(str)
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Removes backslash (\) characters. Double backslashes (\\) are replaced
    ! by a single backslash.
    Implicit None
    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str))::outstr
    integer :: i, k, ibsl, lenstr

    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    k=0
    ibsl=0                        ! backslash initially inactive

    do i=1,lenstr
        ch=str(i:i)
        if(ibsl == 1) then          ! backslash active
            k=k+1
            outstr(k:k)=ch
            ibsl=0
            cycle
        end if
        if(ch == '\') then          ! backslash with backslash inactive
            ibsl=1
            cycle
        end if
        k=k+1
        outstr(k:k)=ch              ! non-backslash with backslash inactive
    end do

    str=adjustl(outstr)

end Subroutine
