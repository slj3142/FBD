!! -------------------------------------------------------------------------- !!
Subroutine Test_GURUAuxilary()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    Call WritePrefixMessage( &
    &   prefix   = "[Err] ", &
    &   head     = "Test_GURUAuxilary", &
    &   msg      = "Message1&
    &\nMessage2", &
    &   value = "a", &
    &   refValue = 'b', &
    &   unit     = 6,       &
    &   isColor  = .TRUE.   )

    Block

        Integer :: iUnit

        Open(newunit = iUnit, file = "testGURAuxilary.test", status = "replace")

        Call WritePrefixMessage( &
        &   prefix   = "[asds] ", &
        &   head     = "Test_GURUAuxilary", &
        &   msg      = "Message1&
                     &\nMessage2", &
        &   value = "a", &
        &   refValue = 'b', &
        &   unit     = iUnit,       &
        &   isColor  = .TRUE.   )

        Close(iUnit)

    End Block

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
