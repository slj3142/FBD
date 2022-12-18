!! -------------------------------------------------------------------------- !!
Subroutine Test_LogGURU()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    Call GURU%Initialize_LOG( &
    &   logFilePath = "./log/testLogGURU.log",    &
    &   isDebug     = .TRUE.,               &
    &   isColor     = .TRUE.                )

    Call GURU%Write( &
    &   msg = "Message 1")

    Call GURU%Write( &
    &   msg   = "Message 2 \nNext Line", &
    &   color = "Blue"       )

    Call GURU%Write( &
    &   msg    = "Message 3 \nNext Line", &
    &   nSpace = 4       )

    Call GURU%Write( &
    &   msg    = "Message 4 \nNext Line", &
    &   color  = "Green", &
    &   nSpace = 2, &
    &   isAdvance = .FALSE.     )


    Call GURU%Warn( &
    &   msg    = "Err Message 1 \nErrMessage 2", &
    &   head   = "Test_LogGURU", &
    &   value  = 1,              &
    &   refValue = "Cashe"      )

    Call GURU%Debug( &
    &   msg    = "Err Message 1 \nErrMessage 2", &
    &   head   = "Test_LogGURU", &
    &   value  = 1,              &
    &   refValue = 2.0          )

    Call GURU%Error( &
    &   msg    = "Err Message 1 \nErrMessage 2", &
    &   head   = "Test_LogGURU", &
    &   isStop = .FALSE.)

    Call GURU%Error( &
    &   msg    = "Err Message 1 \nErrMessage 2", &
    &   head   = "Test_LogGURU", &
    &   value  = 1,              &
    &   isStop = .FALSE.        )

    Call GURU%Error( &
    &   msg    = "Err Message 1 \nErrMessage 2", &
    &   head   = "Test_LogGURU", &
    &   value  = 3.D0,              &
    &   refValue = .FALSE.,          &
    &   isStop = .TRUE.        )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
