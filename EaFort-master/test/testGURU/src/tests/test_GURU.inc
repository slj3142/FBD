!! -------------------------------------------------------------------------- !!
Subroutine Test_GURU()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    Call GURU%Initialize( &
    &   projectName = "Test_GURU", &
    &   description = "Testing GURU \n&
                    &Test Line1", &
    &   authors     = "Young-Myung CHOI\n \n&
                    &  Dept. Naval Architecture and Ocean Engineering,\n&
                    &  Pusan National University, Republic of Korea.",  &
    &   date        = "09 December 2021",   &
    &   version     = "0.0.1"               )

    !!... Add Arguments
    Call GURU%AddKey( &
    &   key           = "input",              &
    &   singleKey     = "i",                  &
    &   description   = "Input File Path",    &
    &   nRequiredWord = 1                     )

    !!... Add Arguments
    Call GURU%AddKey(             &
    &   key           = "output",             &
    &   singleKey     = "o",                  &
    &   description   = "Output File Path"    )

    !!... Print & Print out
    Call GURU%UpdateAndPrint()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
