!                                   EaFort:GURU                               !!
!! -------------------------------------------------------------------------- !!
!   GURU to handle the fortran program
!
!    - Error / Warning Message
!    - Argument by command line
!    - Explanation of program
!
!   author: Young-Myung Choi
!   date: 2021-10-20
!
!! -------------------------------------------------------------------------- !!
Module modGURU
!! -------------------------------------------------------------------------- !!

    Use modEaFort       !!... Global variables of EaFort
    Use modOS           !!... Use OS Command

    Use FACE, only: Colorize    !!... Third Party String Module (Colorize)

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Parameters
    Integer, PARAMETER          :: LEN_COLOR = 17
    Character(len=2), Private, Parameter :: lineSep = "\n"

    !!... Prefix before message
    Character(len=8), Parameter, Private  :: PREIX_ERROR = "[ERROR] "
    Character(len=10), Parameter, Private :: PREIX_WARN  = "[WARNING] "
    Character(len=8), Parameter, Private  :: PREIX_DEBUG = "[DEBUG] "

    Character(len=11), Parameter, Private   :: PREFIX_GIVEN = "Given    : "
    Character(len=11), Parameter, Private   :: PREFIX_REF   = "Referene : "

    Character(len=2), Parameter, Private :: INDENT  = "  "
    Character(len=4), Parameter, Private :: INDENT2 = "    "
    Character(len=6), Parameter, Private :: INDENT3 = "      "
    Character(len=8), Parameter, Private :: INDENT4 = "        "

    Character(len=LEN_COLOR), Parameter, Private :: COLOR_ERROR = "RED_INTENSE"
    Character(len=LEN_COLOR), Parameter, Private :: COLOR_WARN  = "YELLOW_INTENSE"
    Character(len=LEN_COLOR), Parameter, Private :: COLOR_DEBUG = "CYAN_INTENSE"
    Character(len=LEN_COLOR), Parameter, Private :: COLOR_HEAD  = "GREEN"

    Character(len=5) :: INFO_ERROR = "error"
    Character(len=5) :: INFO_DEBUG = "debug"
    Character(len=4) :: INFO_WARN  = "warn"

    !!... Log File Max
    Integer, Parameter :: N_MAX_LOG_FILE = 100

!!... Write Messages
#include "auxiliary/WriteMessage.proc"

    !!... Private
    ! Private :: WriteMessage, WritePrefixMessage

!!... Arguments Class Definition
#include "argGURU/CommandArgument/CommandArgument.typ"

!!... Class Definition
#include "GURU/GURU.typ"

    Type(typGURU), target :: GURU


!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Write Messages
#include "auxiliary/WriteMessage.inc"

!!... Arguments Class Functions
#include "argGURU/CommandArgument/CommandArgument.inc"

!!... Arguments GURU Class Functions
#include "argGURU/argGURU.inc"

!!... Log GURU Class Functions
#include "logGURU/logGURU.inc"

!!... Class Functions
#include "GURU/GURU.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
