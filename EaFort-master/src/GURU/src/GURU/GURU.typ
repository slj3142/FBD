!! -------------------------------------------------------------------------- !!
Type typGURU
!! -------------------------------------------------------------------------- !!

    Private

!! -------------------------------------------------------------------------- !!
!!  Arguments Variables
!! -------------------------------------------------------------------------- !!

    !!... Project name
    Type(string) :: projectName

    !!... Application Description
    Type(string) :: description

    !!... Authors
    Type(string) :: authors

    !!... Date
    Type(string) :: date

    !!... Date
    Type(string) :: version

    !!... The number of arguments
    Integer :: nArguments = 0

    !!... Required Arguments
    Type(typCommandArgument), allocatable :: arguments(:)

!! -------------------------------------------------------------------------- !!
!!  Logging Variables
!! -------------------------------------------------------------------------- !!

    !!... Write the message to the log file
    Logical :: isLogFile = .FALSE.

    !!... Log File Path
    character(len=:), Allocatable :: logFilePath_

    !!... Default Output
    Integer, Public :: logUnit = 11

    !!... Is Debug Mode (Write the message when the debug mode is enabled)
    Logical :: isDebug_ = .FALSE.
    ! Logical :: isDebug_ = .TRUE.

    !!... Is Color Mode (Write the message when the debug mode is enabled)
    Logical :: isColor_ = .FALSE.

    !!... MPI Variable representing the master
    Logical :: MPI_isMaster_ = .TRUE.

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

    !!... Initialize GURU Class (Easy version to Initialize)
    Procedure :: Initialize => Initialize_typGURU

!! -------------------------------------------------------------------------- !!
!!  Arguments Routines
!! -------------------------------------------------------------------------- !!

    !!... Initialize the description
    Procedure :: Initialize_ARG => ARG_Initialize_typGURU

    !!... Add Argument
    Procedure :: AddKey => ARG_AddKey_typGURU

    !!... Update Arguments
    Procedure :: Update_ARG => ARG_Update_ARG_typGURU

    !!... Update and Print
    Procedure :: UpdateAndPrint => ARG_UpdateAndPrint_typGURU

    !!... Print the description
    Procedure :: PrintDescription => ARG_PrintDescription_typGURU

    !!... Print Help
    Procedure :: PrintHelp => ARG_PrintHelp_typGURU

    !!... Print the description
    Procedure :: PrintGivenKey => ARG_PrintGivenKey_typGURU

    !!... Destroy the class
    Procedure :: Destroy_ARG => ARG_Destroy_typGURU

    !!... Found Key
    Procedure :: IsFoundKey => ARG_IsFoundKey_typGURU

    !!... Get the number of keys
    Procedure :: GetNKey => ARG_GetNKey_typGURU

    !!... Get key for given index
    Procedure :: GetKey => ARG_GetKey_typGURU

    !!... Get nWord for given key
    Procedure :: GetNWord_Key => ARG_GetNWord_Key_typGURU
    Procedure :: GetNWord_iKey => ARG_GetNWord_iKey_typGURU
    Generic   :: GetNWord => GetNWord_Key, GetNWord_iKey

    !!... Get Arguments by keyword and its index(option)
    Procedure :: GetArgInt     => ARG_GetArgInt_typGURU
    Procedure :: GetArgReal    => ARG_GetArgReal_typGURU
    Procedure :: GetArgLogical => ARG_GetArgLogical_typGURU
    Procedure :: GetArgChar    => ARG_GetArgChar_typGURU

    !!... Get Argument
    Generic :: GetArg => GetArgInt,     &
    &                    GetArgReal,    &
    &                    GetArgLogical, &
    &                    GetArgChar

    !!... Get Arguments by keyword and its index(option) or default value
    Procedure :: GetArgIntOrDefault     => ARG_GetArgIntOrDefault_typGURU
    Procedure :: GetArgRealOrDefault    => ARG_GetArgRealOrDefault_typGURU
    Procedure :: GetArgLogicalOrDefault => ARG_GetArgLogicalOrDefault_typGURU
    Procedure :: GetArgCharOrDefault    => ARG_GetArgCharOrDefault_typGURU

    !!... Get Arguments vector by keyword
    Procedure :: GetArgIntVector     => ARG_GetArgIntVector_typGURU
    Procedure :: GetArgRealVector    => ARG_GetArgRealVector_typGURU
    Procedure :: GetArgLogicalVector => ARG_GetArgLogicalVector_typGURU
    Procedure :: GetArgCharVector    => ARG_GetArgCharVector_typGURU

    !!... Get Arguments vector by keyword or default
    Procedure :: GetArgIntVectorOrDefault     => ARG_GetArgIntVectorOrDefault_typGURU
    Procedure :: GetArgRealVectorOrDefault    => ARG_GetArgRealVectorOrDefault_typGURU
    Procedure :: GetArgLogicalVectorOrDefault => ARG_GetArgLogicalVectorOrDefault_typGURU
    Procedure :: GetArgCharVectorOrDefault    => ARG_GetArgCharVectorOrDefault_typGURU

!! -------------------------------------------------------------------------- !!
!!  Logging Routines
!! -------------------------------------------------------------------------- !!

    !!... Initialize the Log GURU
    Procedure :: Initialize_LOG => LOG_Initialize_typGURU

    !!... Toggle Debug Mode
    Procedure :: ToggleDebugMode => LOG_ToggleDebugMode_typGURU

    !!... Toggle Color Mode
    Procedure :: ToggleColorMode => LOG_ToggleColorMode_typGURU

    !!... Toggle isMaster
    Procedure :: ToggleIsMaster => LOG_ToggleIsMaster_typGURU

    !!... Retutn the mode is enabled or disabled.
    Procedure :: IsDebug  => LOG_IsDebug_typGURU
    Procedure :: IsColor  => LOG_IsColor_typGURU
    Procedure :: IsMaster => LOG_IsMaster_typGURU

    !!... Write Message
    Procedure :: Write => LOG_Write_typGURU

    !!... Write Message
    Procedure :: WriteDebug => LOG_WriteDebug_typGURU

    !!... Info Message
    Procedure :: Info             => LOG_Info_typGURU
    Procedure :: InfoSingleVector => LOG_InfoSingleVector_typGURU
    Procedure :: InfoVectorVector => LOG_InfoVectorVector_typGURU

    !!... Write Header Message
    Procedure :: Error             => LOG_Error_typGURU
    Procedure :: ErrorSingleVector => LOG_ErrorSingleVector_typGURU
    Procedure :: ErrorVectorVector => LOG_ErrorVectorVector_typGURU

    !!... Write Header Message
    Procedure :: Warn             => LOG_Warn_typGURU
    Procedure :: WarnSingleVector => LOG_WarnSingleVector_typGURU
    Procedure :: WarnVectorVector => LOG_WarnVectorVector_typGURU

    !!... Write Header Message
    Procedure :: Debug             => LOG_Debug_typGURU
    Procedure :: DebugSingleVector => LOG_DebugSingleVector_typGURU
    Procedure :: DebugVectorVector => LOG_DebugVectorVector_typGURU

    !!... Flush File IO
    Procedure :: Flush => LOG_Flush_typGURU

    !!... Flush File IO
    Procedure :: Stop => LOG_Stop_typGURU

    !!... Destroy
    Procedure :: Destroy_LOG => LOG_Destroy_typGURU

!! -------------------------------------------------------------------------- !!
End Type
!! -------------------------------------------------------------------------- !!
