!! -------------------------------------------------------------------------- !!
!                                   EaFort:JSON                               !!
!! -------------------------------------------------------------------------- !!
!   Wrapper Module of JSON-Fortran for easy use.
!
!   author: Young-Myung Choi
!   date: 2021-10-20
!
!! -------------------------------------------------------------------------- !!
Module modJSON
!! -------------------------------------------------------------------------- !!

    Use modEaFort       !!... Global variables of EaFort
    Use modGURU         !!... GURU Module
    Use modString       !!... String Module
    Use modFileIO       !!... File IO Module

    Use json_module, only: &
    &   typJSONCore => json_core, &
    &   typJSON     => json_value, &
    &   typJSONFile => json_file,  &
    &   json_unknown, json_null, json_object, json_array, json_logical, &
    &   json_integer, json_real, json_string, json_CK, json_real

!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... var_type
    Integer, parameter, Public :: ENUM_JSON_UNKNOWN = json_unknown
    Integer, parameter, Public :: ENUM_JSON_NULL    = json_null
    Integer, parameter, Public :: ENUM_JSON_OBJECT  = json_object
    Integer, parameter, Public :: ENUM_JSON_Vector  = json_array
    Integer, parameter, Public :: ENUM_JSON_LOGICAL = json_logical
    Integer, parameter, Public :: ENUM_JSON_INTEGER = json_integer
    Integer, parameter, Public :: ENUM_JSON_REAL    = json_real
    Integer, parameter, Public :: ENUM_JSON_STRING  = json_string
    Integer, parameter, Public :: ENUM_JSON_DBLE    = ENUM_JSON_REAL

    !!... JSON Core
    Type(typJSONCore) :: jsonCore

!!... Functions associated with JSON Core, File, Print, Clone, Close
#include "routines/jsonCore.proc"

!!... Functions associated with basic manipulations
#include "routines/jsonFunc.proc"

!!... Functions to get variable
#include "routines/jsonGetFunc.proc"

!!... Functions to add variable
#include "routines/jsonAddFunc.proc"

!!... Functions to update variable
#include "routines/jsonUpdateFunc.proc"

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

!!... Functions associated with JSON Core, File, Print, Clone, Close
#include "routines/jsonCore.inc"

!!... Functions associated with basic manipulations
#include "routines/jsonFunc.inc"

!!... Functions to get variable
#include "routines/jsonGetFunc.inc"

!!... Functions to add variable
#include "routines/jsonAddFunc.inc"

!!... Functions to update variable
#include "routines/jsonUpdateFunc.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
