!! -------------------------------------------------------------------------- !!
Subroutine JSON_InitializeCore( &
    &   verbose,                        &   !! mainly useful for debugging (default is false)
    &   compact_reals,                  &   !! to compact the real number strings for output (default is true)
    &   print_signs,                    &   !! always print numeric sign (default is false)
    &   real_format,                    &   !! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
    &   spaces_per_tab,                 &   !! number of spaces per tab for indenting (default is 2)
    &   strict_type_checking,           &   !! [SEE BELOW]
    &   trailing_spaces_significant,    &
    &   case_sensitive_keys,            &
    &   no_whitespace,                  &
    &   unescape_strings,               &
    &   comment_char,                   &   !! If present, these characters are used to denote comments in the JSON file
    &   path_mode,                      &
    &   path_separator,                 &
    &   compress_vectors,               &   !! If true, then arrays are printed all on one line.
    &   allow_duplicate_keys,           &
    &   escape_solidus,                 &
    &   stop_on_error,                  &
    &   null_to_real_mode,              &
    &   non_normal_mode,                &
    &   use_quiet_nan,                  &
    &   strict_integer_type_checking    )
!! -------------------------------------------------------------------------- !!
    Implicit None
    logical,intent(in),optional :: verbose
    logical,intent(in),optional :: compact_reals
    logical,intent(in),optional :: print_signs
    character(len=*),intent(in),optional :: real_format
    integer,intent(in),optional :: spaces_per_tab
    logical,intent(in),optional :: strict_type_checking
      !! if true, no integer, double, or logical type
      !! conversions are done for the `get` routines
      !! (default is false).
    logical,intent(in),optional :: trailing_spaces_significant
      !! for name and path comparisons, is trailing
      !! space to be considered significant.
      !! (default is false)
    logical,intent(in),optional :: case_sensitive_keys
      !! for name and path comparisons, are they
      !! case sensitive. (default is true)
    logical,intent(in),optional :: no_whitespace
      !! if true, printing the JSON structure is
      !! done without adding any non-significant
      !! spaces or linebreaks (default is false)
    logical,intent(in),optional :: unescape_strings
      !! If false, then the raw escaped
      !! string is returned from [[json_get_string]]
      !! and similar routines. If true [default],
      !! then the string is returned unescaped.
    character(len=*),intent(in),optional :: comment_char
      !! If present, these characters are used
      !! to denote comments in the JSON file,
      !! which will be ignored if present.
      !! Example: `!`, `#`, or `/!#`. Setting this
      !! to a blank string disables the
      !! ignoring of comments. (Default is `/!#`).
    integer,intent(in),optional :: path_mode
      !! How the path strings are interpreted in the
      !! `get_by_path` routines:
      !!
      !! * 1 : Default mode (see [[json_get_by_path_default]])
      !! * 2 : as RFC 6901 "JSON Pointer" paths
      !!   (see [[json_get_by_path_rfc6901]])
      !! * 3 : JSONPath "bracket-notation"
      !!   see [[json_get_by_path_jsonpath_bracket]])
    character(len=1),intent(in),optional :: path_separator
      !! The `path` separator to use
      !! in the "default" mode for
      !! the paths in the various
      !! `get_by_path` routines.
      !! Example: `.` [default] or `%`.
      !! Note: if `path_mode/=1`
      !! then this is ignored.
    logical,intent(in),optional :: compress_vectors
      !! If true, then arrays of integers,
      !! nulls, doubles, and logicals are
      !! printed all on one line.
      !! [Note: `no_whitespace` will
      !! override this option if necessary].
      !! (Default is False).
    logical,intent(in),optional :: allow_duplicate_keys
      !! * If True [default] then no special checks
      !!   are done to check for duplicate keys.
      !! * If False, then after parsing, if any duplicate
      !!   keys are found, an error is thrown. A call to
      !!   [[json_value_validate]] will also check for
      !!   duplicates.
    logical,intent(in),optional :: escape_solidus
      !! * If True then the solidus "`/`" is always escaped
      !!   "`\/`" when serializing JSON
      !! * If False [default], then it is not escaped.
      !!
      !! Note that this option does not affect parsing
      !! (both escaped and unescaped are still valid in
      !! all cases).
    logical,intent(in),optional :: stop_on_error
      !! If an exception is raised, then immediately quit.
      !! (Default is False).
    integer,intent(in),optional :: null_to_real_mode
      !! if `strict_type_checking=false`:
      !!
      !! * 1 : an exception will be raised if
      !!   try to retrieve a `null` as a real.
      !! * 2 : a `null` retrieved as a real
      !!   will return a NaN. [default]
      !! * 3 : a `null` retrieved as a real
      !!   will return 0.0.
    integer,intent(in),optional :: non_normal_mode
      !! How to serialize NaN, Infinity, and
      !! -Infinity real values:
      !!
      !! * 1 : as strings (e.g., "NaN",
      !!   "Infinity", "-Infinity") [default]
      !! * 2 : as JSON `null` values
    logical,intent(in),optional :: use_quiet_nan
      !! * If true [default], `null_to_real_mode=2`
      !!   and [[string_to_real]] will use
      !!   `ieee_quiet_nan` for NaN values.
      !! * If false,
      !!   `ieee_signaling_nan` will be used.
    logical,intent(in),optional :: strict_integer_type_checking
      !! * If false, when parsing JSON, if an integer numeric value
      !!   cannot be converted to an integer (`integer(IK)`),
      !!   then an attempt is then make to convert it
      !!   to a real (`real(RK)`).
      !! * If true, an exception will be raised if the integer
      !!   value cannot be read.
      !!
      !! (default is true)
!! -------------------------------------------------------------------------- !!
    Call jsonCore%Initialize( &
    &   verbose                      = verbose,                        &
    &   compact_reals                = compact_reals,                  &
    &   print_signs                  = print_signs,                    &
    &   real_format                  = real_format,                    &
    &   spaces_per_tab               = spaces_per_tab,                 &
    &   strict_type_checking         = strict_type_checking,           &
    &   trailing_spaces_significant  = trailing_spaces_significant,    &
    &   case_sensitive_keys          = case_sensitive_keys,            &
    &   no_whitespace                = no_whitespace,                  &
    &   unescape_strings             = unescape_strings,               &
    &   comment_char                 = comment_char,                   &
    &   path_mode                    = path_mode,                      &
    &   path_separator               = path_separator,                 &
    &   compress_vectors             = compress_vectors,               &
    &   allow_duplicate_keys         = allow_duplicate_keys,           &
    &   escape_solidus               = escape_solidus,                 &
    &   stop_on_error                = stop_on_error,                  &
    &   null_to_real_mode            = null_to_real_mode,              &
    &   non_normal_mode              = non_normal_mode,                &
    &   use_quiet_nan                = use_quiet_nan,                  &
    &   strict_integer_type_checking = strict_integer_type_checking    )
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_ReadFile( filePath, jsonFile, json, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)     :: filePath
    Type(typJSONFile), intent(inout) :: jsonFile
    Type(typJSON), pointer           :: json
    Logical, optional                :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isExist, isError_
    Character(len=:), Allocatable :: filePath_, info

    info = "error"
    if ( present(isError) ) info = "debug"

    !!... Check
    isError_ = .FALSE.

    If ( len_trim(adjustl(filePath)).le.0 ) then
        isError_ = .TRUE.
        Call GURU%Info( &
        &   info   = info,                          &
        &   msg    = "Given 'filePath' is empty.",  &
        &   head   = "JSON_ReadFile",               &
        &   isStop = .FALSE.                        )
    End if

    filePath_ = trim(adjustl(filePath))

    Inquire( file = filePath_, exist = isExist )
    if (.NOT.isExist) then
        isError_ = .TRUE.
        Call GURU%Info( &
        &   info   = info,                      &
        &   msg   = "JSON file do not exist.",  &
        &   head  = "JSON_ReadFile",            &
        &   value = filePath_,                  &
        &   isStop = .FALSE.                    )
    end if

    !!... Initialize JSON File
    Call jsonFile%Initialize()
    If ( jsonFile%Failed() ) then
        isError_ = .TRUE.
        Call GURU%Info( &
        &   info   = info,                              &
        &   msg   = "Failed to initialize JSON file.",  &
        &   head  = "JSON_ReadFile",                    &
        &   isStop = .FALSE.                            )
    End if

    !!... Load JSON File
    Call jsonFile%Load( filePath_ )
    If ( jsonFile%Failed() ) then
        isError_ = .TRUE.
        Call GURU%Info( &
        &   info   = info,                          &
        &   msg   = "Failed to load JSON file.",    &
        &   head  = "JSON_ReadFile",                &
        &   value = filePath_ ,                     &
        &   isStop = .FALSE.                        )
    End if

    !!... Get JSON root
    Call jsonFile%Get( json )
    If ( jsonFile%Failed() ) then
        isError_ = .TRUE.
        Call GURU%Info( &
        &   info   = info,                      &
        &   msg   = "Failed to get JSON root.", &
        &   head  = "JSON_ReadFile",            &
        &   value = filePath_,                  &
        &   isStop = .FALSE.                    )
    End if

    If ( present(isError) ) isError = isError_

    if (Allocated(filePath_)) deallocate( filePath_ )
    if (Allocated(info)) deallocate( info )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_ReadFileIfExist( filePath, jsonFile, json, isError )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Character(len=*), intent(in)     :: filePath
    Type(typJSONFile), intent(inout) :: jsonFile
    Type(typJSON), pointer           :: json
    Logical, optional                :: isError
!! -------------------------------------------------------------------------- !!
    Logical :: isError_

    !!... Get File
    Call JSON_ReadFile( filePath, jsonFile, json, isError_ )

    if ( isError_ ) then
        Call GURU%Warn( &
        &   msg   = "Failed to load JSON file. json null pointer is returned.", &
        &   head  = "JSON_ReadFileIfExist", &
        &   value = trim(adjustl(filePath)) )
    end if

    if (present(isError)) isError = isError_

!! -------------------------------------------------------------------------- !!
    End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_CloseFile( jsonFile )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSONFile), intent(inout)  :: jsonFile
!! -------------------------------------------------------------------------- !!
    call jsonFile%Destroy()
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_Print( json, unit )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSON), pointer        :: json
    Integer, intent(in), Optional :: unit
!! -------------------------------------------------------------------------- !!
    Logical :: isFileOpen
    Integer :: unit_

    unit_ = 6
    If (present(unit)) unit_ = unit
    If ( unit_.eq.5 ) then
        Call GURU%Error( &
        &   msg  = "Default in-unit(5) is used for printing JSON.", &
        &   head = "JSON_Print" )
    elseif ( unit_.eq.6 ) then
        !!... Print command window
        Call jsonCore%Print(json)
    else
        Inquire( unit = unit_, opened = isFileOpen )
        if ( isFileOpen ) then
            !!... Print to unit
            Call jsonCore%Print(json, unit)
        else
            Call GURU%Error( &
            &   msg   = "The file corresponds to given 'unit' is not opened.", &
            &   head  = "JSON_Print",        &
            &   value = unit_                )
        end if
    end if
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_PrintFilePath( json, filePath )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSON), pointer        :: json
    Character(len=*), intent(in)  :: filePath
!! -------------------------------------------------------------------------- !!
    Character(len=:), Allocatable :: filePath_
    Logical :: isOpen

    filePath_ = trim(adjustl(filePath))

    if (len(filePath).le.0) then
        Call GURU%Error( &
        &   msg    = "Given 'filePath' is empty.",  &
        &   head   = "JSON_PrintFilePath"           )
    End if

    Inquire( file = filePath_, opened = isOpen )

    if (isOpen) then
        Call GURU%Error( &
        &   msg   = "Given 'filePath' is used by other processor.",  &
        &   head  = "JSON_PrintFilePath",   &
        &   value = filePath_               )
    end if

    !!... Create Base Dir
    Call CreateBaseDir( filePath_ )

    !!... Print File
    Call jsonCore%Print( json, filePath_ )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
    Subroutine JSON_CreateObject(json, name)
!! -------------------------------------------------------------------------- !!
    Implicit None
    type(typJSON), pointer       :: json
    Character(len=*), intent(in) :: name
!! -------------------------------------------------------------------------- !!
    if (len_trim(name).le.0) then
        Call GURU%Error( &
        &   msg    = "Given 'name' is empty.",  &
        &   head   = "JSON_CreateObject"           )
    End if
    Call jsonCore%Create_object(json, name)
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_Clone(from, to)
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!
    Type(typJSON), pointer :: from
    Type(typJSON), pointer :: to
!! -------------------------------------------------------------------------- !!
    Call jsonCore%Clone( from, to )
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine JSON_Destroy(json)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Type(typJSON), pointer :: json
!! -------------------------------------------------------------------------- !!
    Call jsonCore%Destroy(json)
!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
