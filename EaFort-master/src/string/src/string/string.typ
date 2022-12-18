Type, Extends(string), Public :: typString

Contains

    !!... Get integer value from raw character
    Procedure :: GetInt => GetInt_typString

    !!... Get real value from raw character
    Procedure :: GetReal => GetReal_typString

    !!... Get logical value from raw character
    Procedure :: GetLogical => GetLogical_typString

    !!... Get char value from raw character
    Procedure :: GetChar => GetChar_typString

    !!... Get int value from raw character or use default value
    Procedure :: GetIntOrDefault => GetIntOrDefault_typString

    !!... Get real value from raw character or use default value
    Procedure :: GetRealOrDefault => GetRealOrDefault_typString

    !!... Get logical value from raw character or use default value
    Procedure :: GetLogicalOrDefault => GetLogicalOrDefault_typString

    !!... Get characeter value from raw character or use default value
    Procedure :: GetCharOrDefault => GetCharOrDefault_typString

    !!... Get integer vector from raw character
    Procedure :: GetIntVector => GetIntVector_typString

    !!... Get integer vector from raw character
    Procedure :: GetRealVector => GetRealVector_typString

    !!... Get integer vector from raw character
    Procedure :: GetLogicalVector => GetLogicalVector_typString

    !!... Get string vector
    Procedure :: GetStringVector => GetStringVector_typString

    !!... Get integer vector or default value
    Procedure :: GetIntVectorOrDefault => GetIntVectorOrDefault_typString

    !!... Get Real vector or default value
    Procedure :: GetRealVectorOrDefault => GetRealVectorOrDefault_typString

    !!... Get Logical vector or default value
    Procedure :: GetLogicalVectorOrDefault => GetLogicalVectorOrDefault_typString

    !!... Get String vector or default value
    Procedure :: GetStringVectorOrDefault => GetStringVectorOrDefault_typString

    !!... Split itself into typString vector
    Procedure, Private :: Split_typString

    !!... Split itself with special single characters
    !!    1 : Black ' '
    !!    2 : Comma ','
    Procedure :: SplitSimple => SplitSimple_typString

    !1... Generic Procedure

    !!... Split
    Generic :: Split => Split_typString

    !!... Destroy
    Procedure :: Destroy => Destroy_typString

End Type
