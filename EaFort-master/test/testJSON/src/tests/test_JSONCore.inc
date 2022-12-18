!! -------------------------------------------------------------------------- !!
Subroutine Test_JSONCore()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!
    !!...
    Type(typJSONFile)      :: jsonFile
    Type(typJSON), Pointer :: json, json2, json3
    Integer                :: iUnit

    !!... Test JSON File
    Call JSON_ReadFile( &
    &   filePath = "test/testJSON/src/JSON/test.json", &
    &   jsonFile = jsonFile, &
    &   json     = json )

    !!... Print Test JSON file
    Call JSON_Print( json )
    !!... Print Test JSON File
    Call JSON_Print( json, "out.json" )
    Call JSON_Clone(json, json2)

    !!... Set New Tab Size
    Call JSON_InitializeCore( spaces_per_tab = 4 )

    !!... Print Test JSON File
    Open( newunit = iUnit, file = "out2.json", status = "replace" )
    Call JSON_Print( json2, iUnit )

    !!...
    Call JSON_CreateObject(json3, name = "json3")

    !!... Print Test JSON File
    Call JSON_Print( json3, "out3.json" )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
