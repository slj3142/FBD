!! -------------------------------------------------------------------------- !!
Subroutine Test_JSONUpdate()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!...
    Type(typJSONFile)      :: jsonFile
    Type(typJSON), Pointer :: json, jsonFruit, jsonAnimal, jsonParent
    Type(typJSON), Pointer :: jsonApple, jsonBanana, jsonLion, jsonTiger
    Integer                :: iUnit
    Type(typJSON), Pointer :: jsonChild
    Type(typString), dimension(3) :: friendsNames

    !!... Test JSON File
    Call JSON_ReadFile( &
    &   filePath = "test/testJSON/src/JSON/test.json", &
    &   jsonFile = jsonFile, &
    &   json     = json )

    Call JSON_InitializeCore( compress_vectors = .TRUE.)

    !!... Print Test JSON file
    Call JSON_Print( json )

    !!...
    Call JSON_GetChild(json, "fruit", jsonFruit)
    Call JSON_GetChild(jsonFruit, "apple", jsonApple)
    Call JSON_GetChild(jsonFruit, "banana", jsonBanana)

    Call JSON_UpdateVector( jsonBanana, "weight", [1.D0, 2.D0] )

    Call JSON_Update( jsonBanana, "newItem", .FALSE. )
    Call JSON_Update( jsonBanana, "adsad", "dsashdks" )

    Call JSON_CreateObject( jsonChild, "apple" )
    Call JSON_Update( jsonChild, "brand", "Michigan Corp.")
    Call JSON_Update( jsonFruit, jsonChild )

    !!... Print Test JSON file
    Call JSON_Print( json )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
