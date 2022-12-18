!! -------------------------------------------------------------------------- !!
Subroutine Test_JSONAdd()
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

    !!...
    Call JSON_AddVector(jsonBanana, "newItem", [1, 3, 5])
    Call JSON_Add(jsonBanana, "newItem1", 5.D0)

    Call JSON_CreateObject( jsonChild, "child" )
    Call JSON_Add( jsonChild, "age", 11 )
    Call JSON_Add( jsonChild, "height", 120.7 )
    Call JSON_Add( jsonChild, "male", .TRUE. )
    Call JSON_Add( jsonChild, "hobby", 'foot ball')
    friendsNames(1) = "Jack"
    friendsNames(2) = "Bob"
    friendsNames(3) = "Rorenz"

    Call JSON_AddVector( jsonChild, "friends", friendsNames)
    Call JSON_Add( json, jsonChild )

    !!... Print Test JSON file
    Call JSON_Print( json )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
