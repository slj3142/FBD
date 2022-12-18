!! -------------------------------------------------------------------------- !!
Subroutine Test_JSONFunc()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    Type(typJSONFile)      :: jsonFile
    Type(typJSON), Pointer :: json
    Character(len=:), allocatable :: name
    Integer :: nChild

    !!... Test JSON File
    Call JSON_ReadFile( &
    &   filePath = "test/testJSON/src/JSON/test.json", &
    &   jsonFile = jsonFile, &
    &   json     = json )

    !!... Print Test JSON file
    Call JSON_Print( json )

    !!... Get Info
    Call JSON_GetInfo( json = json, name = name, nChild = nChild )
    write(*,*) "  name : ", name
    write(*,*) "nChild : ", nChild

    !!... Get nChild
    Call JSON_GetNChild( json, nChild )
    write(*,*) "nChild : ", nChild

    !!... Get name
    Call JSON_GetName( json, name )
    write(*,*) "  name : ", name

    !!... Get name
    Call JSON_Rename( json, "newName" )
    Call JSON_GetName( json, name )
    write(*,*) "  name : ", name

    !!... Print Test JSON file
    Call JSON_Print( json )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
