!! -------------------------------------------------------------------------- !!
Subroutine Test_JSONGet()
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!...
    Type(typJSONFile)      :: jsonFile
    Type(typJSON), Pointer :: json, jsonFruit, jsonAnimal, jsonParent
    Type(typJSON), Pointer :: jsonApple, jsonBanana, jsonLion, jsonTiger
    Integer                :: iUnit

    !!... Test JSON File
    Call JSON_ReadFile( &
    &   filePath = "test/testJSON/src/JSON/test.json", &
    &   jsonFile = jsonFile, &
    &   json     = json )

    !!... Print Test JSON file
    ! Call JSON_Print( json )
    Call JSON_GetChild(json, "fruit", jsonFruit)
    Call JSON_GetChild(jsonFruit, "apple", jsonApple)
    Call JSON_GetChild(jsonFruit, "banana", jsonBanana)

    Call JSON_GetChild(json, "animal", jsonAnimal)
    Call JSON_GetChild(jsonAnimal, "lion", jsonLion)
    Call JSON_GetChild(jsonAnimal, "tiger", jsonTiger)
    Call JSON_GetParent( jsonTiger, jsonParent )
    ! Call JSON_Print( jsonParent )

    ! !!...
    ! Block
    !     Integer :: intVal
    !     Real(4) :: realVal
    !     Real(8) :: dbleVal
    !     Logical :: logVal
    !     Character(len=:), Allocatable :: chVal
    !     Type(typString) :: str, str2
    !
    !     Call JSON_GetInt( jsonApple, "weight", intVal )
    !     Call JSON_GetReal( jsonApple, "diameter", realVal )
    !     Call JSON_GetReal( jsonApple, "diameter", dbleVal )
    !     Call JSON_GetLogical( jsonApple, "isSell", logVal )
    !     Call JSON_GetChar( jsonApple, "market", chVal )
    !     Call JSON_GetString( jsonApple, "market", str )
    !
    !     write(*,*) "intVal  : ", intVal
    !     write(*,*) "realVal : ", realVal
    !     write(*,*) "dbleVal : ", dbleVal
    !     write(*,*) "logVal  : ", logVal
    !     write(*,*) "chVal   : ", chVal
    !     write(*,*) "str     : ", str%Chars()
    !
    !     str2 = "sadsd"
    !
    !     Call JSON_GetIntOrDefault( jsonApple, "weight1", intVal, 5 )
    !     Call JSON_GetRealOrDefault( jsonApple, "diameter1", realVal, 23.0 )
    !     Call JSON_GetDbleOrDefault( jsonApple, "diameter1", dbleVal, 255.D0 )
    !     Call JSON_GetLogicalOrDefault( jsonApple, "isSell1", logVal, .FALSE. )
    !     Call JSON_GetCharOrDefault( jsonApple, "market1", chVal, "sdad" )
    !     Call JSON_GetStringOrDefault( jsonApple, "market1", str, 'das' )
    !
    !     write(*,*) "intVal  : ", intVal
    !     write(*,*) "realVal : ", realVal
    !     write(*,*) "dbleVal : ", dbleVal
    !     write(*,*) "logVal  : ", logVal
    !     write(*,*) "chVal   : ", chVal
    !     write(*,*) "str     : ", str%Chars()
    !
    !     Call JSON_GetCharOrDefault( jsonApple, "market1", chVal, str2 )
    !     Call JSON_GetStringOrDefault( jsonApple, "market1", str, str2 )
    !
    !     write(*,*) "chVal   : ", chVal
    !     write(*,*) "str     : ", str%Chars()
    !
    ! End Block

    Block
        !!...
        Integer, allocatable :: intVec(:)
        Real(8), allocatable :: realVec(:)
        Real(8), allocatable :: dbleVec(:)
        Logical, allocatable :: logVec(:)
        Character(len=:), Allocatable, dimension(:) :: chVec(:)
        Type(typString), allocatable :: strVec(:), strVec2(:)
        !!...
        Integer :: nStr, iStr

        ! Call JSON_GetIntVector( jsonBanana, "length", intVec )
        ! Call JSON_GetRealVector( jsonBanana, "weight", realVec )
        ! Call JSON_GetReal( jsonBanana, "weight", dbleVec )
        ! Call JSON_GetLogicalVector( jsonBanana, "isSell", logVec )
        ! Call JSON_GetCharVector( jsonBanana, "market", chVec )
        ! Call JSON_GetStringVector( jsonBanana, "market", strVec )
        ! write(*,*) "intVal  : ", intVec
        ! write(*,*) "realVal : ", realVec
        ! write(*,*) "dbleVal : ", dbleVec
        ! write(*,*) "logVal  : ", logVec
        !
        ! nStr = size(strVec)
        ! do iStr = 1, nStr
        !     write(*,*) "ch(iStr)  : ", chVec(iStr)
        !     write(*,*) "str(iStr) : ", strVec(iStr)%Chars()
        ! end do

        Allocate(strVec2(2))
        strVec2(1) = "ABVC"
        strVec2(2) = "ABVCDD"

        Call JSON_GetIntVectorOrDefault( jsonBanana, "length1", intVec, [1 ,4, 3] )
        Call JSON_GetRealVectorOrDefault( jsonBanana, "weight1", realVec, [1.5D0, 2.1D0])
        Call JSON_GetRealVectorOrDefault( jsonBanana, "weight1", dbleVec, [2.D0, 2.3D0] )
        Call JSON_GetLogicalVectorOrDefault( jsonBanana, "isSell1", logVec, [.FALSE., .FALSE.] )
        ! Call JSON_GetCharVectorOrDefault( jsonBanana, "market1", chVec, ["A", "B", "C"] )
        ! Call JSON_GetStringVectorOrDefault( jsonBanana, "market1", strVec, ["A", "B", "C"] )

        ! Call JSON_GetCharVectorOrDefault( jsonBanana, "market1", chVec, strVec2 )
        ! Call JSON_GetStringVectorOrDefault( jsonBanana, "market1", strVec, strVec2 )

        Call JSON_GetCharVectorOrDefault( jsonBanana, "market1", chVec, [1, 2, 3] )
        Call JSON_GetStringVectorOrDefault( jsonBanana, "market1", strVec, [10.0D0, 2.7D0, 5.3D0] )

        write(*,*) "intVal  : ", intVec
        write(*,*) "realVal : ", realVec
        write(*,*) "dbleVal : ", dbleVec
        write(*,*) "logVal  : ", logVec

        nStr = size(chVec)
        do iStr = 1, nStr
            write(*,*) "ch(iStr)  : ", chVec(iStr)
            write(*,*) "str(iStr) : ", strVec(iStr)%Chars()
        end do

    End Block

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
