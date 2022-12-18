function(FindLibraryAndGetFlag LIB_FLAGS_OUT LIB_CORRECT)

    set(funcPrefix FindLibraryAndGetFlag)

    ## Input Argument Type List
    set(options)
    set(oneValueArgs IS_USER_DEFINED_PATH )
    set(multiValueArgs LIB_NAMES LIB_PATH_HINT LIB_PATHS )

    ## Get Input Arguments
    cmake_parse_arguments( ${funcPrefix}
        "${options}" "${oneValueArgs}" "${multiValueArgs}" "${ARGN}" )

    ## Set Variables
    set( IS_USER_DEFINED_PATH "${${funcPrefix}_IS_USER_DEFINED_PATH}" )
    set( LIB_NAMES            "${${funcPrefix}_LIB_NAMES}" )
    set( LIB_PATH_HINT        "${${funcPrefix}_LIB_PATH_HINT}" )
    set( LIB_PATHS            "${${funcPrefix}_LIB_PATHS}" )

    set( LIB_FLAGS "" )
    set( ${LIB_CORRECT} TRUE PARENT_SCOPE )
    foreach( LIB_NAME ${LIB_NAMES} )

        message("${Yellow}  Searching library: ${ColourReset}" "${LIB_NAME}")
        find_library( "${LIB_FLAG}" NAMES "${LIB_NAME}" HINTS ${LIB_PATH_HINT} )

        if ( IS_USER_DEFINED_PATH )
            find_library( LIB_FLAG_${LIB_NAME}
                NAMES ${LIB_NAME}
                HINTS ${LIB_PATH_HINT} NO_DEFAULT_PATH )
        else()
            find_library( LIB_FLAG_${LIB_NAME}
                NAMES ${LIB_NAME}
                HINTS ${LIB_PATH_HINT} ENV LD_LIBRARY_PATH
                )
        endif()

        ## Add flag if library is found
        if (${LIB_FLAG_${LIB_NAME}} STREQUAL LIB_FLAG_${LIB_NAME}-NOTFOUND)
            message("${Red}    NOT FOUND")
            set( ${LIB_CORRECT} FALSE PARENT_SCOPE )
        else()
            message("${Green}    [Found] FLAG:${ColourReset} " ${LIB_FLAG_${LIB_NAME}})
            list(APPEND LIB_FLAGS ${LIB_FLAG_${LIB_NAME}})
        endif()

    endforeach()
    message("${ColourReset}")

    set( ${LIB_FLAGS_OUT} ${LIB_FLAGS} PARENT_SCOPE )
    return()

endfunction()
