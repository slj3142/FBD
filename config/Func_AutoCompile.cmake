function( AutoCompile )

## Arguments ---------------------------------------------------------------- ##

##... Function Input Arguments
set(options
)

##... Arguments with one value
set(oneValueArgs
    IS_COMPILE_EXE          # Compile as "Executable", True Or False
    IS_COMPILE_LIB          # Compile as "Library", True Or False
    IS_DYNAMIC_LIB          # Compile as "Dynamic Library", True Or False
    INSTALL_NAME            # Installation Name
    INSTALL_PATH            # Installation Path
)

##... Arguments with multi variables
set(multiValueArgs
    INSTALL_SRC             # Source File List
    INSTALL_INC             # Include Path List
    INSTALL_LIB             # Library Path List
    INSTALL_DEPEND          # Dependent project list
    INSTALL_EXTLIB          # Extra library list
    INSTALL_EXTINC          # Extra include path list
    INSTALL_EXTFLAG         # Extra Flags
)

##... Get Function Arguments
    cmake_parse_arguments( AutoCompile
        "${options}" "${oneValueArgs}" "${multiValueArgs}" "${ARGN}" )

##... Set arguments as variables in function script
set( INSTALL_SRC         "${AutoCompile_INSTALL_SRC}" )
set( INSTALL_INC         "${AutoCompile_INSTALL_INC}" )
set( INSTALL_LIB         "${AutoCompile_INSTALL_LIB}" )
set( INSTALL_DEPEND      "${AutoCompile_INSTALL_DEPEND}" )
set( INSTALL_EXTLIB      "${AutoCompile_INSTALL_EXTLIB}" )
set( INSTALL_EXTINC      "${AutoCompile_INSTALL_EXTINC}" )
set( INSTALL_EXTFLAG     "${AutoCompile_INSTALL_EXTFLAG}" )

set( IS_COMPILE_EXE      "${AutoCompile_IS_COMPILE_EXE}" )
set( IS_COMPILE_LIB      "${AutoCompile_IS_COMPILE_LIB}" )
set( IS_DYNAMIC_LIB      "${AutoCompile_IS_DYNAMIC_LIB}" )
set( INSTALL_NAME        "${AutoCompile_INSTALL_NAME}" )
set( INSTALL_PATH        "${AutoCompile_INSTALL_PATH}" )

## Check Inputs ------------------------------------------------------------- ##

# message("${INSTALL_NAME}")
# message("${AutoCompile_INSTALL_NAME}")

##... Check black words
if ("${INSTALL_NAME}" STREQUAL "")
    message(" ${BoldRed} No INSTALL_NAME is defined. \n")
    message(" ${ColourReset} ")
    message( FATAL_ERROR )
endif()

if ("${INSTALL_PATH}" STREQUAL "")
    if ( IS_COMPILE_EXE )
        set(INSTALL_PATH ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    elseif( IS_COMPILE_LIB )
        set(INSTALL_PATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
    endif()
endif()

if ("${INSTALL_SRC}" STREQUAL "")
    message(" ${BoldRed} No INSTALL_SRC is defined. \n")
    message(" ${ColourReset} ")
    message( FATAL_ERROR )
endif()

get_property( Fortran_FLAG GLOBAL PROPERTY Fortran_FLAG_GLOBAL )
set( Fortran_FLAG "${Fortran_FLAG} ${INSTALL_EXT_FLAG}" )

## Set Properties ----------------------------------------------------------- ##

##... Add Library or Executable and print the name
if ( IS_COMPILE_EXE )

    message("${Yellow}Install Executable: ${White}${INSTALL_NAME}${Green}\n")
    add_executable( ${INSTALL_NAME} ${INSTALL_SRC} )

    set_target_properties(${INSTALL_NAME}
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${INSTALL_PATH})

elseif( IS_COMPILE_LIB )

    if ( IS_DYNAMIC_LIB )
        message("${Yellow}Install Dynamic Library: ${White}${INSTALL_NAME}${Green}\n")
        add_library(${INSTALL_NAME} SHARED ${INSTALL_SRC})
    else()
        message("${Yellow}Install Static Library: ${White}${INSTALL_NAME}${Green}\n")
        add_library(${INSTALL_NAME} STATIC ${INSTALL_SRC})
    endif()

    set_target_properties(${INSTALL_NAME}
        PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${INSTALL_PATH})

    set_target_properties(${INSTALL_NAME}
        PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${INSTALL_PATH})

else()

    message(" ${BoldRed} Specify compilation: executable or library. \n")
    message(" ${ColourReset} ")
    message( FATAL_ERROR )

endif()

##... Print out Install Path and Compile Flags
    message("${Green}  - INSTALL_PATH      : ${White}${INSTALL_PATH}${Green}")
    message("${Green}  - COMPILE_FLAG      : ${White}${Fortran_FLAG}${Green}")

## Set Fortran Flag
set_target_properties(${INSTALL_NAME} PROPERTIES COMPILE_FLAGS "${Fortran_FLAG}" )

##... Print Out Information
if (NOT "${INSTALL_INC}" STREQUAL "" )
    message("${Green}  - INSTALL_INC       : ${White}${INSTALL_INC}${Green}")
    target_include_directories(${INSTALL_NAME} PRIVATE "${INSTALL_INC}" )
endif()

if (NOT "${INSTALL_LIB}" STREQUAL "" )
    message("${Green}  - INSTALL_LIB       : ${White}${INSTALL_LIB}${Green}")
    target_link_directories(${INSTALL_NAME} PRIVATE "${INSTALL_LIB}" )
endif()

if (NOT "${INSTALL_DEPEND}" STREQUAL "" )
    message("${Green}  - INSTALL_DEPEND    : ${White}${INSTALL_DEPEND}")
    add_dependencies(${INSTALL_NAME} ${INSTALL_DEPEND} )
    target_link_libraries(${INSTALL_NAME} ${INSTALL_DEPEND} )
endif()

if (NOT "${INSTALL_EXTLIB}" STREQUAL "" )
    message("${Green}  - INSTALL_EXT_LIB   : ${White}${INSTALL_EXTLIB}")
    target_link_libraries(${INSTALL_NAME} ${INSTALL_EXTLIB} )
endif()

if (NOT "${INSTALL_EXTINC}" STREQUAL "" )
    message("${Green}  - INSTALL_EXT_INC   : ${White}${INSTALL_EXTINC}")
    target_include_directories(${INSTALL_NAME} PRIVATE ${INSTALL_EXTINC} )
endif()

if (NOT "${INSTALL_EXTFLAG}" STREQUAL "" )
    message("${Green}  - INSTALL_EXT_FLAG  : ${White}${INSTALL_EXTFLAG}")
    set_target_properties(${INSTALL_NAME} PROPERTIES COMPILE_FLAGS "${INSTALL_EXTFLAG}" )
endif()

message("${ColourReset} ")

## Compile ------------------------------------------------------------------ ##

install(TARGETS ${INSTALL_NAME} DESTINATION "${INSTALL_PATH}")

endfunction()
