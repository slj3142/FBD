## -------------------------------------------------------------------------- ##
#                                                                              #
#                     EaFort: Easy Fortran Handling Library                    #
#                                                                              #
#    * Author : Young-Myung CHOI                                               #
#                                                                              #
## -------------------------------------------------------------------------- ##

## Default Settings --------------------------------------------------------- ##

##... Set the minimum version of cmake
cmake_minimum_required(VERSION 3.1 FATAL_ERROR)
if(POLICY CMP0054)
  cmake_policy(SET CMP0054 NEW)
endif()

##... CMake Project Name
project(EaFort)

##... Set Default Fortran Compiler & Eable the Fortran Compilation Environment
enable_language(Fortran)
if (NOT DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER gfortran)
endif()

## CMake Configuration ------------------------------------------------------ ##

##... Load Default CMake Configuration
include( config/config.cmake )

##... Set Fortran Compiler Flags
include( config/fortranFlag.cmake )

##... Auto Compilation Functions
include( config/Func_AutoCompile.cmake )

## Find Library and compile
include( config/FindLibraryAndGetFlag.cmake)

## Print CMake Project Information ------------------------------------------ ##
message("")
message("${BoldYellow}Project: ${BoldWhite}${CMAKE_PROJECT_NAME}")
message("")
message("  - ${Green}Projct Path       ${White}: ${PROJECT_COMPILE_PATH} ${Green}")
message("  - ${Green}GIT_BRANCH        ${White}: ${GIT_BRANCH} ${Green}")
message("  - ${Green}GIT_COMMIT_HASH   ${White}: ${GIT_COMMIT_HASH} ${Green}")
message("")
message("  - ${Green}Lib. Install Path ${White}: ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${Green}")
message("  - ${Green}Inc. Install Path ${White}: ${CMAKE_Fortran_MODULE_DIRECTORY} ${Green}")
message("  - ${Green}Exe. Install Path ${White}: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} ${Green}")
message("")

if( ${FORTRAN_COMPILE_MODE} STREQUAL "Release" )
message("  - ${Green}Compilation Mode  ${White}: Release ${Green}")
elseif( ${FORTRAN_COMPILE_MODE} STREQUAL "Debug" )
message("  - ${Green}Compilation Mode  ${White}: Debug ${Green}")
endif()

message("${Green}")
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
message("  - ${Green}Fortran Compiler  ${White}: Intel Fortran ${Green}")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
message("  - ${Green}Fortran Compiler  ${White}: GNU Fortran ${Green}")
endif()
message("  - ${Green}Global Flag       ${White}: ${CMAKE_Fortran_FLAGS} ${Green}")
message("${ColourReset}")

## Math Kernel Library ------------------------------------------------------ ##

set( IS_MATHFACK_COMPILE FALSE )
# set( IS_MATHFACK_COMPILE TRUE )

if ( ${IS_MATHFACK_COMPILE} )
    set( LAPACK_MANUAL_INSTALL FALSE )
    # set( LAPACK_MANUAL_INSTALL TRUE )
    if ( ${LAPACK_MANUAL_INSTALL} )
        set(LAPACK_VERSION 3.10.1)
        set(LAPACK_PATH_HINT ${PROJECT_COMPILE_PATH}/ThirdParty/lapack-${LAPACK_VERSION}/lib)
        set(LAPACK_DEFINED_PATH TRUE)
    else()
        set(LAPACK_DEFINED_PATH FALSE)
    endif()
    include( config/findMathKernel.cmake )
endif()

## Compile EaFort Library --------------------------------------------------- ##

##... Set Library name
set( LIB_NAME_EaFort EaFort )
set( EXTLIB_EaFort ${MKL_LIB} )

##... Set dependencies
add_subdirectory(src)

## Compile Testing EaFort Programs ------------------------------------------ ##

add_subdirectory(test/testString)
add_subdirectory(test/testGURU)
add_subdirectory(test/testOS)
add_subdirectory(test/testAuxFunc)
add_subdirectory(test/testFileIO)
add_subdirectory(test/testJSON)


if (${IS_MATHFACK_COMPILE})
    add_subdirectory(test/testMathPack)
endif()
