## -------------------------------------------------------------------------- ##
#
#   Project: FortranProjectBase
#
#   Description:
#       Project example to begin the computer package and to educate student.
#
#   Author:
#       Young-Myung CHOI
#         Dept. of Naval Arch. and Ocean Eng.,
#         Pusan National University, Republic of Korea
#
## -------------------------------------------------------------------------- ##

## Default Settings --------------------------------------------------------- ##

##... Set the minimum version of cmake
cmake_minimum_required(VERSION 2.9 FATAL_ERROR)

##... CMake Project Name
project(FortranProjectBase)

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

## Print CMake Project Information ------------------------------------------ ##
message("")
message("${BoldYellow}Project: ${BoldWhite}${CMAKE_PROJECT_NAME}")
message("")
message("  - ${Green}Projct Path       ${White}: ${PROJECT_COMPILE_PATH} ${Green}")
message("  - ${Green}GIT_BRANCH        ${White}: ${GIT_BRANCH} ${Green}")
message("  - ${Green}GIT_COMMIT_HASH   ${White}: ${GIT_COMMIT_HASH} ${Green}")
message("")
message("  - ${Green}Lib. Install Path ${White}: ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${Green}")
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
## Compile Library ---------------------------------------------------------- ##

#set(COMPILE_DYNAMIC_LIB False)
# set(COMPILE_DYNAMIC_LIB True)

# #... Library 1
# set(PROJ_TYPE "LIB")
# set(PROJ_NAME lib1)
# set(${PROJ_TYPE}_NAME_${PROJ_NAME} ${PROJ_NAME})
# # # set(${PROJ_TYPE}_PATH_${PROJ_NAME} )
# # # set(${PROJ_TYPE}_DEPEND_${PROJ_NAME})
# set(${PROJ_TYPE}_EXTLIB_${PROJ_NAME} /winUser/Test/EaFort-master/lib/libEaFort.a)
# set(${PROJ_TYPE}_EXTINC_${PROJ_NAME} /winUser/Test/EaFort-master/lib)
# # # set(${PROJ_TYPE}_EXTFLAG_app1 )
# add_subdirectory(src/${PROJ_NAME})

# #... Library 2
# set(PROJ_TYPE "LIB")
# set(PROJ_NAME lib2)
# set(${PROJ_TYPE}_NAME_${PROJ_NAME} ${PROJ_NAME})
# # set(${PROJ_TYPE}_PATH_${PROJ_NAME} )
# set(${PROJ_TYPE}_DEPEND_${PROJ_NAME} lib1)
# # set(${PROJ_TYPE}_EXTLIB_${PROJ_NAME})
# # set(${PROJ_TYPE}_EXTINC_${PROJ_NAME})
# # set(${PROJ_TYPE}_EXTFLAG_app1 )
# add_subdirectory(src/${PROJ_NAME})

# ## Compile Test Programs ---------------------------------------------------- ##

# #... Test 1
# set(PROJ_TYPE "TEST")
# set(PROJ_NAME testLib1)
# set(${PROJ_TYPE}_NAME_${PROJ_NAME} ${PROJ_NAME})
# # set(${PROJ_TYPE}_PATH_${PROJ_NAME} )
# set(${PROJ_TYPE}_DEPEND_${PROJ_NAME} lib1)
# # set(${PROJ_TYPE}_EXTLIB_${PROJ_NAME})
# # set(${PROJ_TYPE}_EXTINC_${PROJ_NAME})
# # set(${PROJ_TYPE}_EXTFLAG_app1 )
# add_subdirectory(tests/${PROJ_NAME})

# #... Test 2
# set(PROJ_TYPE "TEST")
# set(PROJ_NAME testLib2)
# set(${PROJ_TYPE}_NAME_${PROJ_NAME} ${PROJ_NAME})
# # set(${PROJ_TYPE}_PATH_${PROJ_NAME} )
# set(${PROJ_TYPE}_DEPEND_${PROJ_NAME} lib1 lib2)
# # set(${PROJ_TYPE}_EXTLIB_${PROJ_NAME})
# # set(${PROJ_TYPE}_EXTINC_${PROJ_NAME})
# # set(${PROJ_TYPE}_EXTFLAG_app1 )
# add_subdirectory(tests/${PROJ_NAME})

## Compile Applications ----------------------------------------------------- ##

#... Application 1
set(PROJ_TYPE "APP")
set(PROJ_NAME app1)
set(${PROJ_TYPE}_NAME_${PROJ_NAME} ${PROJ_NAME})
# set(${PROJ_TYPE}_PATH_${PROJ_NAME} )
# set(${PROJ_TYPE}_DEPEND_${PROJ_NAME} lib1 )
set(${PROJ_TYPE}_EXTLIB_${PROJ_NAME} /winUser/FinalAssignment/EaFort-master/lib/libEaFort.a)
set(${PROJ_TYPE}_EXTINC_${PROJ_NAME} /winUser/FinalAssignment/EaFort-master/lib)
# set(${PROJ_TYPE}_EXTFLAG_app1 )
add_subdirectory(applications/${PROJ_NAME})
