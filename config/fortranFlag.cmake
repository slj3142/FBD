## Set Fortran Flags for Compilation Modes
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")

    if (WIN32)
        set(Fortran_FLAG_GLOBAL_RELEASE "/fpp /O2 /Qmkl:sequential /openmp /heap-arrays" )
        set(Fortran_FLAG_GLOBAL_DEBUG   "/fpp /Od /Qmkl:sequential /openmp /heap-arrays" )
    else()
        set(Fortran_FLAG_GLOBAL_RELEASE "-fpp -ipo -fPIC -O2 -ip -qmkl:sequential -qopenmp -heap-arrays" )
        set(Fortran_FLAG_GLOBAL_DEBUG   "-fpp -ipo -fPIC -O0 -traceback -qmkl:sequential -qopenmp -heap-arrays" )
    endif()
    add_definitions(-D__INTEL_FORTRAN__)

elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")

    set(Fortran_FLAG_GLOBAL_RELEASE "-cpp -fPIC -O2 -fopenmp")
    set(Fortran_FLAG_GLOBAL_DEBUG   "-cpp -fPIC -O0 -Wall -Wextra -fcheck=all -fbacktrace -pedantic -fopenmp" )
    add_definitions(-D__GNU_FORTRAN__)

endif()

##... Set Default Compilation Mode "Debug" or "Release"
set( FORTRAN_COMPILE_MODE_DEFAULT "Debug" )

if ("${CMAKE_BUILD_TYPE}" STREQUAL "")
    set(FORTRAN_COMPILE_MODE ${FORTRAN_COMPILE_MODE_DEFAULT})
else()
    if( ${CMAKE_BUILD_TYPE} STREQUAL "Release" OR ${CMAKE_BUILD_TYPE} STREQUAL "RELEASE")
        set(FORTRAN_COMPILE_MODE "Release")
    elseif( ${CMAKE_BUILD_TYPE} STREQUAL "Debug" OR ${CMAKE_BUILD_TYPE} STREQUAL "DEBUG")
        set(FORTRAN_COMPILE_MODE "Debug")
    else()
        set(FORTRAN_COMPILE_MODE "${FORTRAN_COMPILE_MODE_DEFAULT}")
    endif()
endif()

##... Set Flags Compilation Modes
if( ${FORTRAN_COMPILE_MODE} STREQUAL "Release" )
    set_property(GLOBAL PROPERTY Fortran_FLAG_GLOBAL ${Fortran_FLAG_GLOBAL_RELEASE})
elseif( ${FORTRAN_COMPILE_MODE} STREQUAL "Debug" )
    set_property(GLOBAL PROPERTY Fortran_FLAG_GLOBAL ${Fortran_FLAG_GLOBAL_DEBUG})
else()
    set_property(GLOBAL PROPERTY Fortran_FLAG_GLOBAL ${Fortran_FLAG_GLOBAL_RELEASE})
endif()

##... Set Global Variables
get_property(Fortran_FLAG GLOBAL PROPERTY Fortran_FLAG_GLOBAL)
set(CMAKE_Fortran_FLAGS "${Fortran_FLAG}")
