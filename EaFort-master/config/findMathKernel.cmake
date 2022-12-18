message("${Yellow}Searching Math Kernel Library ${ColourReset}")
message("${ColourReset}")

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")

    ## NOTE: Math Kernel Library with intel fortran ----------------------------
    ##                                                                        ##
    ## If the intel fortran is used, the math kernel library is already added ##
    ## in the compilation flag.                                               ##
    ##                                                                        ##
    ## "/Qmkl:sequential" for windows                                         ##
    ## "-qmkl:sequential" for linux                                           ##
    ##                                                                        ##
    ## -------------------------------------------------------------------------

    ## Find MKL Lib
    # set(MKL_LIB_CORRECT FALSE)
    # set(MKL_LIB_NAME "mkl_intel_lp64" "mkl_sequential" "mkl_core" "mkl_blas95_lp64" )
    # set(MKL_LIB_PATH_HINT
    #     "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/mkl/lib/intel64"
	# 	"C:/Program Files (x86)/Intel/oneAPI/mkl/latest/lib/intel64"
    #     "/opt/intel/compilers_and_libraries/linux/mkl/lib/intel64"
    #     "/opt/intel/oneapi/mkl/latest/lib/intel64" )
    #
    # message("${Yellow}Searching MKL Libraries... ${ColourReset}")
    # message("${ColourReset}")
    #
    # ## Find Math Kernel Library first
    # FindLibraryAndGetFlag(
    #     MKL_LIB_FLAGS
    #     MKL_LIB_CORRECT
    #     LIB_NAMES       "${MKL_LIB_NAME}"
    #     LIB_PATH_HINT   "${MKL_LIB_PATH_HINT}" )

elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")

    if ( LAPACK_MANUAL_INSTALL )

        message("${ColourReset}")
        message("${Yellow}  Searching LaPack... ${ColourReset}")
        message("${ColourReset}")

        FindLibraryAndGetFlag(
            BLAS_FLAGS
            BLAS_CORRECT
            LIB_NAMES       blas
            LIB_PATH_HINT   ${LAPACK_PATH_HINT}
            IS_USER_DEFINED_PATH  ${LAPACK_DEFINED_PATH}   )

        FindLibraryAndGetFlag(
            LAPACK_FLAGS
            LAPACK_CORRECT
            LIB_NAMES       lapack
            LIB_PATH_HINT   ${LAPACK_PATH_HINT}
            IS_USER_DEFINED_PATH  ${LAPACK_DEFINED_PATH}   )

        if ( ${BLAS_CORRECT} AND ${LAPACK_CORRECT} )
            set( LAPACK_LIBRARIES "${LAPACK_FLAGS}" "${BLAS_FLAGS}" )
            set( LAPACK_FOUND TRUE )
        else()
            message("${Red}    NOT FOUND")
            message("${ColourReset}")
            message("${Green}    MKL_LIB_PATH_HINT : ${ColourReset}" ${MKL_LIB_PATH_HINT})
            message("${Green}    LAPACK_PATH_HINT  : ${ColourReset}" ${LAPACK_PATH_HINT})
            message("${Green}    BLAS_PATH_HINT    : ${ColourReset}" ${BLAS_PATH_HINT})
            message(FATAL_ERROR)
        endif()

    else()

        message("${ColourReset}")
        message("${Yellow}  Searching LaPack... ${ColourReset}")
        message("${ColourReset}")

        ## Automatic searching MPI Flag from system
        find_package(LAPACK)
        message("${ColourReset}")

    endif()

    if ( LAPACK_FOUND )
        message("${Green}    LAPACK_LIBRARIES:${ColourReset} " "${LAPACK_LIBRARIES}")
        set(MKL_LIB_FLAGS "${LAPACK_LIBRARIES}" )
        set(MKL_LIB_CORRECT TRUE)
    else()
        message("${Red}    NOT FOUND")
        set(MKL_LIB_CORRECT FALSE)
    endif()
    message("${ColourReset}")

endif()

if (NOT MKL_LIB_CORRECT)
    message("${Red}    MKL or LAPACK is not found.${ColourReset}")
else()
    message("${Green}    MKL or LAPACK is found.${ColourReset}")
endif()
message("${ColourReset}")
set(MKL_LIB "${MKL_LIB_FLAGS}")
