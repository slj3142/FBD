##... Source Files
set(ITEM_TYPE "LIB")
set(ITEM_NAME "EaFort")
set(SRC_${ITEM_NAME}
    ##.... Wrapper Module of EaFort
    modEaFort.F90
    pkgEaFort.F90
)

set(LIST_SUB_ITEM_NAME
    string
    JSON
    GURU
    OS
    fileIO
)

if ( IS_MATHFACK_COMPILE )
    set(LIST_SUB_ITEM_NAME "${LIST_SUB_ITEM_NAME}" mathPack)
endif()

message(${LIST_SUB_ITEM_NAME})

foreach( SUB_ITEM_NAME IN LISTS LIST_SUB_ITEM_NAME )
    add_subdirectory( ${SUB_ITEM_NAME} )
    set( SRC_${ITEM_NAME} ${SRC_${ITEM_NAME}} ${SRC_${SUB_ITEM_NAME}} )
endforeach()

##... Compile
AutoCompile(
    IS_COMPILE_LIB      True
    IS_DYNAMIC_LIB      False
    INSTALL_NAME        "${LIB_NAME_${ITEM_NAME}}"
    INSTALL_PATH        "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    INSTALL_DEPEND      "${DEPEND_${ITEM_NAME}}"
    INSTALL_SRC         "${SRC_${ITEM_NAME}}"
    INSTALL_EXTLIB      "${EXTLIB_${ITEM_NAME}}"
)
