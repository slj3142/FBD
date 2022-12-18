set(ITEM_TYPE "test")
set(ITEM_NAME "GURU")
set(SRC_${ITEM_NAME}
    src/testGURUModule.F90
    src/testGURU.F90
)

set(DEPEND_${ITEM_NAME}
    EaFort
)

##... Compile
AutoCompile(
    IS_COMPILE_EXE      True
    INSTALL_NAME        "test${ITEM_NAME}"
    INSTALL_PATH        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    INSTALL_SRC         "${SRC_${ITEM_NAME}}"
    INSTALL_DEPEND      "${DEPEND_${ITEM_NAME}}"
)
