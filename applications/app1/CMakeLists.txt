SET(ITEM_PREFIX "APP")
SET(ITEM_NAME   "app1")

set(${ITEM_PREFIX}_SRC_${ITEM_NAME}
    ##.... Wrapper Module of EaFort
    src/2DBEM.F90
    src/module/modParam.F90
)

##... Compile
AutoCompile(
    IS_COMPILE_EXE      True
    INSTALL_NAME        "${${ITEM_PREFIX}_NAME_${ITEM_NAME}}"
    INSTALL_SRC         "${${ITEM_PREFIX}_SRC_${ITEM_NAME}}"
    INSTALL_PATH        "${${ITEM_PREFIX}_PATH_${ITEM_NAME}}"
    INSTALL_DEPEND      "${${ITEM_PREFIX}_DEPEND_${ITEM_NAME}}"
    INSTALL_EXTLIB      "${${ITEM_PREFIX}_EXTLIB_${ITEM_NAME}}"
    INSTALL_EXTINC      "${${ITEM_PREFIX}_EXTINC_${ITEM_NAME}}"
    INSTALL_EXTFLAG     "${${ITEM_PREFIX}_EXTFLAG_${ITEM_NAME}}"
)
