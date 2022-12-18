set(ITEM_NAME "mathPack")
set(SRC_CURRENT_${ITEM_NAME}
    src/modMathPack.F90
    src/pkgMathPack.F90

    src/basicFunctions/modBasicFunc.F90
    src/specialFunctions/modSpecialFunc.F90
    src/linearAlgebra/modLinearAlgebra.F90

)

foreach( SUB_SRC_NAME IN LISTS SRC_CURRENT_${ITEM_NAME} )

    list( APPEND SRC_GATHER_${ITEM_NAME}
            ${CMAKE_CURRENT_SOURCE_DIR}/${SUB_SRC_NAME}
        )

endforeach()

set( SRC_${ITEM_NAME} "${SRC_GATHER_${ITEM_NAME}}" PARENT_SCOPE )
