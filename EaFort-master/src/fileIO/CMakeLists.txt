set(ITEM_NAME "fileIO")
set(SRC_CURRENT_${ITEM_NAME}
    src/modFileIO.F90
)

foreach( SUB_SRC_NAME IN LISTS SRC_CURRENT_${ITEM_NAME} )

    list( APPEND SRC_GATHER_${ITEM_NAME}
            ${CMAKE_CURRENT_SOURCE_DIR}/${SUB_SRC_NAME}
        )

endforeach()

set( SRC_${ITEM_NAME} "${SRC_GATHER_${ITEM_NAME}}" PARENT_SCOPE )
