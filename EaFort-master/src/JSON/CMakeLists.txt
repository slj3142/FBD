set(ITEM_NAME "JSON")
set(SRC_CURRENT_${ITEM_NAME}
    json-fortran/src/json_file_module.F90
    json-fortran/src/json_kinds.F90
    json-fortran/src/json_module.F90
    json-fortran/src/json_parameters.F90
    json-fortran/src/json_string_utilities.F90
    json-fortran/src/json_value_module.F90

    src/modJSON.F90
)

foreach( SUB_SRC_NAME IN LISTS SRC_CURRENT_${ITEM_NAME} )

    list( APPEND SRC_GATHER_${ITEM_NAME}
            ${CMAKE_CURRENT_SOURCE_DIR}/${SUB_SRC_NAME}
        )

endforeach()

set( SRC_${ITEM_NAME} "${SRC_GATHER_${ITEM_NAME}}" PARENT_SCOPE )
