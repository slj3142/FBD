set(ITEM_NAME "string")
set(SRC_CURRENT_${ITEM_NAME}
    StringiFor/src/BeFoR64/befor64_pack_data_m.F90
    StringiFor/src/BeFoR64/befor64.F90

    StringiFor/src/FACE/face.F90

    StringiFor/src/PENF/penf_b_size.F90
    StringiFor/src/PENF/penf.F90
    StringiFor/src/PENF/penf_global_parameters_variables.F90
    StringiFor/src/PENF/penf_stringify.F90

    StringiFor/src/StringiFor/stringifor.F90
    StringiFor/src/StringiFor/stringifor_string_t.F90

    src/modString.F90
)

foreach( SUB_SRC_NAME IN LISTS SRC_CURRENT_${ITEM_NAME} )

    list( APPEND SRC_GATHER_${ITEM_NAME}
            ${CMAKE_CURRENT_SOURCE_DIR}/${SUB_SRC_NAME}
        )

endforeach()

set( SRC_${ITEM_NAME} "${SRC_GATHER_${ITEM_NAME}}" PARENT_SCOPE )
