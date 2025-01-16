if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(BIG_ENDIAN "-fconvert=swap")
endif()

add_subdirectory (GMAO_Shared)

