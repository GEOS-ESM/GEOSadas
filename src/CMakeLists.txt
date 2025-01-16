# 1/16/2025:  test if needed by gfortran-13
#
#  CMake Error at src/GMAO_Shared/GMAO_ods/CMakeLists.txt:81 (set_target_properties):
#  set_target_properties called with incorrect number of arguments.
#
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(BIG_ENDIAN "-fconvert=swap")
endif()

add_subdirectory (GMAO_Shared)

