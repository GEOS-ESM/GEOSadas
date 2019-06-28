if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT4}")
endif ()

add_executable (profcqc.x profcqc.f)
target_link_libraries (profcqc.x NCEP_bufr_r4i4 NCEP_w3_r4i4)
