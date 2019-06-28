# This is equivalent to FOPT= in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT2}")
endif ()

ecbuild_add_executable (
  TARGET prepacqc.x
  SOURCES prepacqc.f
  LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4)