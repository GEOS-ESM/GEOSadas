# This is equivalent to FOPT= in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FOPT3}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FPE0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

ecbuild_add_executable (
  TARGET block
  SOURCES block_09.F
  LIBS NCEP_bufr_r4i4)

ecbuild_add_executable (
  TARGET unblock
  SOURCES block_09.F
  DEFINITIONS UNBLOCK
  LIBS NCEP_bufr_r4i4)

ecbuild_add_executable (
  TARGET reblock
  SOURCES reblock.c)
