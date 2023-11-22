# This is equivalent to FOPT= in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FOPT3}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

ecbuild_add_executable (
  TARGET combfr.x
  SOURCES combfr.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET combfrd.x
  SOURCES combfrd.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET scanbuf0.x
  SOURCES scanbuf0.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET scanbuf0_accum.x
  SOURCES scanbuf0_accum.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET scanbuf2.x
  SOURCES scanbuf2.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET chkbufr.x
  SOURCES chkbufr.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET cp_2ssi.x
  SOURCES cp_2ssi.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)
