# This is equivalent to FOPT= in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FOPT3}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

ecbuild_add_executable (
  TARGET acarsqc.x
  SOURCES acarsqc.f
  LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4 NCEP_bacio_r4i4)

if (EXTENDED_SOURCE)
  set_target_properties (acarsqc.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()

file(GLOB parm_files *.parm)
install(FILES ${parm_files} DESTINATION etc)
