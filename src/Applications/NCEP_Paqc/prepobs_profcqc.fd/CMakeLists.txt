if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT4}")
endif ()

ecbuild_add_executable (
  TARGET profcqc.x
  SOURCES profcqc.f
  LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4)

file(GLOB parm_files *.parm)
install(FILES ${parm_files} DESTINATION etc)
