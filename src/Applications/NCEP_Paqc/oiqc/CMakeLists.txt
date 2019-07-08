if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT2}")
endif ()

ecbuild_add_executable(
  TARGET oiqcbufr.x
  SOURCES oiqcbufr.f
  LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4 ${MPI_Fortran_LIBRARIES})

install(PROGRAMS gmao_prepqc prepqc.pl DESTINATION bin)

set (DASPERL /usr/bin/perl)
configure_file(prepqc_daemon.pl prepqc_daemon.pl @ONLY)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/prepqc_daemon.pl DESTINATION bin)

install(FILES prepobs_oiqc.oberrs DESTINATION etc)
