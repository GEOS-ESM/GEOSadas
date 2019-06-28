ecbuild_add_executable(
  TARGET oiqcbufr.x
  SOURCES oiqcbufr.f
  LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4 ${MPI_Fortran_LIBRARIES})

install(FILES gmao_prepqc prepqc.pl prepqc_daemon.pl DESTINATION bin)