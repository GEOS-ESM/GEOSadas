
add_executable(oiqcbufr.x oiqcbufr.f)
target_link_libraries (oiqcbufr.x PUBLIC NCEP_bufr_r4i4 NCEP_w3_r4i4 ${MPI_Fortran_LIBRARIES})
