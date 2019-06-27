
ecbuild_add_executable (TARGET tranavhrr.x SOURCES tranavhrr.f90 avhrr.f90 func.f90 w3trnarg.f LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4)

ecbuild_add_executable (TARGET table_and_timestamp.x SOURCES table_and_timestamp.f90 LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4)


