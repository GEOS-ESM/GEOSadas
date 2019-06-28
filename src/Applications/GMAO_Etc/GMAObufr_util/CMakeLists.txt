if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -convert big_endian -list -fp-model strict -assume noold_ldout_format -assume byterecl")
endif ()

add_executable (tranavhrr.x tranavhrr.f90 avhrr.f90 func.f90 w3trnarg.f)
target_link_libraries (tranavhrr.x PUBLIC NCEP_bufr_r4i4 NCEP_w3_r4i4)

add_executable (table_and_timestamp.x table_and_timestamp.f90)
target_link_libraries (table_and_timestamp.x PUBLIC NCEP_bufr_r4i4 NCEP_w3_r4i4)


