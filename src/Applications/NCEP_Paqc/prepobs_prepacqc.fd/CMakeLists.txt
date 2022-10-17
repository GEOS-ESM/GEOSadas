# This is equivalent to FOPT= in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT2}")
endif ()

ecbuild_add_executable (
  TARGET prepacqc_profl.x
  SOURCES prepacqc.f acftobs_qc.f indexc40.f input_acqc.f output_acqc_noprof.f output_acqc_prof.f sub2mem_mer.f sub2mem_um.f tranQCflags.f pmat.f90 pmat2.f90 pmat3.f90 pietc.f90 pspl.f90 pkind.f90
  LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4)

file(GLOB parm_files *.parm)
install(FILES ${parm_files} DESTINATION etc)
