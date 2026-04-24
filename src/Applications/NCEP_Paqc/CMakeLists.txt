esma_check_if_debug()

if( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )
  add_compile_options("-fdec" "-fallow-argument-mismatch" "-fallow-invalid-boz")
endif()

esma_add_subdirectories(
  block-unblock combine_bfr oiqc prepobs_acarsqc.fd radcor
  prepobs_cqcbufr.fd prepobs_prepacqc.fd prepobs_profcqc.fd
  GMAOprev prepobs_cqcvad.fd modify_bufr
  )



