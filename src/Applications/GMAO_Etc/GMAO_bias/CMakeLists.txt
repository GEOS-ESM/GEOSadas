ecbuild_add_executable(TARGET cft_update.x SOURCES cft_update.f LIBS GMAO_mpeu)
ecbuild_add_executable(TARGET convert_cft.x SOURCES convert_cft.f LIBS GMAO_mpeu)
ecbuild_add_executable(TARGET tailwrite.x SOURCES tailwrite.f)

if (EXTENDED_SOURCE)
  set_target_properties (convert_cft.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
  set_target_properties (cft_update.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()

if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FPE0}" "${BIG_ENDIAN} ${FPE0}" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

install (FILES gmao_acft_bias.parm DESTINATION etc)
