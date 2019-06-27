ecbuild_add_executable(TARGET cft_update.x SOURCES cft_update.f LIBS GMAO_mpeu)
ecbuild_add_executable(TARGET convert_cft.x SOURCES convert_cft.f LIBS GMAO_mpeu)

if (EXTENDED_SOURCE)
  set_target_properties (convert_cft.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
  set_target_properties (cft_update.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()
