
add_executable (acarsqc.x acarsqc.f)
target_link_libraries (acarsqc.x PUBLIC NCEP_bufr_r4i4 NCEP_w3_r4i4 NCEP_bacio_r4i4)
if (EXTENDED_SOURCE)
  set_target_properties (acarsqc.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()
