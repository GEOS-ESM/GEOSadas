ecbuild_add_executable (
  TARGET change_said.x
  SOURCES change_said.f
  LIBS NCEP_bufr_r4i4)

ecbuild_add_executable (
  TARGET fix_ascat.x
  SOURCES fix_ascat.f
  LIBS NCEP_bufr_r4i4)

ecbuild_add_executable (
  TARGET explode.x
  SOURCES explode.f
  LIBS NCEP_bufr_r4i4)

ecbuild_add_executable (
  TARGET twindow.x
  SOURCES twindow.f
  LIBS NCEP_bufr_r4i4 NCEP_w3_r4i4 GMAO_mpeu)

if (EXTENDED_SOURCE)
  set_target_properties (twindow.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()

