
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

