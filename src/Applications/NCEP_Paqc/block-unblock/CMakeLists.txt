ecbuild_add_executable (
  TARGET block
  SOURCES block_09.F
  LIBS NCEP_bufr_r4i4)

ecbuild_add_executable (
  TARGET unblock
  SOURCES block_09.F
  DEFINITIONS UNBLOCK
  LIBS NCEP_bufr_r4i4)

