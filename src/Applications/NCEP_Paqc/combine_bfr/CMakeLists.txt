
ecbuild_add_executable (
  TARGET combfr.x
  SOURCES combfr.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET combfrd.x
  SOURCES combfrd.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET scanbuf0.x
  SOURCES scanbuf0.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET scanbuf2.x
  SOURCES scanbuf2.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET chkbufr.x
  SOURCES chkbufr.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)


ecbuild_add_executable (
  TARGET cp_2ssi.x
  SOURCES cp_2ssi.f
  LIBS NCEP_bufr_r4i4 GMAO_mpeu)
