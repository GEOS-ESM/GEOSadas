ecbuild_add_executable (
  TARGET gmao_prevents.x
  SOURCES gblevents_gmao.f prevents.f
  LIBS NCEP_bufr_r8i4 NCEP_w3_r8i4 GMAO_hermes)

file(GLOB parm_files *.parm)
install (FILES ${parm_files} prepobs_errtable.global DESTINATION etc)
