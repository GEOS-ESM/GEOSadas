ecbuild_add_executable (
  TARGET gmao_prevents.x
  SOURCES gblevents_gmao.f prevents.f
  LIBS NCEP_bufr_r8i4 NCEP_w3_r8i4 GMAO_hermes)

ecbuild_add_executable (
  TARGET check_virtmp.x
  SOURCES check_virtmp.f
  LIBS NCEP_bufr_r4i4)

string(REPLACE " " ";" tmp ${FREAL8})
foreach (flag ${tmp})
   target_compile_options (gmao_prevents.x PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
endforeach ()

file(GLOB parm_files *.parm)
install (FILES ${parm_files} prepobs_errtable.global DESTINATION etc)
