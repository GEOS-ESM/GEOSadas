esma_set_this()

ecbuild_add_executable (
  TARGET gmao_prevents.x
  SOURCES gblevents_gmao.f prevents.f
  LIBS NCEP_bufr_r8i4 NCEP_w3_r8i4 GMAO_hermes)
set_target_properties(gmao_prevents.x PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this})

string(REPLACE " " ";" tmp ${FREAL8})
foreach (flag ${tmp})
   target_compile_options (gmao_prevents.x PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
endforeach ()

file(GLOB parm_files *.parm)
install (FILES ${parm_files} prepobs_errtable.global DESTINATION etc)
