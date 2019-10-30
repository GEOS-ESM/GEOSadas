esma_set_this()
set (SRCS
  supvit_modules.f supvit_main.f)

# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

ecbuild_add_executable(TARGET supvit.x SOURCES ${SRCS} LIBS NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4)
set_target_properties (supvit.x PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this})

string(REPLACE " " ";" tmp ${FREAL8})
foreach(flag ${tmp})
   target_compile_options (supvit.x PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
endforeach()

