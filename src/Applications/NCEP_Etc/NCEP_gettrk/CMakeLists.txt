# This is equivalent to FOPT= in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FOPT3}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

ecbuild_add_executable(TARGET gettrk.x SOURCES gettrk_main.f gettrk_modules.f LIBS NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4 NCEP_bufr_r4i4 NCEP_bacio_r4i4)

if (EXTENDED_SOURCE)
  set_target_properties (gettrk.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()

install (
   PROGRAMS vtrack vtrack_training
   DESTINATION bin)

file (GLOB ctl_files *.ctl)
file (GLOB rc_files *.rc)
file (GLOB table_files *.table)
install (
   FILES ${ctl_files} ${rc_files} ${table_files} vtx.ctl.tmpl vtrack.ctl.tmpl
   DESTINATION etc
   )
