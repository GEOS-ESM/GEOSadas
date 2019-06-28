# This is equivalent to FOPT= in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FOPT3}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

add_executable (combfr.x combfr.f)
target_link_libraries (combfr.x NCEP_bufr_r4i4 GMAO_mpeu)

add_executable (combfrd.x combfrd.f)
target_link_libraries (combfrd.x NCEP_bufr_r4i4 GMAO_mpeu)

add_executable (scanbuf0.x scanbuf0.f)
target_link_libraries (scanbuf0.x NCEP_bufr_r4i4 GMAO_mpeu)

add_executable (scanbuf2.x scanbuf2.f)
target_link_libraries (scanbuf2.x NCEP_bufr_r4i4 GMAO_mpeu)

add_executable (chkbufr.x chkbufr.f)
target_link_libraries (chkbufr.x NCEP_bufr_r4i4 GMAO_mpeu)

add_executable (cp_2ssi.x cp_2ssi.f)
target_link_libraries (cp_2ssi.x NCEP_bufr_r4i4 GMAO_mpeu)
