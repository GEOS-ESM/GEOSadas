
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
