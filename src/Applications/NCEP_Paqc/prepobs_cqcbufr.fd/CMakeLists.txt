

add_executable (cqcbufr.x cqcbufr.f radcor.f)
target_link_libraries (cqcbufr.x NCEP_bufr_r4i4 NCEP_w3_r4i4)
