add_executable (cqcvad.x cqcvad.f)
target_link_libraries (cqcvad.x PUBLIC NCEP_bufr_r4i4 NCEP_w3_r4i4)
