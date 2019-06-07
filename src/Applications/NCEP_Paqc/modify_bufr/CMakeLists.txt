
add_executable (change_said.x change_said.f)
target_link_libraries (change_said.x PUBLIC NCEP_bufr_r4i4)

add_executable (fix_ascat.x fix_ascat.f)
target_link_libraries (fix_ascat.x PUBLIC NCEP_bufr_r4i4)

add_executable (explode.x explode.f)
target_link_libraries (explode.x PUBLIC NCEP_bufr_r4i4)

