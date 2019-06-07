add_executable (block block_09.F)
target_link_libraries (block NCEP_bufr_r4i4)

add_executable (unblock block_09.F)
target_compile_definitions (unblock PRIVATE -DUNBLOCK)
target_link_libraries (unblock NCEP_bufr_r4i4)
