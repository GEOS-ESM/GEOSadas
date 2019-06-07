add_executable (gmao_prevents.x gblevents_gmao.f prevents.f)
target_link_libraries (gmao_prevents.x PUBLIC NCEP_bufr_r8i4 NCEP_w3_r8i4 GMAO_hermes)
