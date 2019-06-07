add_executable (gsi.x gsimain.F90)
target_link_libraries (gsi.x PUBLIC GSI_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4 GMAO_mpeu)

add_executable (GSIsa.x GSIsa.F90)
target_link_libraries (GSIsa.x PUBLIC GEOSaana_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4 MAPL_Base GMAO_mpeu)

#add_executable (GSIsa_Gaussian.x GSIsa_Gaussian.F90 gauss_lat_nmc.f)
#target_link_libraries (GSIsa_Gaussian.x PUBLIC GEOSaana_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4 MAPL_Base GMAO_mpeu )

add_executable (gsidiag_bin2txt.x gsidiag_bin2txt.f90 gauss_lat_nmc.f)
target_link_libraries (gsidiag_bin2txt.x PUBLIC GEOSaana_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4)

