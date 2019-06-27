set (BINS mkiau mp_stats pod rstupd updAincWpert mp_dyn_stats stochphysics)

foreach (bin ${BINS})
  ecbuild_add_executable (TARGET ${bin}.x  SOURCES ${bin}.F90 LIBS NCEP_w3_r4i4 GMAO_gfio_r8 MAPL_cfio_r4 MAPL_Base GMAO_hermes fvgcm ${MKL_LIBRARIES} FVdycoreCubed_GridComp GMAO_stoch GEOSmkiau_GridComp GEOSmoist_GridComp GEOSchem_GridComp GFDL_fms_r8 )
  #target_link_libraries (${bin}.x NCEP_w3_r4i4 GMAO_gfio_r8 MAPL_cfio_r4 MAPL_Base GMAO_hermes fvgcm ${MKL_LIBRARIES} FVdycoreCubed_GridComp GMAO_stoch GEOSmkiau_GridComp GEOSmoist_GridComp GEOSchem_GridComp GFDL_fms_r8)
endforeach()
