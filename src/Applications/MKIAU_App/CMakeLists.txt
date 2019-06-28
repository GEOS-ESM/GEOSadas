set (BINS mkiau mp_stats pod rstupd updAincWpert mp_dyn_stats stochphysics)

# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${TRACEBACK} ${ALIGNCOM}")
endif ()

foreach (bin ${BINS})
  add_executable (${bin}.x  ${bin}.F90 ) #iau_stubs.F90)
  target_link_libraries (${bin}.x NCEP_w3_r4i4 GMAO_gfio_r8 MAPL_cfio_r4 MAPL_Base GMAO_hermes fvgcm ${MKL_LIBRARIES} FVdycoreCubed_GridComp GMAO_stoch GEOSmkiau_GridComp GEOSmoist_GridComp GEOSchem_GridComp GFDL_fms_r8)
endforeach()
