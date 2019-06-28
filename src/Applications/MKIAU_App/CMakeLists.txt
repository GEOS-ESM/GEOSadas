set (BINS mkiau mp_stats pod rstupd updAincWpert mp_dyn_stats stochphysics)

if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${TRACEBACK} ${ALIGNCOM}")
endif ()

if (FV_PRECISION MATCHES R4)
  set (GFDL_fms GFDL_fms_r4)
else ()
  set (GFDL_fms GFDL_fms_r8)
endif ()
  
set (LIBS
  NCEP_w3_r4i4 GMAO_gfio_r8 MAPL_cfio_r4 MAPL_Base GMAO_hermes fvgcm ${GFDL_fms} ${MKL_LIBRARIES}
  FVdycoreCubed_GridComp GMAO_stoch GEOSmkiau_GridComp GEOSmoist_GridComp GEOSchem_GridComp GFDL_fms_r8)

foreach (bin ${BINS})
  ecbuild_add_executable (
     TARGET ${bin}.x
     SOURCES ${bin}.F90 
     LIBS NCEP_w3_r4i4 GMAO_gfio_r8 MAPL_cfio_r4 MAPL_Base GMAO_hermes fvgcm ${MKL_LIBRARIES} FVdycoreCubed_GridComp GMAO_stoch GEOSmkiau_GridComp GEOSmoist_GridComp GEOSchem_GridComp GFDL_fms_r8 
     )
endforeach()
