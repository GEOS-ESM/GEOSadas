set (BINS dyn2cubed mkiau mp_stats pod rstupd updAincWpert mp_dyn_stats stochphysics)

if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${TRACEBACK} ${ALIGNCOM}")
endif ()

if (FV_PRECISION MATCHES R4)
  set (fms fms_r4)
else ()
  set (fms fms_r8)
endif ()
  
set (LIBS
   NCEP_w3_r4i4 GMAO_gfio_r8 MAPL_cfio_r4 MAPL GMAO_hermes fvgcm ${fms} ${MKL_LIBRARIES}
  FVdycoreCubed_GridComp GMAO_stoch GEOSmkiau_GridComp GEOSmoist_GridComp GEOSchem_GridComp fms_r8)

foreach (bin ${BINS})
  ecbuild_add_executable (
     TARGET ${bin}.x
     SOURCES ${bin}.F90 
     LIBS NCEP_w3_r4i4 GMAO_gfio_r8 MAPL_cfio_r4 MAPL GMAO_hermes fvgcm ${MKL_LIBRARIES} FVdycoreCubed_GridComp GMAO_stoch GEOSmkiau_GridComp GEOSmoist_GridComp GEOSchem_GridComp fms_r8
     )
endforeach()

file (GLOB rc_tmpl_files *.rc.tmpl)
install (FILES ${rc_tmpl_files} DESTINATION etc)
