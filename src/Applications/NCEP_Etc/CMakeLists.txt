esma_check_if_debug()

esma_add_subdirectories(
  NCEP_bkgecov NCEP_gettrk NCEP_supvit NCEP_syndata NCEP_vtxreloc NCEP_bias
  )

if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/NCEP_enkf)
  add_subdirectory (NCEP_enkf NCEP_enkf_geos)
  add_subdirectory (NCEP_enkf NCEP_enkf_aero)
endif ()
  

