ecbuild_add_executable (
  TARGET gettrk.x
  SOURCES gettrk_main.f gettrk_modules.f
  LIBS NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4 NCEP_bufr_r4i4 NCEP_bacio_r4i4)
set_target_properties (gettrk.x PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
