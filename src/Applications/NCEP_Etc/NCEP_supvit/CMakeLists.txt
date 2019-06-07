add_executable (supvit.x supvit_main.f supvit_modules.f)
target_link_libraries(supvit.x PUBLIC NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4)
set_target_properties (supvit.x PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
