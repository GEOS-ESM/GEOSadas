esma_set_this()
set (SRCS
  supvit_modules.f supvit_main.f)

esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4)

ecbuild_add_executable(TARGET supvit.x SOURCES supvit_main.f supvit_modules.f LIBS ${this} NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4)