esma_set_this ()

esma_add_library (${this}
  SRCS gblevents.f spherp.f
  DEPENDENCIES NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4 NCEP_bufr_r4i4
  )

ecbuild_add_executable (TARGET syndat_sindata.x SOURCES syndata.f LIBS ${this})
