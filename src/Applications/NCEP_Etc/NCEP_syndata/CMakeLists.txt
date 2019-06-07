esma_set_this ()

esma_add_library (${this}
  SRCS gblevents.f spherp.f
  DEPENDENCIES NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4 NCEP_bufr_r4i4
  )

add_executable (syndat_sindata.x syndata.f)
target_link_libraries (syndat_sindata.x PUBLIC ${this} )
