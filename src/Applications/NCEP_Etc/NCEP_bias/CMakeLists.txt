esma_set_this ()

set (SRCS
  determine_time_levels.f90 m_sisind.f90 m_sischnTable.F90 bias_mpeu.F90
)

esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES GSI_GridComp GMAO_mpeu
  )

foreach (prog init_coeffs main sac_convert sbc_convert sif_convert write_biascr_option testtbl)
  add_executable (${prog}.x ${prog}.f90)
  target_link_libraries (${prog}.x PUBLIC ${this})
endforeach()
