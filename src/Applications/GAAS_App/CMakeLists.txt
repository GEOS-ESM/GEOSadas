esma_set_this()

set (SRCS
  aod_util.F90  m_ana.F90  m_duplicate.F  m_obs.F90  m_simul.F90  m_sqc.F90
  )

esma_add_library(${this}
  SRCS ${SRCS}
  DEPENDENCIES GMAO_psas GMAO_ods GMAO_hermes Chem_Base)

if (EXTENDED_SOURCE)
  set_target_properties (${this} PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()


