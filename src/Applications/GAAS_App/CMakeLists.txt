esma_set_this()

set (SRCS
  aod_util.F90  m_ana.F90  m_duplicate.F  m_obs.F90  m_simul.F90  m_sqc.F90
  )

esma_add_library(${this}
  SRCS ${SRCS}
  DEPENDENCIES GMAO_psas GMAO_ods GMAO_hermes Chem_Base)

file(GLOB pythonscripts *.py)

ecbuild_add_executable(TARGET ana_aod.x SOURCES ana_aod.F LIBS ${this} GMAO_gfio_r4 Chem_Base Chem_Shared) 
ecbuild_add_executable(TARGET mpana_aod.x SOURCES mpana_aod.F90 LIBS ${this} GMAO_gfio_r4 Chem_Base Chem_Shared) 
install(FILES ${pythonscripts} get_aero_obs.csh run_gaas_ana.csh DESTINATION bin)

if (EXTENDED_SOURCE)
  set_target_properties (${this} PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
  set_target_properties (ana_aod.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()
