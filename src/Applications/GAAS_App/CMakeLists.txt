esma_set_this()

set (SRCS
  aod_util.F90  m_ana.F90  m_duplicate.F  m_obs.F90  m_simul.F90  m_sqc.F90
  )

# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${ALIGNCOM}")
endif ()

esma_add_library(${this}
  SRCS ${SRCS}
  DEPENDENCIES GMAO_psas GMAO_ods GMAO_hermes Chem_Base)


ecbuild_add_executable(TARGET ana_aod.x SOURCES ana_aod.F LIBS ${this} GMAO_gfio_r4 Chem_Base Chem_Shared) 
ecbuild_add_executable(TARGET mpana_aod.x SOURCES mpana_aod.F90 LIBS ${this} GMAO_gfio_r4 Chem_Base Chem_Shared) 

set(pythonscripts modis_l2a.py mxd04_l2a.py avhrr_l2a.py patmosx_l2a.py aod_data.py)
install(PROGRAMS ${pythonscripts} get_aero_obs.csh run_gaas_ana.csh DESTINATION bin)

install(
   FILES mxd04_nnr.py avhrr_nnr.py
   DESTINATION lib/Python
   )

if (EXTENDED_SOURCE)
  set_target_properties (${this} PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
  set_target_properties (ana_aod.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()

file(GLOB rc_files *.rc)
file(GLOB pcf_files *.pcf)
file(GLOB tmpl_files *.tmpl)

install (
   FILES ${rc_files} ${pcf_files} ${tmpl_files}
   DESTINATION etc
   )
