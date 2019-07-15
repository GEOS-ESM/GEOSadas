esma_set_this ()

set (SRCS
  determine_time_levels.f90 m_sisind.f90 m_sischnTable.F90 bias_mpeu.F90
)

# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${BIG_ENDIAN} ${ALIGNCOM}")
endif ()

esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES GSI_GridComp_with_stubs GMAO_mpeu
  )

foreach (prog init_coeffs sac_convert sbc_convert sif_convert write_biascr_option)
  ecbuild_add_executable (
    TARGET ${prog}.x
    SOURCES ${prog}.f90
    LIBS ${this})
endforeach()

ecbuild_add_executable (TARGET sac.x SOURCES main.f90 LIBS ${this})

install (
   PROGRAMS sac.pl sac_convert sbc_convert sif_convert
   DESTINATION bin)

install (FILES sac.nl.tmpl DESTINATION etc)
