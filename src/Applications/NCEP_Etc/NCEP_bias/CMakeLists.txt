esma_set_this ()

set (SRCS
  determine_time_levels.f90 m_sisind.f90 m_sischnTable.F90 bias_mpeu.F90
)

# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FPE0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES GSI_GridComp GMAO_mpeu
  )

foreach (prog init_coeffs sac_convert sbc_convert sif_convert write_biascr_option)
  ecbuild_add_executable (
    TARGET ${prog}.x
    SOURCES ${prog}.f90
    LIBS ${this})
endforeach()

ecbuild_add_executable (TARGET sac.x SOURCES main.f90 LIBS ${this})

install (
   FILES sac.pl sac_convert sbc_convert sif_convert
   DESTINATION bin)
