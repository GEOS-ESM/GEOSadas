esma_set_this ()

# This is equivalent to FOPT=$(FOPT2) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FOPT3}" "${FOPT2}" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

esma_add_library (${this}
  SRCS gblevents.f spherp.f
  DEPENDENCIES NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4 NCEP_bufr_r4i4
  )

add_executable (syndat_sindata.x syndata.f)
target_link_libraries (syndat_sindata.x PUBLIC ${this} )
