esma_set_this ()

set (SRCS
  calcbal.F90   delmod.f90  genqsat.f90
  getcases.f90 m_getcases.f90 m_vordiv.f90
  m_fvHeader.f90 m_GsiGrided.f90 horlength.F90 isrchfge.f90 mat.f90
  postmod.f90  m_readpairs.F90 smoothz.f90 specgrid.f90
  m_stvp.f90 sstmod.f90 m_speclap.f90 m_calap.f90 compact_diffs.f90
  comm_mod.f90 deter_subdomain.f90 init_commvars.f90
  type_kinds.F90 variables.f90 variances.F90
  vertlength.F90 addoz.f90 m_nc_berror.f90
  )

# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FPE0}" "${BIG_ENDIAN} ${BYTERECLEN} ${FPE0}" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES NCEP_bacio_r4i4 NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r8 GMAO_hermes GMAO_transf ${MKL_LIBRARIES}
)

add_definitions (-D_LAPACK_ -Dgmao_intf)

ecbuild_add_executable(TARGET calcstats.x SOURCES statsmain.F90 LIBS ${this} ${MKL_LIBRARIES})
ecbuild_add_executable(TARGET write_berror_global.x SOURCES write_berror_global.f90 LIBS ${this} ${MKL_LIBRARIES})
ecbuild_add_executable(TARGET ut_sst_berror.x SOURCES ut_sst_berror.f90 LIBS ${this} ${MKL_LIBRARIES})

file (GLOB tmpl_files *.tmpl)
install (FILES ${tmpl_files} DESTINATION etc)

install (FILES stats.parm.sample DESTINATION etc RENAME stats.parm)
