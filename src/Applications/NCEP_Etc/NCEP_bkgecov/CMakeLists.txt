esma_set_this ()

set (SRCS
  calcbal.F90   m_rdgrids.f90  genqsat.f90
  getcases.f90 m_getcases.f90 m_vordiv.f90
  m_fvHeader.f90 m_GsiGrided.f90 horlength.F90 isrchfge.f90 mat.f90
  postmod.f90  m_readpairs.F90 smoothz.f90 specgrid.f90
  m_stvp.f90 sstmod.f90 m_speclap.f90 m_calap.f90 compact_diffs.f90
  type_kinds.F90 variables.f90 variances.F90
  vertlength.F90 addoz.f90
  )

esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES NCEP_bacio_r4i4 NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r8 GMAO_hermes GMAO_transf
)
