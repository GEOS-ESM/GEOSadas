esma_set_this ()
string (REPLACE NCEP_enkf_ "" case ${this})

if (case MATCHES "aero")
  set (XSRCS
    gridinfo_geos5.f90
    gridio_aergeos5.f90
    smooth_geos5.f90
    )
 ecbuild_add_executable(TARGET enkf_aero.x SOURCES enkf_main.f90 LIBS GSI_GridComp_with_stubs ${this} OpenMP::OpenMP_Fortran)
elseif (case MATCHES "geos")
  set (XSRCS
    gridinfo_geos5.f90
    gridio_geos5.f90
    smooth_geos5.f90
    )
 ecbuild_add_executable(TARGET ene_adaptinf.x SOURCES ene_adaptinf.f90 LIBS GSI_GridComp_with_stubs )
 ecbuild_add_executable(TARGET enkf_obs2ods.x SOURCES enkf_obs2ods.F90 LIBS GSI_GridComp_with_stubs GMAO_ods)
 ecbuild_add_executable(TARGET enkf_geos.x SOURCES enkf_main.f90 LIBS GSI_GridComp_with_stubs ${this} OpenMP::OpenMP_Fortran)
elseif (case MATCHES "gfs")
  set (XSRCS
    gridinfo_gfs.f90
    gridio_gfs.f90
    smooth_gfs.f90
    )
elseif (case MATCHES "wrf")
  set (XSRCS)
endif ()

set (SRCS
  common_mtx.f90
  covlocal.f90
  enkf.f90
  enkf_main.f90
  enkf_obs_sensitivity.f90
  fftpack.f90
  fso.f90
  genqsat1.f90
  inflation.f90
  innovstats.f90
  kdtree2.f90
  letkf.F90
  loadbal.f90
  mpi_readobs.f90
  mpisetup.F90
  netlib.f90
  enkf_obsmod.f90
  params.f90
  quicksort.f90
  radbias.f90
  read_locinfo.f90
  readaerobs.f90
  readconvobs.f90
  readozobs.f90
  readsatobs.f90
  reducedgrid.f90
  rnorm.f90
  smooth.F90
  sorting.f90
  specmod.f90
  statevec.f90
  write_logfile.f90
  ${XSRCS}
  )

if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT1} ${TRACEBACK} ${BIG_ENDIAN} ${FPE0} ${ALIGNCOM}")
endif ()


esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES GSI_GridComp_with_stubs NCEP_sp_r8i4 NCEP_w3_r8i4 NCEP_bacio_r4i4 NCEP_nemsio NCEP_bufr_r8i4 GMAO_gfio_r8 GMAO_ods Chem_Shared
  )

add_subdirectory(scripts)
