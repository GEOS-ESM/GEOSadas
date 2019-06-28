# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${BIG_ENDIAN} ${BYTERECLEN} ${FP_MODEL_STRICT} ${ALIGNCOM}")
endif ()

ecbuild_add_executable (
  TARGET gsi.x
  SOURCES gsimain.F90
  LIBS GSI_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4 GMAO_mpeu)

ecbuild_add_executable (
  TARGET GSIsa.x
  SOURCES GSIsa.F90
  LIBS GEOSaana_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4 MAPL_Base GMAO_mpeu)

ecbuild_add_executable (
  TARGET gsidiag_bin2txt.x
  SOURCES gsidiag_bin2txt.f90 gauss_lat_nmc.f
  LIBS GEOSaana_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4)

install(FILES fvssi fvssisetup DESTINATION bin) 
