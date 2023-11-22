# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${BIG_ENDIAN} ${BYTERECLEN} ${FP_MODEL_STRICT} ${ALIGNCOM}")
endif ()

#####################################################################################
# # Currently not installed in GNU Make                                             #
# ecbuild_add_executable (                                                          #
#   TARGET gsi.x                                                                    #
#   SOURCES gsimain.F90                                                             #
#   LIBS GEOSaana_GridComp GSI_GridComp GEOSgsi_Coupler                             #
#        NCEP_crtm NCEP_sfcio NCEP_bufr_r8i4 NCEP_nemsio NCEP_bacio_r4i4 NCEP_gfsio #
#        NCEP_sp_r8i4 NCEP_w3_r8i4 nc_diag_read nc_diag_write)                      #
#####################################################################################

ecbuild_add_executable (
  TARGET GSIsa.x
  SOURCES GSIsa.F90
  LIBS GEOSaana_GridComp GSI_GridComp GEOSgsi_Coupler NCEP_sigio fvgcm NCEP_crtm
       GMAO_transf GMAO_hermes GMAO_gfio_r8 GEOSagcmPert_GridComp
       GEOS_PertShared GEOSphysicsPert_GridComp GEOSradiationPert_GridComp GEOSgwdPert_GridComp
       GEOSmoistPert_GridComp GEOSturbulencePert_GridComp GEOSsurfacePert_GridComp GEOSchemPert_GridComp
       GEOSpchemPert_GridComp fvdycorepert Tapenade GEOSdynamicsPert_GridComp Chem_Base MAPL
       NCEP_gfsio NCEP_nemsio NCEP_bacio_r4i4 NCEP_bufr_r8i4 NCEP_w3_r8i4 NCEP_sp_r8i4
       NCEP_sfcio MAPL_cfio_r4 GMAO_mpeu nc_diag_read nc_diag_write OpenMP::OpenMP_Fortran)

ecbuild_add_executable (
  TARGET gsidiag_bin2txt.x
  SOURCES gsidiag_bin2txt.f90 gauss_lat_nmc.f
  LIBS GEOSaana_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4)

ecbuild_add_executable (
  TARGET gsidiag_conv_bin2nc4.x
  SOURCES gsidiag_conv_bin2nc4.f90 m_diag_conv.f90
  LIBS GEOSaana_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4)

ecbuild_add_executable (
  TARGET gsidiag_rad_bin2nc4.x
  SOURCES gsidiag_rad_bin2nc4.f90
  LIBS GEOSaana_GridComp NCEP_bufr_r8i4 NCEP_w3_r8i4)

ecbuild_add_executable (
  TARGET getfixsfc.x
  SOURCES getfixsfc.f90 m_getfixsfc.f90
  LIBS NCEP_sfcio NCEP_bacio_r4i4 MAPL_cfio_r4 )

install(PROGRAMS fvssi    DESTINATION bin) 
install(PROGRAMS o3anasa  DESTINATION bin) 

set (DASPERL /usr/bin/perl)
configure_file(fvssisetup fvssisetup @ONLY)
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/fvssisetup DESTINATION bin)

set (etcfiles
   GSI_GridComp.rc.tmpl
   GSI_GridComp_1.rc.tmpl
   GSI_GridComp_2.rc.tmpl
   kxinfo.txt
   kxlist.txt
   prepobs_prep.bufrtable
   )

install (
   FILES ${etcfiles}
   DESTINATION etc
   )

install (
   FILES GSI_GridComp.rc.tmpl
   DESTINATION etc
   RENAME OBS_GridComp.rc.tmpl
   )
