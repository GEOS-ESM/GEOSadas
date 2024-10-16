# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
#  set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${EXTENDED_SOURCE} ${BIG_ENDIAN} ${BYTERECLEN} ${FP_MODEL_STRICT} ${ALIGNCOM}")
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${EXTENDED_SOURCE}                             ${FP_MODEL_STRICT} ${ALIGNCOM}")
endif ()

ecbuild_add_executable (
  TARGET saber2ods.x
  SOURCES saber2ods.f90 m_saber.f m_pbutil.f m_saber_err.f90
  LIBS GMAO_ods GMAO_hermes GMAO_mpeu NCEP_bufr_r8i4 NCEP_w3_r8i4  NetCDF::NetCDF_Fortran)
# LIBS GMAO_ods GMAO_mpeu NetCDF::NetCDF_Fortran )

ecbuild_add_executable (
  TARGET saber_obserr.x 
  SOURCES saber_obserr.f m_saber_err.f90
  LIBS GMAO_hermes NCEP_bufr_r8i4 NCEP_w3_r8i4  NetCDF::NetCDF_Fortran)

