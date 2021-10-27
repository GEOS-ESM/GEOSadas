esma_set_this ()

set (SRCS modules.f BOUND.f fft99.f sig_p_convt1.f SEPAR.f WNLIT.f 
  FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f WNLSM.f WNNLS.f 
  XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f XERSAV.f 
  srotm.f srotmg.f amatrix.f rodist.f m_cutdm.F90 
  m_stmGrid.F90 m_show.F90 
  m_specGridIO.F
  m_geosGridIO.F90
  m_hurrmess.F90
  m_hurr.F90
  m_Hdata.F90
  mv_nvortex.f
  HURR_MESS.F
  HURR_REL.F
  m_vtxgrid.F90
  )

if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FOPT3}" "${FOPT2} ${BIG_ENDIAN}" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FPE0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES MAPL GMAO_mpeu GMAO_hermes GMAO_transf
  INCLUDES ${INC_ESMF})

string(REPLACE " " ";" tmp ${FREAL8})
foreach(flag ${tmp})
  target_compile_options (${this} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
endforeach()

if (EXTENDED_SOURCE)
  set_target_properties (${this} PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()

ecbuild_add_executable (
  TARGET relocate_mv_nvortex.x
  SOURCES main.F90
  LIBS ${this} NCEP_w3_r8i4 NCEP_sp_r8i4)

string(REPLACE " " ";" tmp ${FREAL8})
foreach(flag ${tmp})
  target_compile_options (relocate_mv_nvortex.x PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
endforeach()

install(PROGRAMS vtxreloc DESTINATION bin)
