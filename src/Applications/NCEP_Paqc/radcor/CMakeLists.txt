esma_set_this()

set (SRCS
  errexit.f
  m_AdvError.f
  m_ArrList.f
  m_AtoX.f
  m_CARDS.f
  m_clouds.f
  m_convert.f
  m_CSpline.f
  m_ellipsoid.f
  m_humidity.f
  m_IGRA.f
  m_PText.f
  m_RadBufr.f
  m_RadData.f
  m_RadDataF.f
  m_RadList.f
  m_RadLists.f
  m_RadNCEP.f
  m_RadSort.f
  m_RadTplate.f
  m_Range.f
  m_RaobCore.f
  m_soundings.f
  m_stdatm.f
  m_SunAlt.f
  m_SysIO.f
  m_SysUtil.f
  m_temperature.f
  m_TextUtil.f
  m_VaiReg.f
  m_VaiUtil.f
  m_VIZReg.f
  )

# This is equivalent to FOPT=$(FOPT2) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   string (REPLACE "${FOPT3}" "${FOPT2}" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${OPTREPORT0}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${FTZ}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${ALIGN_ALL}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
   string (REPLACE "${NO_ALIAS}" "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
endif ()

esma_add_library(${this}
  SRCS ${SRCS}
  DEPENDENCIES NCEP_bufr_r8i8
  )

ecbuild_add_executable (TARGET raobcore.x SOURCES raobcore.f LIBS ${this})
ecbuild_add_executable (TARGET hradcor.x SOURCES hradcor.f LIBS ${this})
ecbuild_add_executable (
  TARGET read_prepbufr.x
  SOURCES read_prepbufr.f
  LIBS ${this})


foreach (target ${this} raobcore.x hradcor.x read_prepbufr.x)
   string(REPLACE " " ";" tmp ${FREAL8})
   foreach (flag ${tmp})
      target_compile_options (${target} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
   endforeach ()

   string(REPLACE " " ";" tmp ${FINT8})
   foreach (flag ${tmp})
      target_compile_options (${target} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${flag}>)
   endforeach ()
endforeach ()

if (EXTENDED_SOURCE)
  set_target_properties (${this} PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
endif()

