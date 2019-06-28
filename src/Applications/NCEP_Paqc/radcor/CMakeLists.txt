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
