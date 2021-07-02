esma_set_this ()

#set (SRCS
  #GEOSgcmPert.F90 SingularVectors.F90 stubs.F90
#)


#esma_add_library (${this}
  #SRCS ${SRCS}
  #DEPENDENCIES MAPL_Base GEOSagcmPert_GridComp GEOS_PertSvecs
  #INCLUDES ${INC_ESMF})

  ecbuild_add_executable(TARGET GEOSgcmPert.x SOURCES GEOSgcmPert.F90 LIBS MAPL GEOSagcmPert_GridComp GEOS_PertSvecs ${MKL_LIBRARIES})
  ecbuild_add_executable(TARGET SingularVectors.x SOURCES SingularVectors.F90 LIBS MAPL GEOSagcmPert_GridComp GEOS_PertSvecs ${MKL_LIBRARIES} parpack arpack util putil)

file(GLOB rc_files *.rc)
file(GLOB tmpl_files *.tmpl)
install (
   FILES ${rc_files} ${tmpl_files}
   DESTINATION etc
   )
