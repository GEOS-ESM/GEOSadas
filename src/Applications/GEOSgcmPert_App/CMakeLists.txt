esma_set_this ()

set (SRCS
  GEOSgcmPert.F90 SingularVectors.F90 stubs.F90
)


esma_add_library (${this}
  SRCS ${SRCS}
  DEPENDENCIES MAPL_Base GEOSagcmPert_GridComp GEOS_PertSvecs
  INCLUDES ${INC_ESMF})

