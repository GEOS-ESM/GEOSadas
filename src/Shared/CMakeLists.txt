esma_add_subdirectories (
#  JCSDA_crtm
  @MAPL
  @GMAO_Shared
  @NCEP_Shared
#  @AeroML/src
  )

## Special case - FMS is built twice with two
## different precisions.
#add_subdirectory (@FMS fms_r4)
#add_subdirectory (@FMS fms_r8)
#
