#esma_set_this()
#set (SRCS
    #gettrk_modules.f gettrk_main.f)

#esma_add_library (${this}
  #SRCS ${SRCS}
  #DEPENDENCIES NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4 NCEP_bufr_r4i4 NCEP_bacio_r4i4)

#ecbuild_add_executable(TARGET gettrk.x SOURCES gettrk_main.f gettrk_modules.f LIBS ${this} NCEP_w3_r4i4 NCEP_sp_r4i4 GMAO_gfio_r4 NCEP_bufr_r4i4 NCEP_bacio_r4i4)

#if (EXTENDED_SOURCE)
  #set_target_properties (gettrk.x PROPERTIES COMPILE_FLAGS ${EXTENDED_SOURCE})
#endif()

#install(
   #FILES vtrack vtrack_training
   #DESTINATION bin)