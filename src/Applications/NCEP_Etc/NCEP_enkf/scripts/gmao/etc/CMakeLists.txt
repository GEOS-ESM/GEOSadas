set (ALLETC
   AGCM.rc.tmpl
   AtmEnsConfig.csh
   aens_stoch.rc
   ana_aodmean.rc
   ana_aodmember.rc
   atmens_incenergy.rc.tmpl
   atmens_rst_regrid.rc
   atmens_storage.arc
   atmos_enkf.nml.tmpl
   atmos_enkf_sens.nml.tmpl
   CAP.rc.tmpl
   dyn_recenter_l72.rc
   dyn_recenter_l91.rc
   dyn_recenter_l132.rc
   dyn_recenter_l181.rc
   dyn_recenter_l72_sppt.rc
   dyn_recenter_l91_sppt.rc
   dyn_recenter_l132_sppt.rc
   dyn_recenter_l181_sppt.rc
   GAAS_GridComp.rc
   GEOS_ChemGridComp.rc
   GSI_GridComp.rc.tmpl
   GSI_GridComp_ensfinal.rc.tmpl
   HISTAENS.rc.tmpl
   mkiau.rc.tmpl
   mp_stats_perts.rc
   mp_stats.rc
   nmcperts.rc
   post_egcm.rc
   odsstats_ktonly.rc
   obs_aodmean.rc
   obs_aodmember.rc
   obs1gsi_mean.rc
   obs1gsi_member.rc
   )

install (
   FILES ${ALLETC}
   DESTINATION etc/atmens
   )

set (R21C_files
   R21C/atmos_enkf.nml.tmpl  
   R21C/obs1gsi_mean.rc  
   R21C/obs1gsi_member.rc
   )

install (
   FILES ${R21C_files}
   DESTINATION etc/atmens/R21C
   )
