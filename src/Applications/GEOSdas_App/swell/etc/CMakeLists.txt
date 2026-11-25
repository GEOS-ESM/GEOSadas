set (ALLETC
   swell_config.csh
   convert_ncdiags.yaml.tmpl
   central_3dfgat.yaml.tmpl
   central_slurm.yaml.tmpl
   atmens_eda.yaml.tmpl
   atmens_getkf.yaml.tmpl
   atmens_getkf2.yaml.tmpl
   atmens_slurm.yaml.tmpl
   atmens_diffstates.yaml.tmpl
   mkiau.rc.tenv
   )

install (
   FILES ${ALLETC}
   DESTINATION etc/swell
   )
