set (ALLETC
   swell_config.csh
   convert_ncdiags.yaml.tmpl
   central_3dfgat.yaml.tmpl
   central_slurm.yaml.tmpl
   )

install (
   FILES ${ALLETC}
   DESTINATION etc/swell
   )
