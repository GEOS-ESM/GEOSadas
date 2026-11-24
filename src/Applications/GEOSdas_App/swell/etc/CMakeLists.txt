set (ALLETC
   swell_config.csh
   convert_ncdiags.yaml.tmpl
   )

install (
   FILES ${ALLETC}
   DESTINATION etc/swell
   )
