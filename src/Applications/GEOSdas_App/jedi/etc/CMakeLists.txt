set (ALLETC
   JEDIanaConfig.csh
   SWELLConfig.csh
   jedi_acquire_bkg.j
   jedi_acquire_ioda.j
   jedi_acquire_vbc.j
   jedi_run_var.j
   ut_jedi.j
   geos_3dvar.yaml
   geos_3dfgat.yaml
   obsop_name_map.yaml
   )

install (
   FILES ${ALLETC}
   DESTINATION etc/jedi
   )

