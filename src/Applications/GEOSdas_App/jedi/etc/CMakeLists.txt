set (ALLETC
   JEDIanaConfig.csh
   SWELLConfig.csh
   geos_3dvar.yaml
   geos_3dfgat.yaml
   jedi_acquire_bkg.j
   jedi_acquire_ioda.j
   jedi_acquire_vbc.j
   jedi_run_var.j
   mkiau.rc.tenv
   obsop_name_map.yaml
   ut_jedi.j
   )

install (
   FILES ${ALLETC}
   DESTINATION etc/jedi
   )

