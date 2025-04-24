set (ALLETC
   JEDIanaConfig.csh
   SWELLConfig.csh
   jedi_acquire_bkg.j
   jedi_acquire_ioda.j
   jedi_acquire_vbc.j
   _jedi_run_var.j
   ut_jedi.j
   geos_3dvar_c181.yaml
   geos_3dfgat_c181.yaml
   )

install (
   FILES ${ALLETC}
   DESTINATION etc/jedi
   )

