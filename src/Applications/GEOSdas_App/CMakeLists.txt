set(dasscripts
    FixUnblocked.csh  
    fix_gocart_rc.csh 
    GEOSdas.csm       
    Reblock.csh       
    fvoseledec        
    fvpert            
    fvpsas           
    fvsens            
    fvsvec            
    g54dvar           
    fp_seamless       
    read_HIST.csh )

set(extraperlscripts
    fcst_convert
    fvarchive 
    fvsavecf 
    fvsetup 
    fvconvert
    monthly_means.pl.tmpl)

file(GLOB perlscripts *.pl)
file(GLOB pythonscripts *.py)
file(GLOB pmscripts *.pm)

install (
   PROGRAMS ${dasscripts} ${extraperlscripts} ${perlscripts} ${pmscripts} ${pythonscripts}
   DESTINATION bin )

file(GLOB rc_files *.rc)
file(GLOB nml_files *.nml)

install (
   FILES ${rc_files} ${nml_files}
   DESTINATION etc
   )

install (
   FILES testsuites/runjob.README
   DESTINATION bin
   )

file(GLOB testsuite_files testsuites/*.input)
install (
   FILES ${testsuite_files}
   DESTINATION etc/testsuites
   )

install (
   PROGRAMS testsuites/runjob.pl testsuites/checkinput.pl
   DESTINATION bin
   )
