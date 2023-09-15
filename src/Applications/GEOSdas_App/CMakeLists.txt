add_subdirectory(testsuites)

set(dasscripts
    FixUnblocked.csh  
    fix_gocart_rc.csh 
    GEOSdas.csm       
    Reblock.csh       
    fvgaas            
    fvoseledec        
    fvpert            
    fvpsas           
    fvsens            
    fvsvec            
    g54dvar           
    fp_seamless 
    ldas_run.csh       
    mom6diag.csh       
    read_HIST.csh )

set(extraperlscripts
    fvarchive 
    fvsetup 
    fvconvert
    )

set(misc_etc
   monthly.yyyymm.pl.tmpl
   monthly_tarandclean.j.tmpl
   monthly_prefetch.j.tmpl
   monthly_means.j.tmpl
   monthly_plots.j.tmpl
   ncep_ana.tbl
   ncep_diag.tbl
   ncep_prog.tbl
   noreplay.acq
   plots_transfer.csh.tmpl
   )


file(GLOB perlscripts *.pl)
file(GLOB pythonscripts *.py)
file(GLOB pmscripts *.pm)

install (
   PROGRAMS ${dasscripts} ${extraperlscripts} ${perlscripts} ${pmscripts} ${pythonscripts}
   DESTINATION bin )

file(GLOB rc_files *.rc)
file(GLOB nml_files *.nml)

install (
   FILES ${rc_files} ${nml_files} ${misc_etc}
   DESTINATION etc
   )

set (perlscripts_with_dasperl
   fcst_convert
   fvsavecf
   )

set (DASPERL /usr/bin/perl)
foreach (script ${perlscripts_with_dasperl})
   configure_file(${script} ${script} @ONLY)
   install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${script} DESTINATION bin)
endforeach ()

# MAT The DAS seems to require VERSION and CVSTAG

# The version is easy as we set that in the project
configure_file(VERSION.in VERSION @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/VERSION DESTINATION etc)

# CVSTAG is set and used in testsuites/

set(RELEASEFILES 
   /etc/SuSE-release
   /etc/os-release
   )

foreach (file ${RELEASEFILES})
   if (EXISTS ${file})
      install(FILES ${file} DESTINATION etc)
   endif ()
endforeach ()
