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
   FILES ${dasscripts} ${extraperlscripts} ${perlscripts} ${pmscripts} ${pythonscripts}
   DESTINATION bin )
