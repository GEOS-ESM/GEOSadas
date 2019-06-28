file(GLOB perlscripts *.pl)
file(GLOB pmscripts *.pm)
file(GLOB cshscripts *.csh)

install(
   FILES ${perlscripts} ${pmscripts} ${cshscripts} atm_ens.j
   DESTINATION bin)
