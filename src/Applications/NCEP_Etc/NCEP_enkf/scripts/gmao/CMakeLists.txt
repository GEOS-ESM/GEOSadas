add_subdirectory(etc)

file(GLOB perlscripts *.pl)
file(GLOB pmscripts *.pm)
file(GLOB cshscripts *.csh)

install(
   PROGRAMS ${perlscripts} ${pmscripts} ${cshscripts} atm_ens.j
   DESTINATION bin)
