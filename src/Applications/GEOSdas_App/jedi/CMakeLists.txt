add_subdirectory(etc)

file(GLOB perlscripts *.pl)
file(GLOB pythonscripts *.py)
file(GLOB pmscripts *.pm)
file(GLOB cshscripts *.csh)

install(
   PROGRAMS ${perlscripts} ${pmscripts} ${pythonscripts} ${cshscripts}
   DESTINATION bin)
