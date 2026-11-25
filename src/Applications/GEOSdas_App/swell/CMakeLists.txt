add_subdirectory(etc)

file(GLOB cshscripts *.csh)

install(
   PROGRAMS ${cshscripts} swell_setup.pl
   DESTINATION bin)
