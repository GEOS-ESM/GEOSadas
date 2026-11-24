add_subdirectory(etc)

file(GLOB cshscripts *.csh)

install(
   PROGRAMS ${cshscripts}
   DESTINATION bin)
