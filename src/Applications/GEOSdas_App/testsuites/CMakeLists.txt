install (
   FILES runjob.README
   DESTINATION bin
   )

file(GLOB testsuite_files *.input)
install (
   FILES ${testsuite_files}
   DESTINATION etc/testsuites
   )

set (ESMABIN ${CMAKE_INSTALL_PREFIX}/bin)
set (ESMATST ${CMAKE_INSTALL_PREFIX}/etc/testsuites)
foreach (perl_script checkinput runjob)
   configure_file ( ${perl_script}.pl ${perl_script} @ONLY)
   install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${perl_script} DESTINATION bin)
endforeach ()

