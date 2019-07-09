install (
   FILES runjob.README
   DESTINATION bin
   )

file(GLOB testsuite_files *.input)
install (
   FILES ${testsuite_files}
   DESTINATION etc/testsuites
   )

# CVSTAG is harder to get as we may-or-may-not have a git tag (should, but...)
# Using the GitInfo package we can get both variables...
if(GIT_FOUND)
   GIT_WC_INFO(${PROJECT_SOURCE_DIR} Project)
   #message("Current revision is ${Project_WC_REVISION_HASH}")
   #message("Current tag is ${Project_WC_LATEST_TAG}")
   #message("git found: ${GIT_EXECUTABLE}")
endif()

# And then fill a variable with either the tag if found or hash
if (NOT "${Project_WC_LATEST_TAG}" STREQUAL "")
   set (GIT_TAG_OR_REV "${Project_WC_LATEST_TAG}")
else ()
   set (GIT_TAG_OR_REV "${Project_WC_REVISION_HASH}")
endif ()
#message("Current tag or rev is ${GIT_TAG_OR_REV}")

configure_file(CVSTAG.in CVSTAG @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/CVSTAG DESTINATION etc)

set (ESMABIN ${CMAKE_INSTALL_PREFIX}/bin)
set (ESMATST ${CMAKE_INSTALL_PREFIX}/etc/testsuites)
foreach (perl_script checkinput runjob)
   configure_file ( ${perl_script}.pl ${perl_script} @ONLY)
   install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${perl_script} DESTINATION bin)
endforeach ()

