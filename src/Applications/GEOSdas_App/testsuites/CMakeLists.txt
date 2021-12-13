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
   if (NOT "${Project_WC_LATEST_TAG}" STREQUAL "")
      execute_process(
         COMMAND ${GIT_EXECUTABLE} merge-base --is-ancestor ${Project_WC_LATEST_TAG} ${Project_WC_REVISION_HASH}
         RESULT_VARIABLE is_latest_tag_an_ancestor
         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
         )
      if (is_latest_tag_an_ancestor EQUAL 0)
         #message("Tag ${Project_WC_LATEST_TAG} is an ancestor of ${Project_WC_REVISION_HASH}")
         #message("Setting GIT_TAG_OR_REV to ${Project_WC_REVISION_HASH}")
         set (GIT_TAG_OR_REV ${Project_WC_REVISION_HASH})
      elseif (is_latest_tag_an_ancestor EQUAL 1)
         #message("Tag ${Project_WC_REVISION_HASH} is an ancestor of ${Project_WC_LATEST_TAG}")
         #message("Setting GIT_TAG_OR_REV to ${Project_WC_LATEST_TAG}")
         set (GIT_TAG_OR_REV ${Project_WC_LATEST_TAG})
      else ()
         message(FATAL_ERROR "This should not be reached")
      endif ()
   else ()
      set (GIT_TAG_OR_REV GEOSadas-${CMAKE_PROJECT_VERSION})
   endif ()

   message("Setting fvID to fvsetup SHA1 (1st 10 digits)")
   execute_process(
      COMMAND ${GIT_EXECUTABLE} hash-object src/Applications/GEOSdas_App/fvsetup
      COMMAND cut -c1-10
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      OUTPUT_VARIABLE fvID
      OUTPUT_STRIP_TRAILING_WHITESPACE
   )
   message("fvID=${fvID}")
endif()

configure_file(CVSTAG.in CVSTAG @ONLY)
install (
   FILES ${CMAKE_CURRENT_BINARY_DIR}/CVSTAG
   DESTINATION etc
)

set (ESMABIN ${CMAKE_INSTALL_PREFIX}/bin)
set (ESMATST ${CMAKE_INSTALL_PREFIX}/etc/testsuites)
foreach (perl_script checkinput runjob)
   configure_file ( ${perl_script}.pl ${perl_script} @ONLY)
   install (
      PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${perl_script}
      DESTINATION bin
   )
endforeach ()
