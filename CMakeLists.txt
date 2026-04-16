cmake_minimum_required (VERSION 3.13)
cmake_policy (SET CMP0053 NEW)
cmake_policy (SET CMP0054 NEW)

project (
  GEOSadas
  VERSION 5.27.1
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
   message(SEND_ERROR "In-source builds are disabled. Please
           issue cmake command in separate build directory.")
endif ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")

# Set the default build type to release
if (NOT CMAKE_BUILD_TYPE)
  message (STATUS "Setting build type to 'Release' as none was specified.")
  set (CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif ()

set (DOING_GEOS5 YES)

# Should find a better place for this - used in Chem component
set (ACG_FLAGS -v)
set (F2PYEXT .so)
set (F2PY_SUFFIX .so)
set (FV_PRECISION R4R8)

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/@cmake")
include (esma)

# =====================================================
# GNU / gfortran specific settings for legacy + modern Fortran
# (Intel build is left completely untouched)
# =====================================================
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  message(STATUS "=== GNU Fortran (gfortran/mpifort) detected ===")
  message(STATUS "    Applying legacy compatibility flags (safe for mixed F77/F90)")

  # These two flags are the ones you were using:
  #   -fallow-argument-mismatch  → turns argument-mismatch errors into warnings
  #   -ffixed-line-length-none   → handles old fixed-form F77; harmless for free-form F90
  # They are applied ONLY to Fortran files (C/C++ are unaffected).
  #add_compile_options(
  #  $<$<COMPILE_LANGUAGE:Fortran>:-fallow-argument-mismatch>
  #  $<$<COMPILE_LANGUAGE:Fortran>:-ffree-line-length-none>
  #  $<$<COMPILE_LANGUAGE:Fortran>:-fd-lines-as-comments>
  #)

  # BIG_ENDIAN flag that your esma module expects on gfortran
  set(BIG_ENDIAN "-fconvert=big-endian" CACHE STRING "Endian conversion flag for gfortran")

  # If you ever need -fdec or -std=legacy for a few very old files,
  # add them ONLY in those specific subdir CMakeLists.txt using target_compile_options.
  # Do NOT add them here globally.

  # Optional: pull LDFLAGS from the environment (set by g5_modules.sh or your script)
  if(DEFINED ENV{LDFLAGS})
    string(APPEND CMAKE_EXE_LINKER_FLAGS " $ENV{LDFLAGS}")
    message(STATUS "   Appended LDFLAGS from environment: $ENV{LDFLAGS}")
  endif()
else()
    message(STATUS "Intel Fortran detected - no extra GNU flags applied")
endif()

ecbuild_declare_project()

# Generic DFLAGS
# These should be relocated and/or eliminated.
add_definitions(-Dsys${CMAKE_SYSTEM_NAME} -DESMA64)
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})

# Recursively build source tree
add_subdirectory (@env)
add_subdirectory (src)

# https://www.scivision.dev/cmake-auto-gitignore-build-dir/
# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

# Piggyback that file into install
install(
   FILES ${PROJECT_BINARY_DIR}/.gitignore
   DESTINATION ${CMAKE_INSTALL_PREFIX}
   )
