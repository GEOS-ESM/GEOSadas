#!/bin/csh
#
# swell_config.csh - the ONE place that pins swell and JEDI for GEOS-ADAS.
#
# Read by swell_env.csh. Search order for this file:
#    $SWELL_CONFIG  (if set)
#    $FVHOME/run/swell/swell_config.csh   (experiment copy - edit this one)
#    $FVROOT/etc/swell/swell_config.csh   (installed default)
#
# Every setting can be pre-set in the environment; values here are defaults.
# ---------------------------------------------------------------------------

# swell: python venv (with swell installed in editable mode) and its source tree
if ( ! $?SWELL_VENV )   setenv SWELL_VENV   /discover/nobackup/yyu11/venv_all/venv_ss1.9_py3.11.7_PR656_uv
if ( ! $?SWELL_SRC )    setenv SWELL_SRC    /discover/nobackup/yyu11/prep_swell/install_swell/swell_etkf_adas

# Expected swell commit (full or abbreviated hash). Empty string disables the check.
# SWELL_COMMIT_STRICT=1 aborts on mismatch; 0 only warns.
if ( ! $?SWELL_COMMIT )        setenv SWELL_COMMIT        099bd02dd9f2d1fc9079e2556300c671ce028eda
if ( ! $?SWELL_COMMIT_STRICT ) setenv SWELL_COMMIT_STRICT 1

# JEDI build and source used by swell (jedi_build_method: use_existing)
if ( ! $?SWELL_JEDI_BUILD )  setenv SWELL_JEDI_BUILD  /discover/nobackup/yyu11/generated/br_2026-10-27/build
if ( ! $?SWELL_JEDI_SRC )    setenv SWELL_JEDI_SRC    /discover/nobackup/yyu11/generated/br_2026-10-27/jedi-bundle

# Static files and CRTM coefficients
if ( ! $?SWELL_STATIC_FILES ) setenv SWELL_STATIC_FILES /discover/nobackup/projects/gmao/advda/SwellStaticFiles
if ( ! $?SWELL_CRTM_COEFFS ) then
   if ( $?FVHOME ) then
      setenv SWELL_CRTM_COEFFS $FVHOME/fvInput/gsi/etc/JEDI-CRTM-2.4.1j1-GMAO-1/Little_Endian/
   endif
endif

# swell platform and workflow behavior
if ( ! $?SWELL_PLATFORM )      setenv SWELL_PLATFORM      nccs_discover_sles15
if ( ! $?SWELL_TIMEOUT )       setenv SWELL_TIMEOUT       PT5M   # cylc stall timeout after a task failure
if ( ! $?SWELL_KEEP_CYLC_RUN ) setenv SWELL_KEEP_CYLC_RUN 0      # 1: keep ~/cylc-run/<id>-suite after success
