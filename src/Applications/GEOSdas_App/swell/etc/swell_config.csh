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
if ( ! $?SWELL_COMMIT )        setenv SWELL_COMMIT        44fe8d1f9ccd89d204c31089708e17151a291b78
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

# slurm settings for swell tasks that run as batch jobs
if ( ! $?SWELL_SLURM_ACCOUNT ) then
   if ( $?GID ) then
      setenv SWELL_SLURM_ACCOUNT $GID
   endif
endif
if ( ! $?SWELL_SLURM_QOS )        setenv SWELL_SLURM_QOS        allnccs
if ( ! $?SWELL_SLURM_CONSTRAINT ) setenv SWELL_SLURM_CONSTRAINT mil

# ---------------------------------------------------------------------------
# Central analysis (swell_ana_central.csh, suite 3dfgat_atmos)
# ---------------------------------------------------------------------------
# 1: run the central 3D-FGAT through swell (used when JEDI_HYBRID=0); 0: old jedi_set/jedi_run path
if ( ! $?SWELL_CENTRAL ) setenv SWELL_CENTRAL 1

# Cubed-sphere resolution of the JEDI backgrounds as npx (C12 -> 13, C90 -> 91, ...).
# Processor layout and GSIBEC grid follow from it (see swell_ana_central.csh).
#if ( ! $?SWELL_CENTRAL_CRES ) setenv SWELL_CENTRAL_CRES 13

if ( ! $?SWELL_GSIBEC_VERSION )   setenv SWELL_GSIBEC_VERSION   1.0.2   # $SWELL_STATIC_FILES/.../gsibec/<version>
if ( ! $?SWELL_VAR_NITER )        setenv SWELL_VAR_NITER        100     # inner iterations
if ( ! $?SWELL_VAR_BKG_FREQ )     setenv SWELL_VAR_BKG_FREQ     PT3H    # FGAT background frequency
if ( ! $?SWELL_CENTRAL_WALLTIME ) setenv SWELL_CENTRAL_WALLTIME 00:40:00

# Where GEOS keeps <EXPID>/rs/Y%Y/M%m/<EXPID>.bkgcrst.<date>.tar (normally $ARCHIVE)
if ( ! $?SWELL_BKG_ARCHIVE ) then
   if ( $?ARCHIVE ) then
      setenv SWELL_BKG_ARCHIVE $ARCHIVE
   endif
endif

# GMAO observing system database (gmao_satinfo.db, gmao_ozinfo.db)
if ( ! $?SWELL_MKSI_PATH ) then
   if ( $?FVROOT ) then
      setenv SWELL_MKSI_PATH $FVROOT/etc/
   endif
endif

# Persistent copy of the latest JEDI VarBC output, used by the next cycle
# when JEDI_FEEDBACK_VARBC=1 (bootstrap from the GSI biases when empty)
if ( ! $?SWELL_VARBC_DIR ) then
   if ( $?FVHOME ) then
      setenv SWELL_VARBC_DIR $FVHOME/swell/varbc
   endif
endif
