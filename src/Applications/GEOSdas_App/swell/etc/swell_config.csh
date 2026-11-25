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
if ( ! $?SWELL_COMMIT )        setenv SWELL_COMMIT        881b8c307ed70b3e4312f2bca1b67a6660a377fb
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
# 1: GEOSdas.csm runs the central analysis with swell_driver.csh (3D-FGAT);
# 0: the jedi/ layer does it (jedi_driver.csh, hybrid schemes), converting the
#    GSI ncdiags with swell_gsi2ioda.csh and the swell pinned here
if ( ! $?SWELL_CENTRAL ) setenv SWELL_CENTRAL 1

# Observations: converted from the GSI ncdiags of the cycle (default), or taken from
# an IODA archive <dir>/<yyyymmddThhmmssZ>/geos_atmosphere (e.g. when SKIPGSI)
if ( ! $?SWELL_OFFLINE_IODA_DIR ) setenv SWELL_OFFLINE_IODA_DIR ""

if ( ! $?SWELL_FEEDBACK_VARBC ) setenv SWELL_FEEDBACK_VARBC 1   # cycle JEDI VarBC (else GSI biases)
if ( ! $?SWELL_MKIAU )          setenv SWELL_MKIAU          1   # make the JEDI IAU forcing
if ( ! $?SWELL_IAU_OVERWRITE )  setenv SWELL_IAU_OVERWRITE  1   # GEOS uses it instead of the GSI IAU
if ( ! $?SWELL_MKIAU_MPIRUN )   setenv SWELL_MKIAU_MPIRUN   mpirun

# Cubed-sphere resolution of the JEDI backgrounds as npx (C12 -> 13, C90 -> 91, ...).
# Processor layout and GSIBEC grid follow from it (see swell_ana_central.csh).
#if ( ! $?SWELL_CENTRAL_CRES ) setenv SWELL_CENTRAL_CRES 13

if ( ! $?SWELL_GSIBEC_VERSION )   setenv SWELL_GSIBEC_VERSION   1.0.2   # $SWELL_STATIC_FILES/.../gsibec/<version>
if ( ! $?SWELL_VAR_NITER )        setenv SWELL_VAR_NITER        100     # inner iterations
if ( ! $?SWELL_VAR_BKG_FREQ )     setenv SWELL_VAR_BKG_FREQ     PT3H    # FGAT background frequency
if ( ! $?SWELL_CENTRAL_WALLTIME ) setenv SWELL_CENTRAL_WALLTIME 00:40:00

# Where GEOS keeps <EXPID>/rs/Y%Y/M%m/<EXPID>.bkgcrst.<date>.tar: the archive of
# the experiment ($FVARCH from FVDAS_Run_Config, else $ARCHIVE)
if ( ! $?SWELL_BKG_ARCHIVE ) then
   if ( $?FVARCH ) then
      setenv SWELL_BKG_ARCHIVE $FVARCH
   else if ( $?ARCHIVE ) then
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
# when SWELL_FEEDBACK_VARBC=1 (bootstrap from the GSI biases when empty)
if ( ! $?SWELL_VARBC_DIR ) then
   if ( $?FVHOME ) then
      setenv SWELL_VARBC_DIR $FVHOME/swell/varbc
   endif
endif

# Observations and biases used by the central analysis, per cycle, read by the
# ensemble analysis of the same cycle (latest SWELL_OBS_SHARE_KEEP cycles kept)
if ( ! $?SWELL_OBS_SHARE ) then
   if ( $?FVHOME ) then
      setenv SWELL_OBS_SHARE $FVHOME/swell/obs
   endif
endif
if ( ! $?SWELL_OBS_SHARE_KEEP ) setenv SWELL_OBS_SHARE_KEEP 4

# ---------------------------------------------------------------------------
# Ensemble analysis (swell_ana_atmens.csh, called by atm_ens.j)
# ---------------------------------------------------------------------------
# none: GSI EnKF (or the old JEDI path); eda: eda_atmos; getkf: localensembleda;
# getkf2: localensembleda_2steps
if ( ! $?SWELL_ENSEMBLE ) setenv SWELL_ENSEMBLE none

# Cubed-sphere resolution of the ensemble backgrounds as npx (C12 -> 13, C24 -> 25,
# C90 -> 91, C180 -> 181). Layout and GSIBEC grid follow from it (see swell_ana_atmens.csh).
#if ( ! $?SWELL_ENS_CRES ) setenv SWELL_ENS_CRES 13

if ( ! $?SWELL_ENS_NITER )    setenv SWELL_ENS_NITER    50      # EDA inner iterations
if ( ! $?SWELL_ENS_OBS_PERT ) setenv SWELL_ENS_OBS_PERT 0.5     # EDA obs perturbation amplitude
if ( ! $?SWELL_ENS_WALLTIME ) setenv SWELL_ENS_WALLTIME 00:40:00
# Obs thinning rejection fraction; default 0.8 (eda) / 0.75 (getkf), as in jedi_eda/getkf.yaml
#if ( ! $?SWELL_ENS_THIN ) setenv SWELL_ENS_THIN 0.8
