#!/bin/csh -f
#
# swell_run.csh - run ONE cycle of a swell suite and wait for it to finish
#
# usage: swell_run.csh suite template exproot expid nymd nhms
#
#   suite     swell suite/config name (e.g. convert_ncdiags, eda_atmos)
#   template  override template; a bare name is searched for in
#             $FVHOME/run/swell, $FVROOT/etc/swell, <this script dir>/etc
#   exproot   swell experiment_root (directory owned by this run)
#   expid     swell experiment_id (unique per role and cycle, e.g. x0055-g2i-2026082612)
#   nymd nhms analysis date/time (middle of window)
#
# Steps:
#   1. swell_env.csh (pin file, venv, commit check)
#   2. render template with vED -env; abort on any unresolved $VAR
#   3. swell create <suite> -o <override> -p $SWELL_PLATFORM -k [-s slurm.yaml]
#   4. swell launch <suite dir> -b -t $SWELL_TIMEOUT    (blocks until done)
#   5. require the swell CleanCycle marker  <cycle dir>/cycle_done_*
#   6. save cylc logs into <exproot>/<expid>/cylc_log.tar.gz; remove the
#      ~/cylc-run entry on success (kept on failure for 'cylc cat-log')
#
# Variables available to templates (in addition to the whole environment):
#   SWELL_EXPERIMENT_ID  SWELL_EXPERIMENT_ROOT  SWELL_ISO_DATE  SWELL_CYCLE_TIME
#   SWELL_JEDI_BUILD  SWELL_JEDI_SRC  SWELL_STATIC_FILES  SWELL_CRTM_COEFFS
#
# Optional environment:
#   SWELL_SLURM_FILE  slurm yaml passed with -s (default $FVHOME/run/swell/slurm.yaml if present)
#   SWELL_MODEL       model component holding the done marker (default geos_atmosphere)
#   SWELL_DONE_MARKER none: do not require cycle_done_* (for suites without CleanCycle)
#
# Exit status: 0 on success, 1 on failure.
# ---------------------------------------------------------------------------

set myname = swell_run.csh

if ( $#argv < 6 ) then
   echo "usage: $myname suite template exproot expid nymd nhms"
   exit 1
endif

set suite    = $1
set template = $2
set exproot  = $3
set expid    = $4
set nymd     = $5
set nhms     = `echo $6 | awk '{printf "%06d", $1}'`

set mydir = `dirname $0`
set mydir = `cd $mydir && pwd`

# installed layout ($FVROOT/bin + $FVROOT/etc/swell) or source tree (swell/ + swell/etc)
set etcdir = $mydir/etc
if ( -d $mydir/../etc/swell ) set etcdir = `cd $mydir/../etc/swell && pwd`
setenv SWELL_ETC_FALLBACK $etcdir

# --- environment -----------------------------------------------------------
source $mydir/swell_env.csh
if ( ! $SWELL_ENV_OK ) then
   echo "${myname}: swell environment setup failed, aborting"
   exit 1
endif

which vED >& /dev/null
if ( $status ) then
   echo "${myname}: vED not found in PATH (needed to render $template), aborting"
   exit 1
endif

# --- dates -----------------------------------------------------------------
set yyyy = `echo $nymd | cut -c1-4`
set mm   = `echo $nymd | cut -c5-6`
set dd   = `echo $nymd | cut -c7-8`
set hh   = `echo $nhms | cut -c1-2`
set mn   = `echo $nhms | cut -c3-4`
set ss   = `echo $nhms | cut -c5-6`

setenv SWELL_EXPERIMENT_ID   $expid
setenv SWELL_EXPERIMENT_ROOT $exproot
setenv SWELL_ISO_DATE        "${yyyy}-${mm}-${dd}T${hh}:${mn}:${ss}Z"
setenv SWELL_CYCLE_TIME      T${hh}

if ( ! $?SWELL_MODEL ) setenv SWELL_MODEL geos_atmosphere
set expdir   = $exproot/$expid
set suitedir = $expdir/${expid}-suite
set cycdir   = $expdir/run/${nymd}T${nhms}Z/$SWELL_MODEL
set cylcrun  = $HOME/cylc-run/${expid}-suite

# --- locate template -------------------------------------------------------
set tmpl = ""
if ( -e $template ) then
   set tmpl = $template
else
   set search = ( $etcdir )
   if ( $?FVROOT ) then
      set search = ( $FVROOT/etc/swell $search )
   endif
   if ( $?FVHOME ) then
      set search = ( $FVHOME/run/swell $search )
   endif
   foreach d ( $search )
      if ( "$tmpl" == "" && -e $d/$template ) set tmpl = $d/$template
   end
endif
if ( "$tmpl" == "" ) then
   echo "${myname}: cannot find override template $template, aborting"
   exit 1
endif

# --- fresh experiment directory --------------------------------------------
if ( "$exproot" == "" || "$expid" == "" ) then
   echo "${myname}: empty exproot or expid, aborting"
   exit 1
endif
mkdir -p $exproot
if ( -d $expdir ) /bin/rm -rf $expdir
if ( -d $cylcrun ) /bin/rm -rf $cylcrun

# --- render override -------------------------------------------------------
set override = $exproot/${expid}.override.yaml
vED -env $tmpl -o $override
if ( $status || ! -e $override ) then
   echo "${myname}: failed to render $tmpl, aborting"
   exit 1
endif
# (\x24 is '$'; a literal dollar cannot be used inside csh backquotes)
set nunres = `grep -v '^[[:space:]]*#' $override | grep -cP '\x24\{?[A-Za-z_]'`
if ( $nunres > 0 ) then
   echo "${myname}: unresolved variables in $override (from $tmpl):"
   grep -v '^[[:space:]]*#' $override | grep -nP '\x24\{?[A-Za-z_]'
   exit 1
endif

echo "${myname}: suite=$suite expid=$expid cycle=$SWELL_ISO_DATE"
echo "${myname}: template=$tmpl"
echo "${myname}: override=$override"

# --- create ----------------------------------------------------------------
set slurmopt = ""
if ( ! $?SWELL_SLURM_FILE ) then
   if ( $?FVHOME ) then
      if ( -e $FVHOME/run/swell/slurm.yaml ) setenv SWELL_SLURM_FILE $FVHOME/run/swell/slurm.yaml
   endif
endif
if ( $?SWELL_SLURM_FILE ) then
   set slurmopt = "-s $SWELL_SLURM_FILE"
endif

swell create $suite -o $override -p $SWELL_PLATFORM -k $slurmopt
set rc = $status
if ( $rc || ! -e $suitedir/flow.cylc ) then
   echo "${myname}: swell create $suite failed (status $rc), aborting"
   exit 1
endif

# provenance
cat >! $expdir/swell_run.info << EOF
suite:        $suite
experiment:   $expid
cycle:        $SWELL_ISO_DATE
override:     $override
template:     $tmpl
swell_venv:   $SWELL_VENV
swell_src:    $SWELL_SRC
swell_commit: $SWELL_HEAD
jedi_build:   $SWELL_JEDI_BUILD
host:         `hostname`
created:      `date -u +%Y-%m-%dT%H:%M:%SZ`
EOF

# --- launch and wait -------------------------------------------------------
swell launch $suitedir -b -t $SWELL_TIMEOUT
set rc = $status

# keep the cylc logs with the experiment
if ( -d $cylcrun/runN/log ) then
   tar -C $cylcrun/runN -czf $expdir/cylc_log.tar.gz log
endif

if ( $rc ) then
   echo "${myname}: swell launch failed (status $rc); cylc run kept in $cylcrun"
   exit 1
endif

# --- success checks --------------------------------------------------------
if ( ! $?SWELL_DONE_MARKER ) setenv SWELL_DONE_MARKER cycle_done
if ( "$SWELL_DONE_MARKER" != "none" ) then
   set marker = `find $cycdir -maxdepth 1 -name "${SWELL_DONE_MARKER}_*" |& head -1`
   if ( "$marker" == "" ) then
      echo "${myname}: workflow ended but $cycdir/${SWELL_DONE_MARKER}_* not found, aborting"
      exit 1
   endif
endif

if ( ! $SWELL_KEEP_CYLC_RUN ) /bin/rm -rf $cylcrun

echo "${myname}: $suite $expid completed"
exit 0
