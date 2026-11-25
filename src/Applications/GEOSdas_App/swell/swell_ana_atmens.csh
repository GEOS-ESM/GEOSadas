#!/bin/csh -f
#
# swell_ana_atmens.csh - ADAS ensemble analysis (EDA or GETKF) with swell
#
# usage: swell_ana_atmens.csh nymdb nhmsb nymda nhmsa
#
#   nymdb nhmsb  beginning of the analysis window
#   nymda nhmsa  analysis time (middle of the window)
#
# Replaces jedi_driver_atmens.csh. Method from SWELL_ENSEMBLE:
#   eda     swell eda_atmos               member increments <expid>.inc_iter1.memNNN.<date>.nc4
#   getkf   swell localensembleda         member increments localensembleda.inc.eta.<date>.nc4
#   getkf2  swell localensembleda_2steps  member increments localensembleda_2steps.inc.eta.<date>.nc4
#
# Inputs:
#   $SWELL_OBS_SHARE/<ana>/geos_atmosphere     observations + biases of the central analysis
#                                              (written by swell_ana_central.csh)
#   $ATMENSLOC/ensbkgx/memNNN/*.bkg_clcv.*     cubed member backgrounds (read in place)
#   $ATMENSLOC/memNNN/$EXPID.bkg.eta.*         lat-lon member backgrounds
# Outputs (what post_eana.csh / atmos_ens2gcm.csh expect):
#   $FVWORK/updated_ens/memNNN/$EXPID.ana.eta.<yyyymmdd_hhmm>z.nc4   (dyn_jediupd.x: bkg + inc)
#   $FVWORK/updated_ens/{ensmean,ensrms}/                            (filled by post_eana.csh)
#   $FVWORK/updated_ens/$EXPID.atmens_olog.<yyyymmdd_hh>z.tar.gz     (swell/JEDI logs)
#
# Resolution: SWELL_ENS_CRES (npx: 13, 25, 91, 181) selects the processor layout and
# GSIBEC grid; any SWELL_ENS_{NPX,NPY,PERHOST,GSIBEC_NLATS,GSIBEC_NLONS} already set wins.
# ---------------------------------------------------------------------------

set myname = swell_ana_atmens.csh

if ( $#argv < 4 ) then
   echo "usage: $myname nymdb nhmsb nymda nhmsa"
   exit 1
endif

set nymdb = $1
set nhmsb = `echo $2 | awk '{printf "%06d", $1}'`
set nymda = $3
set nhmsa = `echo $4 | awk '{printf "%06d", $1}'`
set hhb   = `echo $nhmsb | cut -c1-2`
set hha   = `echo $nhmsa | cut -c1-2`
set hhmma = `echo $nhmsa | cut -c1-4`
set yyyymmddhh = ${nymda}${hha}

if ( ! $?EXPID || ! $?FVHOME || ! $?FVWORK ) then
   echo "${myname}: EXPID, FVHOME and FVWORK must be defined, aborting"
   exit 1
endif
if ( ! $?ATMENSLOC ) setenv ATMENSLOC $FVHOME/atmens

set donefile = $FVWORK/.DONE_${myname}.$yyyymmddhh
if ( -e $donefile ) then
   echo "${myname}: already done"
   exit 0
endif

set mydir = `dirname $0`
set mydir = `cd $mydir && pwd`

source $mydir/swell_env.csh
if ( ! $SWELL_ENV_OK ) then
   echo "${myname}: swell environment setup failed, aborting"
   exit 1
endif
foreach v ( SWELL_ENS_CRES SWELL_OBS_SHARE SWELL_MKSI_PATH SWELL_SLURM_ACCOUNT )
   printenv $v >& /dev/null
   if ( $status ) then
      echo "${myname}: $v is not set (see swell_config.csh), aborting"
      exit 1
   endif
end

# --- method ----------------------------------------------------------------
switch ( $SWELL_ENSEMBLE )
   case eda:
      set suite = eda_atmos
      set tmpl  = atmens_eda.yaml.tmpl
      set thin  = 0.8
      breaksw
   case getkf:
      set suite = localensembleda
      set tmpl  = atmens_getkf.yaml.tmpl
      set thin  = 0.75
      breaksw
   case getkf2:
      set suite = localensembleda_2steps
      set tmpl  = atmens_getkf2.yaml.tmpl
      set thin  = 0.75
      breaksw
   default:
      echo "${myname}: SWELL_ENSEMBLE=$SWELL_ENSEMBLE is not one of eda, getkf, getkf2, aborting"
      exit 1
endsw
if ( ! $?SWELL_ENS_THIN ) setenv SWELL_ENS_THIN $thin

# --- layout from resolution (as in setup_aanajedi.pl set_ens_res) -----------
switch ( $SWELL_ENS_CRES )
   case 13:
      set lay = ( 2 2 24 46 72 )
      breaksw
   case 25:
      set lay = ( 4 4 96 46 72 )
      breaksw
   case 91:
      set lay = ( 4 5 120 91 144 )
      breaksw
   case 181:
      set lay = ( 6 6 108 181 288 )
      breaksw
   default:
      echo "${myname}: no layout for SWELL_ENS_CRES=$SWELL_ENS_CRES, aborting"
      exit 1
endsw
if ( ! $?SWELL_ENS_NPX )          setenv SWELL_ENS_NPX          $lay[1]
if ( ! $?SWELL_ENS_NPY )          setenv SWELL_ENS_NPY          $lay[2]
if ( ! $?SWELL_ENS_PERHOST )      setenv SWELL_ENS_PERHOST      $lay[3]
if ( ! $?SWELL_ENS_GSIBEC_NLATS ) setenv SWELL_ENS_GSIBEC_NLATS $lay[4]
if ( ! $?SWELL_ENS_GSIBEC_NLONS ) setenv SWELL_ENS_GSIBEC_NLONS $lay[5]
@ ntasks = 6 * $SWELL_ENS_NPX * $SWELL_ENS_NPY
@ nodes  = ( $ntasks + $SWELL_ENS_PERHOST - 1 ) / $SWELL_ENS_PERHOST
setenv SWELL_ENS_NODES $nodes

# --- members ---------------------------------------------------------------
set members = ( `find -H $ATMENSLOC -maxdepth 1 -name "mem[0-9][0-9][0-9]" | sort | xargs -n1 basename` )
if ( $#members == 0 ) then
   echo "${myname}: no member directories in $ATMENSLOC, aborting"
   exit 1
endif
setenv SWELL_ENS_NMEM $#members
echo "${myname}: $SWELL_ENSEMBLE ($suite), $#members members, C$SWELL_ENS_CRES layout ${SWELL_ENS_NPX}x${SWELL_ENS_NPY}x6 = $ntasks tasks on $nodes node(s)"

foreach m ( $members )
   if ( ! -e $ATMENSLOC/$m/$EXPID.bkg.eta.${nymda}_${hhmma}z.nc4 ) then
      echo "${myname}: missing $ATMENSLOC/$m/$EXPID.bkg.eta.${nymda}_${hhmma}z.nc4, aborting"
      exit 1
   endif
   if ( ! -e $ATMENSLOC/ensbkgx/$m/$EXPID.bkg_clcv.${nymda}_${hhmma}z.nc4 ) then
      echo "${myname}: missing $ATMENSLOC/ensbkgx/$m/$EXPID.bkg_clcv.${nymda}_${hhmma}z.nc4, aborting"
      exit 1
   endif
end

# --- observations: those of the central analysis of this cycle -------------
set obsdir = $SWELL_OBS_SHARE/${nymda}T${nhmsa}Z/geos_atmosphere
set nobs = `find -H $obsdir -maxdepth 1 -type f |& grep -c "^$obsdir/"`
if ( $nobs == 0 ) then
   echo "${myname}: no observations in $obsdir; the central analysis must run"
   echo "${myname}: through swell (swell_ana_central.csh) before the ensemble, aborting"
   exit 1
endif

# --- backgrounds: present $ATMENSLOC under the archive layout swell expects --
setenv JEDIWORK $FVWORK/jedi.${nymda}.${nhmsa}
mkdir -p $JEDIWORK
setenv SWELL_ENS_ROOT $JEDIWORK/swell_ens
set yyyyb = `echo $nymdb | cut -c1-4`
set mmb   = `echo $nymdb | cut -c5-6`
set ensdir = $SWELL_ENS_ROOT/$EXPID/atmens/Y$yyyyb/M$mmb
/bin/rm -rf $SWELL_ENS_ROOT
mkdir -p $ensdir
ln -s $ATMENSLOC $ensdir/$EXPID.atmens_ebkgx.${nymdb}_${hhb}z

# --- run swell -------------------------------------------------------------
set expid = ${EXPID}-ens-$yyyymmddhh
setenv SWELL_SLURM_TEMPLATE atmens_slurm.yaml.tmpl
$mydir/swell_run.csh $suite $tmpl $JEDIWORK $expid $nymda $nhmsa
if ( $status ) then
   echo "${myname}: swell $suite failed, aborting"
   exit 1
endif
set cycdir = $JEDIWORK/$expid/run/${nymda}T${nhmsa}Z/geos_atmosphere

# --- member analyses: lat-lon background + JEDI increment ------------------
mkdir -p $FVWORK/updated_ens/ensmean $FVWORK/updated_ens/ensrms
foreach m ( $members )
   if ( $SWELL_ENSEMBLE == eda ) then
      set incs = ( `find -H $cycdir/analysis/$m -maxdepth 1 -name "${expid}.inc_iter1.${m}.*.nc4"` )
   else
      set incs = ( `find -H $cycdir/analysis/$m -maxdepth 1 -name "${suite}.inc.eta.*.nc4"` )
   endif
   if ( $#incs != 1 ) then
      echo "${myname}: expected one increment for $m in $cycdir/analysis/$m, found $#incs, aborting"
      exit 1
   endif
   set inc = $incs[1]
   set itag = `basename $inc .nc4 | awk -F. '{print $NF}'`
   if ( "$itag" != "${nymda}_${nhmsa}z" ) then
      echo "${myname}: WARNING: $m increment valid at $itag, used at ${nymda}_${nhmsa}z"
   endif

   set bkg = $ATMENSLOC/$m/$EXPID.bkg.eta.${nymda}_${hhmma}z.nc4
   set outdir = $FVWORK/updated_ens/$m
   set ana = $outdir/$EXPID.ana.eta.${nymda}_${hhmma}z.nc4
   mkdir -p $outdir
   /bin/rm -f $ana
   dyn_jediupd.x $nymda $nhmsa $bkg $inc $ana >& $cycdir/analysis/$m/dyn_jediupd.log
   if ( $status || ! -e $ana ) then
      cat $cycdir/analysis/$m/dyn_jediupd.log
      echo "${myname}: dyn_jediupd.x failed for $m, aborting"
      exit 1
   endif
end
echo "${myname}: $#members member analyses in $FVWORK/updated_ens/mem*"

# --- logs ------------------------------------------------------------------
cd $JEDIWORK
set logs = ( `find -H $expid -name "*log*" -type f` )
if ( $#logs > 0 ) then
   tar czf $FVWORK/updated_ens/$EXPID.atmens_olog.${nymda}_${hha}z.tar.gz $logs
endif

touch $donefile
echo "${myname}: Complete"
exit 0
