#!/bin/csh -f
#
# swell_driver.csh - central JEDI analysis of one ADAS cycle through swell
#
# usage: swell_driver.csh nymdb nhmsb nymda nhmsa
#
#   nymdb nhmsb  beginning of the analysis window
#   nymda nhmsa  analysis time
#
# Called by GEOSdas.csm (JEDI_AnalysisRun_) when GEOSJEDI=2 (FVDAS_Run_Config).
# Steps (each one restartable through its .DONE file):
#   1. observations  $FVWORK/ioda.<A>: swell_gsi2ioda.csh (GSI ncdiags), or links to
#                    $SWELL_OFFLINE_IODA_DIR/<A>T..Z/geos_atmosphere when that is set
#   2. analysis      swell_ana_central.csh
#   3. IAU           swell_mkiau.csh                      (SWELL_MKIAU=1)
#   4. use the IAU   swell_upd4geos.csh  (SWELL_IAU_OVERWRITE=1, forced when SKIPGSI)
# Logs: $FVWORK/$EXPID.jedi_{g2i,run,miau,upd}.log.<B>z.txt (archived as jedi/etc).
# ---------------------------------------------------------------------------

set myname = swell_driver.csh

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
set yyyymmddhh = ${nymda}${hha}

if ( ! $?EXPID || ! $?FVHOME || ! $?FVWORK ) then
   echo "${myname}: EXPID, FVHOME and FVWORK must be defined, aborting"
   exit 1
endif
if ( ! -e $FVHOME/run/swell/swell_config.csh ) then
   echo "${myname}: $FVHOME/run/swell/swell_config.csh not found, aborting"
   exit 1
endif
source $FVHOME/run/swell/swell_config.csh

set donefile = $FVWORK/.DONE_${myname}.$yyyymmddhh
if ( -e $donefile ) then
   echo "${myname}: already done"
   exit 0
endif

set mydir = `dirname $0`
set mydir = `cd $mydir && pwd`
set logtag = ${nymdb}_${hhb}z
setenv JEDIWORK $FVWORK/jedi.${nymda}.${nhmsa}
mkdir -p $JEDIWORK

# --- 1. observations -------------------------------------------------------
set iodadir = $FVWORK/ioda.${nymda}_${hha}0000
if ( "$SWELL_OFFLINE_IODA_DIR" != "" ) then
   set src = $SWELL_OFFLINE_IODA_DIR/${nymda}T${nhmsa}Z/geos_atmosphere
   set srcfiles = ( `find -H $src -maxdepth 1 \( -type f -o -type l \) |& grep "^$src/"` )
   if ( $#srcfiles == 0 ) then
      echo "${myname}: no IODA files in $src, aborting"
      exit 1
   endif
   mkdir -p $iodadir
   foreach f ( $srcfiles )
      ln -sf $f $iodadir/
   end
   echo "${myname}: observations linked from $src"
else
   $mydir/swell_gsi2ioda.csh $nymda $nhmsa $FVWORK $FVWORK $JEDIWORK |& tee -a $FVWORK/$EXPID.jedi_g2i.log.$logtag.txt
   if ( $status || ! -e $FVWORK/.DONE_swell_gsi2ioda.csh.$yyyymmddhh ) then
      echo "${myname}: swell_gsi2ioda.csh failed, aborting"
      exit 1
   endif
endif

# --- 2. analysis -----------------------------------------------------------
$mydir/swell_ana_central.csh $nymdb $nhmsb $nymda $nhmsa |& tee -a $FVWORK/$EXPID.jedi_run.log.$logtag.txt
if ( $status || ! -e $FVWORK/.DONE_swell_ana_central.csh.$yyyymmddhh ) then
   echo "${myname}: swell_ana_central.csh failed, aborting"
   exit 1
endif

# --- 3. IAU ----------------------------------------------------------------
if ( $SWELL_MKIAU ) then
   $mydir/swell_mkiau.csh $nymdb $nhmsb $nymda $nhmsa |& tee -a $FVWORK/$EXPID.jedi_miau.log.$logtag.txt
   if ( $status || ! -e $FVWORK/.DONE_swell_mkiau.csh.$yyyymmddhh ) then
      echo "${myname}: swell_mkiau.csh failed, aborting"
      exit 1
   endif
endif

# --- 4. GEOS uses the JEDI IAU ---------------------------------------------
set overwrite = $SWELL_IAU_OVERWRITE
if ( $?SKIPGSI ) then
   if ( $SKIPGSI ) then
      echo "${myname}: no GSI analysis (SKIPGSI): the JEDI IAU forcing is used"
      set overwrite = 1
   endif
endif
if ( $overwrite ) then
   if ( ! $SWELL_MKIAU ) then
      echo "${myname}: SWELL_IAU_OVERWRITE requires SWELL_MKIAU=1, aborting"
      exit 1
   endif
   $mydir/swell_upd4geos.csh $nymdb $nhmsb $nymda $nhmsa |& tee -a $FVWORK/$EXPID.jedi_upd.log.$logtag.txt
   if ( $status ) then
      echo "${myname}: swell_upd4geos.csh failed, aborting"
      exit 1
   endif
endif

touch $donefile
echo "${myname}: Complete"
exit 0
