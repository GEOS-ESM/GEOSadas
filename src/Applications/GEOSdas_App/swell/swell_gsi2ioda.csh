#!/bin/csh -f
#
# swell_gsi2ioda.csh - convert GSI ncdiags and GSI bias files to IODA with swell
#
# usage: swell_gsi2ioda.csh nymda nhmsa inpdir outdir workdir
#
#   nymda nhmsa  analysis date/time
#   inpdir       directory holding $EXPID.diag_*_ges.<date>.nc4 (normally $FVWORK)
#   outdir       results go to $outdir/ioda.<nymda>_<hh>0000
#   workdir      scratch for this conversion (swell experiment_root)
#
# Inputs:  $inpdir/$EXPID.diag_*_ges.*.nc4
#          $FVHOME/recycle/$EXPID.{ana_satbias_rst,ana_satbiaspc_rst,ana_acftbias_rst}.*.txt
# Outputs: $outdir/ioda.<nymda>_<hh>0000/  IODA obs, satbias, tlapse, aircraft bias
#
# swell (GetGsiBc) looks for the bias files at the previous analysis time
# (analysis - TIMEINC); the single recycle file of each type is linked under that name.
# Override template: convert_ncdiags.yaml.tmpl (see swell_run.csh for the search path).
# ---------------------------------------------------------------------------

set myname = swell_gsi2ioda.csh

if ( $#argv < 5 ) then
   echo "usage: $myname nymda nhmsa inpdir outdir workdir"
   exit 1
endif

set nymda   = $1
set nhmsa   = `echo $2 | awk '{printf "%06d", $1}'`
set inpdir  = $3
set outdir  = $4
set workdir = $5
set hha     = `echo $nhmsa | cut -c1-2`
set yyyymmddhh = ${nymda}${hha}

if ( ! $?EXPID || ! $?FVHOME || ! $?FVWORK ) then
   echo "${myname}: EXPID, FVHOME and FVWORK must be defined, aborting"
   exit 1
endif
if ( ! $?TIMEINC ) setenv TIMEINC 360

set donefile = $FVWORK/.DONE_${myname}.$yyyymmddhh
if ( -e $donefile ) then
   echo "${myname}: already done"
   exit 0
endif

set mydir = `dirname $0`
set mydir = `cd $mydir && pwd`

# previous analysis time: tag swell expects on the GSI bias files
@ dtsec = $TIMEINC * 60
set pdate = ( `tick $nymda $nhmsa -$dtsec` )
if ( $#pdate != 2 ) then
   echo "${myname}: tick failed, aborting"
   exit 1
endif
set ptag = $pdate[1]_`echo $pdate[2] | cut -c1-2`z

# --- stage inputs ----------------------------------------------------------
set stage = $workdir/g2i_inputs.$yyyymmddhh
/bin/rm -rf $stage
mkdir -p $stage/gsi_diags $stage/gsi_bias

set diags = ( `find -H $inpdir -maxdepth 1 -name "${EXPID}.diag_*_ges.*nc4"` )
if ( $#diags == 0 ) then
   echo "${myname}: no $inpdir/${EXPID}.diag_*_ges.*nc4 found, aborting"
   exit 1
endif
foreach f ( $diags )
   ln -sf $f $stage/gsi_diags/
end

foreach ftype ( ana_acftbias_rst ana_satbias_rst ana_satbiaspc_rst )
   set fn = ( `find -H $FVHOME/recycle -maxdepth 1 -name "${EXPID}.${ftype}.*.txt"` )
   if ( $#fn != 1 ) then
      echo "${myname}: expected one $FVHOME/recycle/${EXPID}.${ftype}.*.txt, found $#fn, aborting"
      exit 1
   endif
   ln -sf $fn[1] $stage/gsi_bias/${EXPID}.${ftype}.${ptag}.txt
end

setenv SWELL_GSI_DIAG_DIR $stage/gsi_diags
setenv SWELL_GSI_BIAS_DIR $stage/gsi_bias

# --- run swell -------------------------------------------------------------
set expid = ${EXPID}-g2i-$yyyymmddhh
$mydir/swell_run.csh convert_ncdiags convert_ncdiags.yaml.tmpl $workdir $expid $nymda $nhmsa
if ( $status ) then
   echo "${myname}: swell convert_ncdiags failed, aborting"
   exit 1
endif

# --- deliver ---------------------------------------------------------------
set cycdir = $workdir/$expid/run/${nymda}T${nhmsa}Z/geos_atmosphere
set iodadir = $outdir/ioda.${nymda}_${hha}0000
mkdir -p $iodadir
set outs = ( `find -H $cycdir -maxdepth 1 -type f \( -name "*.nc4" -o -name "*.txt" \)` )
if ( $#outs == 0 ) then
   echo "${myname}: no IODA output in $cycdir, aborting"
   exit 1
endif
/bin/mv $outs $iodadir/
echo "${myname}: $#outs file(s) moved to $iodadir"

touch $donefile
echo "${myname}: Complete"
exit 0
