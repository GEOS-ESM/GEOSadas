#!/bin/csh -f
#
# swell_mkiau.csh - GEOS analysis and IAU forcing from the central JEDI increment
#
# usage: swell_mkiau.csh nymdb nhmsb nymda nhmsa
#
#   nymdb nhmsb  beginning of the analysis window (IAU start)
#   nymda nhmsa  analysis time
#
# In $JEDIWORK=$FVWORK/jedi.<nymda>.<nhmsa> (as left by swell_ana_central.csh):
#   inc/$EXPID.jedi_inc1.eta.<date>.nc4   lat-lon JEDI increment (one file, 3D/FGAT)
# produces
#   ana/$EXPID.jedi_ana.eta.<A>.nc4         dyn_jediupd.x: lat-lon bkg + increment
#   inc/$EXPID.jedi_inc.eta.<A>.nc4         increment on the bkg grid (when grids differ)
#   iau/$EXPID.agcm_import_rst.<B>.nc4      mkiau.x (time tag of the IAU start)
#   $FVWORK/$EXPID.jedi_{agcmrst,ana,inc}.<B>.tar
#
# Lat-lon backgrounds: $FVHOME/recycle/$EXPID.bkg??_eta_rst.<date>.nc4
# mkiau resource: mkiau.rc.tenv ($FVHOME/run/swell, else $FVROOT/etc/swell), with
# ANADATE ANATIME ANAFNAME BKGFNAME AGCMIMPRST filled at run time.
# Port of the central branch of jedi/jedi_mkiau.csh (lat-lon path, 3D increments).
# ---------------------------------------------------------------------------

set myname = swell_mkiau.csh

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
set hhmmb = `echo $nhmsb | cut -c1-4`
set hhmma = `echo $nhmsa | cut -c1-4`
set yyyymmddhh = ${nymda}${hha}

if ( ! $?EXPID || ! $?FVHOME || ! $?FVWORK ) then
   echo "${myname}: EXPID, FVHOME and FVWORK must be defined, aborting"
   exit 1
endif
if ( ! $?SWELL_MKIAU_MPIRUN ) setenv SWELL_MKIAU_MPIRUN mpirun

set donefile = $FVWORK/.DONE_${myname}.$yyyymmddhh
if ( -e $donefile ) then
   echo "${myname}: already done"
   exit 0
endif

set mydir = `dirname $0`
set mydir = `cd $mydir && pwd`

set rctmpl = ""
foreach d ( $FVHOME/run/swell $mydir/../etc/swell $mydir/etc )
   if ( "$rctmpl" == "" && -e $d/mkiau.rc.tenv ) set rctmpl = $d/mkiau.rc.tenv
end
if ( "$rctmpl" == "" ) then
   echo "${myname}: cannot find mkiau.rc.tenv, aborting"
   exit 1
endif

setenv JEDIWORK $FVWORK/jedi.${nymda}.${nhmsa}
if ( ! -d $JEDIWORK/inc ) then
   echo "${myname}: $JEDIWORK/inc not found, aborting"
   exit 1
endif
cd $JEDIWORK
mkdir -p ana bkg iau

# --- lat-lon backgrounds (GEOSdas removes some from FVWORK when GSI runs) ---
# (links named like archived files: keep fvarchive out of bkg/)
touch bkg/.no_archiving
foreach fn ( `find -H $FVHOME/recycle -maxdepth 1 -name "${EXPID}.bkg*_eta_rst.*.nc4"` )
   set sfx = `basename $fn | cut -d. -f3-`
   ln -sf $fn bkg/$EXPID.bkg.eta.$sfx
end
set bkgfn = bkg/$EXPID.bkg.eta.${nymda}_${hhmma}z.nc4
if ( ! -e $bkgfn ) then
   echo "${myname}: $bkgfn not found (from $FVHOME/recycle), aborting"
   exit 1
endif

# --- the increment, valid at the analysis time -----------------------------
set incs = ( `find -H inc -maxdepth 1 -name "$EXPID.jedi_inc1.eta.*.nc4"` )
if ( $#incs != 1 ) then
   echo "${myname}: expected one inc/$EXPID.jedi_inc1.eta.*.nc4, found $#incs, aborting"
   exit 1
endif
set inc  = inc/$EXPID.jedi_inc1.eta.${nymda}_${nhmsa}z.nc4
if ( "$incs[1]" != "$inc" ) then
   # 3D increment written at another time of the window: constant in the window
   /bin/mv $incs[1] $inc
   reset_time.x $inc $nymda $nhmsa -9
endif

# --- analysis = background + increment -------------------------------------
set anafn = ana/$EXPID.jedi_ana.eta.${nymda}_${hhmma}z.nc4
/bin/rm -f $anafn
dyn_jediupd.x $nymda $nhmsa -o inc/$EXPID.jedi_inc.eta.${nymda}_${hhmma}z.nc4 $bkgfn $inc $anafn
if ( $status || ! -e $anafn ) then
   echo "${myname}: dyn_jediupd.x failed to create $anafn, aborting"
   exit 1
endif

# --- IAU forcing -----------------------------------------------------------
set iaufn = iau/$EXPID.agcm_import_rst.${nymda}_${hhmma}z.nc4
setenv ANADATE    $nymda
setenv ANATIME    $nhmsa
setenv ANAFNAME   $anafn
setenv BKGFNAME   $bkgfn
setenv AGCMIMPRST $iaufn
vED -env $rctmpl -o mkiau.rc
set nx = `echorc.x -rc mkiau.rc NX`
set ny = `echorc.x -rc mkiau.rc NY`
@ ncpus = $nx * $ny
touch input.nml
$SWELL_MKIAU_MPIRUN -np $ncpus mkiau.x
if ( $status || ! -e $iaufn ) then
   echo "${myname}: mkiau.x failed to create $iaufn, aborting"
   exit 1
endif
# the IAU file carries the time tag of the IAU start (as GEOSdas expects)
/bin/mv $iaufn iau/$EXPID.agcm_import_rst.${nymdb}_${hhmmb}z.nc4

# --- archive ---------------------------------------------------------------
cd iau
tar cf $FVWORK/$EXPID.jedi_agcmrst.${nymdb}_${hhb}z.tar $EXPID.agcm_import_rst.*
cd ../ana
tar cf $FVWORK/$EXPID.jedi_ana.${nymdb}_${hhb}z.tar $EXPID.jedi_ana*.nc4
cd ../inc
tar cf $FVWORK/$EXPID.jedi_inc.${nymdb}_${hhb}z.tar $EXPID.jedi_*.nc4
cd $JEDIWORK

touch $donefile
echo "${myname}: Complete"
exit 0
