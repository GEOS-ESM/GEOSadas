#!/bin/csh -f
#
# swell_ana_central.csh - ADAS central analysis (3D-FGAT, climatological B) with swell
#
# usage: swell_ana_central.csh nymdb nhmsb nymda nhmsa
#
#   nymdb nhmsb  beginning of the analysis window
#   nymda nhmsa  analysis time (middle of the window)
#
# Replaces jedi_set.csh + jedi_run.csh. On success $JEDIWORK=$FVWORK/jedi.<nymda>.<nhmsa>
# holds what jedi_mkiau.csh and the archiving expect:
#   inc/$EXPID.jedi_inc1.eta.<yyyymmdd_hhmmss>z.nc4    lat-lon increment (window begin for FGAT)
#   ana/$EXPID.jedi_ana.ceta.<yyyymmdd_hhmm>z.nc4      cubed analysis at the analysis time
#   bkg/ iau/                                          (filled by jedi_mkiau.csh)
# and in $FVWORK:
#   $EXPID.jedi_hofx.<B>.tar    observation feedback, named <obs>.<window begin>.nc4
#   $EXPID.jedi_vbc.<B>.tar     VarBC output at the window begin (+ tlapse, acftbias_cov)
#   $EXPID.jedi_var.log.<B>.txt JEDI log
#
# Inputs:
#   $FVWORK/ioda.<nymda>_<hh>0000      IODA obs and GSI-converted biases (swell_gsi2ioda.csh)
#   $SWELL_BKG_ARCHIVE/$EXPID/rs/Y%Y/M%m/$EXPID.bkgcrst.<analysis-9h>.tar   cubed backgrounds
#   $SWELL_VARBC_DIR                   previous JEDI VarBC (used if JEDI_FEEDBACK_VARBC=1)
#
# Resolution: SWELL_CENTRAL_CRES (npx: 13, 25, 91, 181, 361, 721) selects the processor
# layout and GSIBEC grid; any SWELL_CENTRAL_{NPX,NPY,PERHOST,GSIBEC_NLATS,GSIBEC_NLONS}
# already in the environment wins.
# ---------------------------------------------------------------------------

set myname = swell_ana_central.csh

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
if ( ! $?TIMEINC )            setenv TIMEINC 360
if ( ! $?JEDI_FEEDBACK_VARBC ) setenv JEDI_FEEDBACK_VARBC 0

set donefile = $FVWORK/.DONE_${myname}.$yyyymmddhh
if ( -e $donefile ) then
   echo "${myname}: already done"
   exit 0
endif

set mydir = `dirname $0`
set mydir = `cd $mydir && pwd`

# swell_config.csh provides the central settings (pins are re-checked by swell_run.csh)
source $mydir/swell_env.csh
if ( ! $SWELL_ENV_OK ) then
   echo "${myname}: swell environment setup failed, aborting"
   exit 1
endif
foreach v ( SWELL_CENTRAL_CRES SWELL_BKG_ARCHIVE SWELL_MKSI_PATH SWELL_VARBC_DIR SWELL_SLURM_ACCOUNT )
   printenv $v >& /dev/null
   if ( $status ) then
      echo "${myname}: $v is not set (see swell_config.csh), aborting"
      exit 1
   endif
end

# --- layout from resolution (as in setup_aanajedi.pl for 3D schemes) -------
switch ( $SWELL_CENTRAL_CRES )
   case 13:
      set lay = ( 2 2 24 46 72 )
      breaksw
   case 25:
      set lay = ( 4 4 96 46 72 )
      breaksw
   case 91:
      set lay = ( 6 6 12 91 144 )
      breaksw
   case 181:
      set lay = ( 8 8 16 361 576 )
      breaksw
   case 361:
      set lay = ( 10 10 16 361 576 )
      breaksw
   case 721:
      set lay = ( 10 10 16 721 1152 )
      breaksw
   default:
      echo "${myname}: no layout for SWELL_CENTRAL_CRES=$SWELL_CENTRAL_CRES, aborting"
      exit 1
endsw
if ( ! $?SWELL_CENTRAL_NPX )          setenv SWELL_CENTRAL_NPX          $lay[1]
if ( ! $?SWELL_CENTRAL_NPY )          setenv SWELL_CENTRAL_NPY          $lay[2]
if ( ! $?SWELL_CENTRAL_PERHOST )      setenv SWELL_CENTRAL_PERHOST      $lay[3]
if ( ! $?SWELL_CENTRAL_GSIBEC_NLATS ) setenv SWELL_CENTRAL_GSIBEC_NLATS $lay[4]
if ( ! $?SWELL_CENTRAL_GSIBEC_NLONS ) setenv SWELL_CENTRAL_GSIBEC_NLONS $lay[5]
@ ntasks = 6 * $SWELL_CENTRAL_NPX * $SWELL_CENTRAL_NPY
@ nodes  = ( $ntasks + $SWELL_CENTRAL_PERHOST - 1 ) / $SWELL_CENTRAL_PERHOST
setenv SWELL_CENTRAL_NODES $nodes
echo "${myname}: C$SWELL_CENTRAL_CRES layout ${SWELL_CENTRAL_NPX}x${SWELL_CENTRAL_NPY}x6 = $ntasks tasks on $nodes node(s)"

# --- time tags -------------------------------------------------------------
# btag: window begin (VarBC output);  ptag: previous window begin = background time
@ dtsec = $TIMEINC * 60
set pdate = ( `tick $nymdb $nhmsb -$dtsec` )
if ( $#pdate != 2 ) then
   echo "${myname}: tick failed, aborting"
   exit 1
endif
set hhp  = `echo $pdate[2] | awk '{printf "%06d", $1}' | cut -c1-2`
set btag = ${nymdb}T${hhb}0000Z
set ptag = $pdate[1]T${hhp}0000Z

setenv JEDIWORK $FVWORK/jedi.${nymda}.${nhmsa}
mkdir -p $JEDIWORK
cd $JEDIWORK
foreach d ( ana bkg inc iau hofx vbc )
   if ( -d $d ) /bin/rm -rf $d
   mkdir $d
end

# --- backgrounds: fail early if the tar swell will read is missing ---------
set yyyyp = `echo $pdate[1] | cut -c1-4`
set mmp   = `echo $pdate[1] | cut -c5-6`
set bkgtar = $SWELL_BKG_ARCHIVE/$EXPID/rs/Y$yyyyp/M$mmp/$EXPID.bkgcrst.$pdate[1]_${hhp}z.tar
if ( ! -e $bkgtar ) then
   echo "${myname}: background archive $bkgtar not found, aborting"
   exit 1
endif

# --- observations and bias correction --------------------------------------
set iodasrc = $FVWORK/ioda.${nymda}_${hha}0000
set nsrc = `find $iodasrc -maxdepth 1 \( -type f -o -type l \) |& grep -c "^$iodasrc/"`
if ( $nsrc == 0 ) then
   echo "${myname}: no IODA input in $iodasrc, aborting"
   exit 1
endif

setenv SWELL_IODA_DIR $JEDIWORK/swell_obs
set obsdir = $SWELL_IODA_DIR/${nymda}T${nhmsa}Z/geos_atmosphere
/bin/rm -rf $SWELL_IODA_DIR
mkdir -p $obsdir
foreach f ( `find $iodasrc -maxdepth 1 \( -type f -o -type l \)` )
   ln -sf $f $obsdir/
end
cd $obsdir

# bootstrap from GSI biases: satbias covariance = satbias file, aircraft bias renamed
foreach f ( `find . -maxdepth 1 -name "*.${ptag}.satbias.nc4"` )
   set sensor = `basename $f .${ptag}.satbias.nc4`
   if ( ! -e ${sensor}.${ptag}.satbias_cov.nc4 ) then
      ln -sf $f:t ${sensor}.${ptag}.satbias_cov.nc4
   endif
end
if ( -e aircraft_abias_air.${ptag}.nc4 ) then
   if ( ! -e aircraft_temperature.${ptag}.acftbias ) then
      ln -sf aircraft_abias_air.${ptag}.nc4 aircraft_temperature.${ptag}.acftbias
   endif
   if ( ! -e aircraft_temperature.${ptag}.acftbias_cov ) then
      ln -sf aircraft_abias_air.${ptag}.nc4 aircraft_temperature.${ptag}.acftbias_cov
   endif
endif

# cycled VarBC: previous JEDI output at this background time replaces the GSI biases
set vbcsrc = "GSI (bootstrap)"
if ( $JEDI_FEEDBACK_VARBC ) then
   set prev = ()
   if ( -d $SWELL_VARBC_DIR ) set prev = ( `find $SWELL_VARBC_DIR -maxdepth 1 -type f -name "*.${ptag}.*"` )
   if ( $#prev > 0 ) then
      foreach f ( $prev )
         ln -sf $f $f:t
      end
      set vbcsrc = "JEDI $SWELL_VARBC_DIR ($#prev files)"
   else
      echo "${myname}: WARNING: no VarBC at $ptag in $SWELL_VARBC_DIR, using GSI biases"
   endif
endif
echo "${myname}: bias correction from $vbcsrc"
cd $JEDIWORK

# --- run swell -------------------------------------------------------------
set expid = ${EXPID}-ana-$yyyymmddhh
setenv SWELL_SLURM_TEMPLATE central_slurm.yaml.tmpl
$mydir/swell_run.csh 3dfgat_atmos central_3dfgat.yaml.tmpl $JEDIWORK $expid $nymda $nhmsa
if ( $status ) then
   echo "${myname}: swell 3dfgat_atmos failed, aborting"
   exit 1
endif
set cycdir = $JEDIWORK/$expid/run/${nymda}T${nhmsa}Z/geos_atmosphere

# --- JEDI log --------------------------------------------------------------
set varlog = $cycdir/jedi_variational_log.log
if ( ! -e $varlog ) then
   echo "${myname}: $varlog not found, aborting"
   exit 1
endif
/bin/cp $varlog $FVWORK/$EXPID.jedi_var.log.${nymdb}_${hhb}z.txt
set nnan = `grep -c NaN $varlog`
if ( $nnan > 0 ) then
   echo "${myname}: NaNs in the JEDI log, aborting"
   exit 1
endif
grep "Quadratic cost function: J " $varlog | head -1
grep "Quadratic cost function: J " $varlog | tail -1

# --- increment and analysis ------------------------------------------------
set incs = ( `find $cycdir -maxdepth 1 -type f -name "${expid}.increment-iter1.*.nc4"` )
if ( $#incs == 0 ) then
   echo "${myname}: no increment ${expid}.increment-iter1.*.nc4 in $cycdir, aborting"
   exit 1
endif
foreach f ( $incs )
   set ttag = `basename $f .nc4 | sed "s/^${expid}\.increment-iter1\.//"`
   /bin/mv $f inc/$EXPID.jedi_inc1.eta.${ttag}.nc4
end

set anafn = $cycdir/${expid}.analysis.${nymda}_${nhmsa}z.nc4
if ( -e $anafn ) then
   /bin/mv $anafn ana/$EXPID.jedi_ana.ceta.${nymda}_${hha}`echo $nhmsa | cut -c3-4`z.nc4
else
   echo "${myname}: WARNING: no cubed analysis at the analysis time in $cycdir"
endif

# --- observation feedback --------------------------------------------------
foreach f ( `find $cycdir -maxdepth 1 -type f -name "${expid}.*.${btag}.nc4"` )
   set obsname = `basename $f | sed "s/^${expid}\.//"`
   /bin/mv $f hofx/$obsname
end
cd hofx
set nhofx = `find . -maxdepth 1 -name "*.nc4" | wc -l`
if ( $nhofx > 0 ) then
   tar cf $FVWORK/$EXPID.jedi_hofx.${nymdb}_${hhb}z.tar *.nc4
endif
cd $JEDIWORK
echo "${myname}: $nhofx observation feedback file(s)"

# --- VarBC output for the next cycle ---------------------------------------
foreach f ( `find $cycdir -maxdepth 1 -type f \( -name "*.${btag}.satbias.nc4" -o -name "*.${btag}.satbias_cov.nc4" -o -name "*.${btag}.acftbias" -o -name "*.${btag}.acftbias_cov" \)` )
   /bin/mv $f vbc/
end
# JEDI writes no acftbias covariance and no tlapse: carry the inputs forward
if ( -e vbc/aircraft_temperature.${btag}.acftbias && ! -e vbc/aircraft_temperature.${btag}.acftbias_cov ) then
   if ( -e $obsdir/aircraft_temperature.${ptag}.acftbias_cov ) then
      /bin/cp -L $obsdir/aircraft_temperature.${ptag}.acftbias_cov vbc/aircraft_temperature.${btag}.acftbias_cov
   endif
endif
foreach f ( `find $obsdir -maxdepth 1 -name "*.${ptag}.tlapse.txt"` )
   set sensor = `basename $f | cut -d. -f1`
   /bin/cp -L $f vbc/$sensor.${btag}.tlapse.txt
end
cd vbc
set nvbc = `find . -maxdepth 1 -type f | wc -l`
if ( $nvbc > 0 ) then
   tar cf $FVWORK/$EXPID.jedi_vbc.${nymdb}_${hhb}z.tar *
   mkdir -p $SWELL_VARBC_DIR
   find $SWELL_VARBC_DIR -maxdepth 1 -type f -delete
   /bin/cp -p * $SWELL_VARBC_DIR/
endif
cd $JEDIWORK
echo "${myname}: $nvbc VarBC file(s) saved for the next cycle in $SWELL_VARBC_DIR"

# --- record of the JEDI configuration --------------------------------------
# (atm_ens.j counts run/jedi/Config/geosvar.*z.yaml to decide that the central
#  JEDI analysis is cycling; keep providing it until the ensemble moves to swell)
if ( -d $FVHOME/run/jedi/Config ) then
   /bin/cp $cycdir/jedi_variational_config.yaml $FVHOME/run/jedi/Config/geosvar.${nymda}_${hha}z.yaml
endif

touch $donefile
echo "${myname}: Complete"
exit 0
