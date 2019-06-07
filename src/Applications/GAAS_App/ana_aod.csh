#!/bin/csh -f
#
# Simple wrapper script for the AOD analysis.
#


if ( $#argv < 4 ) goto usage

if ( "$1" == "-f" ) then
    set force = 1
    shift
    if ( $#argv < 4 ) goto usage
else
    set force = 0
endif 

set yy = $1
set mm = $2
set dd = $3
set hh = $4

#                 --------------------
#                 File and Directories
#                 --------------------

set here = `dirname $0`
set ExpRootDir = `(cd $here/..; pwd)`
set ExpId = `basename $ExpRootDir`

set aerDir = "/discover/nobackup/projects/gmao/iesa/aerosol/experiments/dR_Fortuna-M-1-1/inst2d_hwl_x"
set aer_f = "$aerDir/Y$yy/M$mm/dR_Fortuna-M-1-1.inst2d_hwl_x.$yy$mm${dd}_${hh}00z.nc4"

if ( ! -e $aer_f ) then
    echo "cannot find aerosol concentration file $aer_f"
    exit 1
endif

set chemDir = "$ExpRootDir/chem/Y$yy/M$mm"
set obsDir  = "$ExpRootDir/obs/Y$yy/M$mm"
set etcDir  = "$ExpRootDir/etc/Y$yy/M$mm"
set scrDir  = "$ExpRootDir/scratch/$$"
set mrgDir  = "$ExpRootDir/morgue"
set rstDir  = "$ExpRootDir/recycle"

mkdir -p $chemDir
mkdir -p $obsDir
mkdir -p $etcDir
mkdir -p $scrDir
mkdir -p $mrgDir
mkdir -p $rstDir

set aod_f = "$chemDir/$ExpId.aod_f.sfc.$yy$mm${dd}_${hh}00z.nc"
set aod_a = "$chemDir/$ExpId.aod_a.sfc.$yy$mm${dd}_${hh}00z.nc"
set aod_d = "$chemDir/$ExpId.aod_d.sfc.$yy$mm${dd}_${hh}00z.nc"
set aod_k = "$chemDir/$ExpId.aod_k.sfc.$yy$mm${dd}_${hh}00z.nc"
set aod_b = "$chemDir/$ExpId.aod_b.sfc.$yy$mm${dd}_${hh}00z.nc"
set ods_a = "$obsDir/$ExpId.aod.obs.$yy$mm${dd}_${hh}00z.ods"
set lst_a = "$etcDir/$ExpId.ana_aod.stdout.$yy$mm${dd}_${hh}00z.txt"
set lst_b = "$etcDir/$ExpId.aod_l2a.stdout.$yy$mm${dd}_${hh}00z.txt"

set bias_rs = "$rstDir/aodbias_internal_restart.nc"
set bias_ck = "$scrDir/aodbias_internal_checkpoint.nc"

set nnr_fn = "/discover/nobackup/projects/gmao/iesa/aerosol/experiments/dR_Fortuna-M-1-1/NNR/Level2/M?D04/Y$yy/M$mm/nnr_002.M?D04_L2a.*.$yy$mm${dd}_${hh}00z.ods"

# Do not recomputed, unless -f is specified
# -----------------------------------------
  if ( $force ) then
    /bin/rm -rf $aod_f $aod_a $aod_d $ods_a $lst_a $nnr_fn
  else
    if ( -e $ods_a ) then
      echo $0": file <$ods_a> exists; skipping calculation." |& tee stdout.txt
      echo $0": specify -f to remove this file and perform analysis."
      mkdir -p $mrgDir/$$
      mv stdout.txt $mrgDir/$$
      exit 1
    endif
  endif

#                               --------------
#                               Execute Binary
#                               --------------

# First, perform MODIS NNR retrieval
# ----------------------------------
  modis_l2a.py modis_l2a.pcf $yy$mm$dd ${hh}0000 |& tee stdout.txt
  if ( $status ) then
      echo $0": cannot create MODIS Level 2a on $yy $mm $dd GMT $hh" |& tee stderr.txt
      mkdir -p $mrgDir/$$
      mv stderr.txt stdout.txt $mrgDir/$$
      exit 1
  else
     gzip stdout.txt
     /bin/mv stdout.txt.gz $lst_b.gz
  endif

# Running environment
# -------------------
  limit  stack           unlimited
  setenv ARCH `uname -s`
  setenv PSAS_NUM_MPI    $NCPUS # for PSAS
  setenv MPIRUN          "mpirun -np $PSAS_NUM_MPI"
  ###setenv OMP_NUM_THREADS 2 # for SQC
  if ( ! $?ESMADIR ) then
      echo $0": ESMADIR variable is not set"
      exit 1
  endif
  set path = ( $ESMADIR/$ARCH/bin $path )

# Clean slate
# -----------
  /bin/rm -rf ana-psas.bin 
  /bin/rm -rf $ods_a.ods
  /bin/rm -f  ExtData

# Put rc files in running directory
# ---------------------------------
  /bin/cp *.rc $scrDir
  cd $scrDir

# Make sure external data is available
# ------------------------------------ 
  ln -s $SHARE/dasilva/fvInput ExtData 

# Stage Bias restart
# ------------------
  if ( -e $bias_rs ) /bin/cp $bias_rs $scrDir

# Run the AOD analysis Fortran code
# ---------------------------------
  echo $0": Starting AOD analysis at $yy $mm $dd $hh ..."
  ana_aod.x -v -x $ExpId -f $aod_f -a $aod_a -d $aod_d -k $aod_k \
               -o $ods_a $aer_f  |&  tee stdout.txt
  set ana_status = $status

#                               ----------------
#                                Post-processing
#                               ----------------

# Bias output
# -----------
  if ( -e $bias_ck ) then
       /bin/cp $bias_ck $aod_b    # regular bias output
       /bin/mv $bias_ck $bias_rs  # checkpoint becomes restart
  endif

# Something is wrong, stop here
# -----------------------------
  if ( $ana_status ) then
    mv $scrDir $mrgDir
    echo $0": abnormal end at $yy $mm $dd $hh, morgue at $mrgDir/$$"
    exit 1
  endif

# Compress output files
# ---------------------
  foreach file ( $aod_f $aod_a $aod_d $aod_b $aod_k $ods_a )
    if ( -e $file ) then
         n4zip $file
    endif
  end

# Listing
# -------
  gzip stdout.txt
  /bin/mv stdout.txt.gz $lst_a.gz
  /bin/rm -rf $scrDir 

# All done
# --------
  echo $0": All done for $yy $mm $dd $hh!"
  exit 0

#................................................................

usage:

    echo "Usage:"
    echo "       ana_aod.csh [-f] yyyy mm dd hh"
    echo "Example:"
    echo "       ana_aod.csh 2008 06 29 12"
    echo ""
    echo "OPTIONS:"
    echo "  -f   by default, it will not do any calculation"
    echo "       if there is an output ODS file already; when" 
    echo "       -f is specified the ODS will be overwritten."
    echo ""
    exit 1

