#!/bin/csh -fx
# ------------------------------
#SBATCH --account=g0613
#SBATCH --constraint=hasw
#SBATCH --ntasks=96
#SBATCH --ntasks-per-node=24
#
#SBATCH --job-name=atm_ens_ageps
#SBATCH --output=atm_ens_ageps.log.o%j
#SBATCH --time=6:00:00
#PBS -N atm_ens_ageps
#PBS -o atm_ens_ageps.log.o%j
#_PBS -l select=1:ncpus=24
#PBS -l walltime=6:00:00
##PBS -l mem=4gb
#PBS -S /bin/csh
#PBS -j eo
# ------------------------------
#
# fvDAS driver script.
#
# This file has been automatically generated by fvsetup.
#
# fvDAS v5_7_1 setup on Wed Sep 28 13:35:13 EDT 2011 by rtodling.


#--------------------------------------------------------------------
 set myname = `basename $0`

# Source Run Time Environment settings
# ------------------------------------
  setenv EXPID   x0026   # experiment ID
  setenv FVHOME  /discover/nobackup/$user/$EXPID   # experiment home directory
  if (-e $FVHOME/run/FVDAS_Run_Config) source $FVHOME/run/FVDAS_Run_Config
# tell which side of the machine we are on ...
  uname -a

  env

#
#                 ----------------------------------
#                  PART I - Prepare the Environment
#                 ----------------------------------

# Experiment environment
# ----------------------
# setenv JOBGEN_QOS advda
  setenv JOBGEN_CONSTRAINT hasw
  setenv GID g0613
  setenv group_list "SBATCH -A $GID"
  setenv ARCH `uname -s`
  setenv HOST `uname -n`
  setenv CASE    $EXPID  # experiment ID (for LSM's sake)
  setenv FVROOT  `cat $FVHOME/.FVROOT`
  setenv FVRUN   $FVHOME/run
  setenv BIGNAME `echo "$EXPID" | tr -s '[:lower:]' '[:upper:]'`

# Load BASEDIR and modules
# ------------------------
  source $FVROOT/bin/g5_modules

# Add FVROOT/bin to front of path so fvDAS binaries are found first
# -----------------------------------------------------------------
  if ( `uname -s` == "Linux" ) then
    set path = ( . $FVHOME/run $FVROOT/bin /share/dasilva/opengrads/Contents $BASEDIR/$ARCH/bin $path )
  else
    set path = ( . $FVHOME/run $FVROOT/bin /share/dasilva/opengrads/Contents $path )
  endif

# Get date/time from restarts
# ---------------------------
  cd $FVHOME/run/ageps
  set lstcases = `/bin/ls -1 standalone_ageps.*`
  if ( $status ) then
    echo $myname": no standalone AGEPS date/time specifed, aborting ..."
    exit 1
  else
     set nymdb = `echo $lstcases[1] | cut -d+ -f1 | cut -d. -f2 | cut -c1-8`
     set hhb   = `echo $lstcases[1] | cut -d+ -f1 | cut -d. -f2 | cut -c10-11`
     set ddb   = `echo $nymdb | cut -c7-8`
     set nhmsb = ${hhb}0000
     set yyyymmddhh = ${nymdb}${hhb}

     set nymde = `echo $lstcases[1] | cut -d+ -f2 | cut -c1-8`
     set hhe   = `echo $lstcases[1] | cut -d+ -f2 | cut -c10-11`

     set diff  = `diffdates.py ${nymdb}${hhb} ${nymde}${hhe}` 
     /bin/mv $lstcases[1] running.$lstcases[1]
  endif

# Define work directory
# ---------------------
  if( (`uname -s` == "Linux") ) then
      if( `uname -m` != "ia64" ) then
         setenv FORT90L -Wl,-T
      endif
      setenv FVWORK /discover/nobackup/projects/gmao/obsdev/$user/ensagepswork_${yyyymmddhh}.$BIGNAME
      if ($?kidwork) then  # this case, overwrite FVWORK with user-specific
         setenv FVWORK $kidwork
      endif
  endif
  /bin/mkdir -p $FVWORK            # create working directory
  echo $FVWORK >! $FVHOME/.ENSWORK  # record working directory

  setenv FVBCS $FVHOME/fvInput
  setenv EXTDATA $FVWORK/ExtData  # External data directory
  /bin/mkdir -p $EXTDATA
  /bin/touch $EXTDATA/.no_archiving
  /bin/rm -f $EXTDATA/agcmpert
  /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/agcmpert $EXTDATA/
  /bin/rm -f $EXTDATA/g5chem
  /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/g5chem $EXTDATA/
  /bin/rm -f $EXTDATA/g5gcm
  /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/g5gcm $EXTDATA/
  /bin/rm -f $EXTDATA/PIESA
  /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/PIESA $EXTDATA/
  /bin/rm -f $EXTDATA/MERRA2
  /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/MERRA2 $EXTDATA/
  /bin/rm -f $EXTDATA/AeroCom
  /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/AeroCom $EXTDATA/
  /bin/rm -f $EXTDATA/enAdas
  /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/enAdas $EXTDATA/

# MPI/OpenMP Hybrid Options
# -------------------------
  if ( -e $FVHOME/run/atmens/mkiau.rc.tmpl ) then
     set IAUX = `which mkiau.x`
  else
     set IAUX = `which makeiau.x`
  endif
  set ANAX    = `which GSIsa.x`
  set SACX    = `which sac.x`

# The following are from OPS parallel
  setenv I_MPI_FABRICS shm:dapl
  setenv I_MPI_FABRICS_LIST "dapl,ofa"
  setenv I_MPI_FALLBACK "enable"
  setenv I_MPI_MPD_RSH sshmpi
  setenv I_MPI_DAPL_CHECK_MAX_RDMA_SIZE 1
#
  setenv DAPL_UCM_CQ_SIZE 4096
  setenv DAPL_UCM_QP_SIZE 4096
  setenv I_MPI_DAPL_UD_SEND_BUFFER_NUM 4096
  setenv I_MPI_DAPL_UD_RECV_BUFFER_NUM 4096
  setenv I_MPI_DAPL_UD_ACK_SEND_POOL_SIZE 4096
  setenv I_MPI_DAPL_UD_ACK_RECV_POOL_SIZE 4096
  setenv I_MPI_DAPL_UD_RNDV_EP_NUM 2
  setenv I_MPI_DAPL_UD_REQ_EVD_SIZE 2000
  setenv DAPL_UCM_REP_TIME 2000
  setenv DAPL_UCM_RTU_TIME 2000
  setenv DAPL_UCM_RETRY 7
  setenv DAPL_ACK_RETRY 7
  setenv DAPL_ACK_TIMER 20
  setenv DAPL_UCM_RETRY 10
  setenv DAPL_ACK_RETRY 10

# For reproducibility between Westmere and Sandybridge nodes
# ----------------------------------------------------------
  setenv MV2_USE_SHMEM_ALLREDUCE 0
  setenv MV2_ON_DEMAND_THRESHOLD 8192
  setenv MKL_CBWR SSE4_2
 
# set HDF2RSX = `which hdf2rs.x`
# Regular DAS env vars
# --------------------
  setenv TIMEINC 360      # analysis frequency in minutes (script not general enough for this to be anything)
  setenv ASYNBKG 180      # background frequency in minutes (script not general enough for this to be anything)
  setenv VAROFFSET 180    # abs value of time off from 1st synoptic hour of var window
  setenv NCSUFFIX nc4
  setenv NCEPINPUT $FVBCS
  setenv SPECRES    62    # should be able to revisit analyzer to avoid needing this

  setenv GAAS_ANA 1

# Run-time mpi-related options
# ----------------------------
  setenv DAPL_ACK_RETRY 7
  setenv DAPL_ACK_TIMER 20
  setenv DAPL_RNR_RETRY 7
  setenv DAPL_RNR_TIMER 28
  setenv I_MPI_MPD_TMPDIR /tmp
  setenv I_MPI_USE_DYNAMIC_CONNECTIONS 0
  setenv I_MPI_JOB_STARTUP_TIMEOUT 10000
  setenv I_MPI_RDMA_RNDV_WRITE 1

# MVAPICH variables
# -----------------
  setenv MPI_COLL_REPRODUCIBLE
  setenv SLURM_DISTRIBUTION block
  setenv MPI_DISPLAY_SETTINGS

# For some reason, PMI_RANK is randomly set and interferes
# with binarytile.x and other executables.
  unsetenv PMI_RANK

  setenv MPI_COMM_MAX  1024
  setenv MPI_GROUP_MAX 1024
  setenv MPI_BUFS_PER_PROC 256

# Internal parameters controlling system behavior
# ----------------------------------------------
  source $FVHOME/run/atmens/AtmEnsConfig.csh

  if ( !($?ATMENS_BATCHSUB) ) then
     echo "atm_ens_geps.j: missing  batch command"
     exit(1)
  endif

# Costumize what to be save from offline forecasts
# ------------------------------------------------
# setenv ENSARCH_FIELDS "eprg,edia,fstat"
  setenv ENSARCH_FIELDS "eprg,fstat"

  setenv HYBRIDGSI     $FVWORK
  setenv STAGE4HYBGSI  $FVWORK
  setenv ATMENSLOC     $FVWORK/atmens
  setenv STAGEEFSENS   $FVHOME/asens
  setenv  DO_ATM_ENS    0
  touch $ATMENSLOC/.no_archiving
  if ( $?eanaonly ) then
     if ( -e $FVHOME/run/${EXPID}_scheduler.j ) then
        setenv  DO_ATM_ENS    0
     endif
  endif
         #    The following set specific pieces separately
         #    Note: FVWORK better be defined by hand 
         #    ---------------------------------------------
         if ( $?eanaonly ) then
            setenv  RUN_EAAOD     1
         else
            setenv  RUN_EAAOD     0
         endif
         setenv  RUN_AGEPS_SET  1
         setenv  RUN_ENS2GCM    1
         setenv  RUN_AENSFCST   1
         setenv  RUN_POSTFCST   0
         setenv  RUN_AENSEFS    0
         setenv  RUN_AENSADFC   1
         setenv  RUN_AENSVTRACK 0
         setenv  RUN_ARCHATMENS 0
  # To trigger ensemble-only set following dates to anything but 0 (as yyyymmddhh)
  #setenv ENSONLY_BEG 2012111221
  #setenv ENSONLY_END 2014050621
  setenv ENSONLY_BEG 0
  setenv ENSONLY_END 0

  setenv DATADIR /archive/u/dao_it
  setenv SRCEXPID x0033_SBC
  setenv ATMGEPS 1
  setenv ATMENS_GEPS_RECENTER 1
# setenv ATMENS_GEPS_FROM_CENTRAL 1
  setenv AENS_DONORECENTER    0
  setenv AENS_ADDINFLATION    0
  setenv FCSTVERIFY ana
# set atype = niana
# set aver  = niana
  set atype = ana
  set aver  = cana
  set aver  = emana

# initial error w/ respect to ana (or bkg, or niana)
  set iniwrt  = niana
  set iniwrt  = ana

# Make sure files are accessible
# ------------------------------
  umask 022

# Discover specific configuration
# ----------------------------
  limit stacksize unlimited
  limit coredumpsize 0
  setenv KMP_STACKSIZE    450m
  unsetenv F_UFMTENDIAN
  setenv KMP_LIBRARY turnaround

  @ taub       = 24 * $diff[1] + $diff[2] - $VAROFFSET / 60
  @ varoffset_sc = $VAROFFSET * 60
  set anadate  = `tick $nymdb $nhmsb $varoffset_sc` 
  set anymd    = $anadate[1]
  set anhms    = $anadate[2]

# Get positioned
# --------------
  setenv STAGEEFSENS   $FVHOME/asens/Hold/${taub}hr/EnsEstimate
  cd $FVWORK

# Define prefix and suffix of job names
  setenv JOBGEN_PFXNAME e26geps
  setenv JOBGEN_SFXNAME ${ddb}_${hhb}z
  if ( $?PBS_JOBID ) then
     qalter -N ${JOBGEN_PFXNAME}_atm_ens_${JOBGEN_SFXNAME} $PBS_JOBID
  else
     if ( $?SLURM_JOBID ) then
         qalter -N ${JOBGEN_PFXNAME}_atm_ens_${JOBGEN_SFXNAME} $SLURM_JOBID
     else
         echo "no JOBID found, abort"
         exit(1)
     endif
  endif

# Get positioned in work area
# ---------------------------
  cd $FVWORK
  if ( -e .FAILED ) then
      echo " Main: clearing past FAILURE "
      /bin/rm .FAILED
  endif

# Set up work directory with needed inputs
# ----------------------------------------
  if ( $RUN_AGEPS_SET || $DO_ATM_ENS ) then

#    Create RC file with files required to run ensemble analysis/ob-impact
#    ---------------------------------------------------------------------
     zeit_ci.x pgeps
     atmens_prepgeps.csh $EXPID $anymd $anhms $taub $atype $aver "setrc"
     if( $status) then
        echo "prepgeps(1) failed"
        exit(1)
     endif
     zeit_co.x pgeps

#    Acquire required fields/files for running analysis
#    --------------------------------------------------
     if ( -e ageps.rc ) then
        if ( ! -e $FVWORK/.DONE_MEM001_GET4AGEPS.$yyyymmddhh) then 
         if(! -d $STAGE4HYBGSI ) mkdir -p $STAGE4HYBGSI
         set spool = "-s $FVWORK/spool"        
         jobgen.pl \
                -q datamove \
                get4ageps           \
                $GID                \
                $OBSVR_WALLCLOCK    \
                "acquire -v -strict -rc $FVWORK/ageps.rc  -d $STAGE4HYBGSI $spool -ssh $anymd $anhms 060000 1" \
                $STAGE4HYBGSI       \
                $myname             \
                $FVWORK/.DONE_MEM001_GET4AGEPS.$yyyymmddhh \
                "Main job script Failed for Get for Ensemble Prediction System"
   
                if ( -e get4ageps.j ) then
                   if ( $ATMENS_BATCHSUB == "sbatch" ) then
                      $ATMENS_BATCHSUB  -W get4ageps.j
                   else
                      $ATMENS_BATCHSUB  -W block=true get4ageps.j
                   endif
                   touch .SUBMITTED
                else
                   echo " $myname: Failed for Get for AGEPS, Aborting ... "
                   touch $FVWORK/.FAILED
                   exit(1)
                endif
        endif
     endif

#    Create RC file with files required to run ensemble analysis/ob-impact
#    ---------------------------------------------------------------------
     zeit_ci.x pgeps
     atmens_prepgeps.csh $EXPID $anymd $anhms $taub $atype $aver NULL
     if( $status) then
        echo "prepgeps(2) failed"
        exit(1)
     endif
     zeit_co.x pgeps

  endif  # RUN_GEPS_SET

# Calculate IAU-related forcing terms
# -----------------------------------
  if ( $RUN_ENS2GCM || $DO_ATM_ENS ) then
      zeit_ci.x ens2gcm
      if ( $ATMENS_DO4DIAU ) then
         atmos_ens2gcm.csh $EXPID $nymdb $nhmsb |& tee -a atm_ens.log
      else
         atmos_ens2gcm.csh $EXPID $anymd $anhms |& tee -a atm_ens.log
      endif
      if( $status) then
         echo "ens2gcm failed"
         exit(1)
      endif
      zeit_co.x ens2gcm
  endif

# Run ensemble of atmospheric GCMs
# ---------------------------------
  if( $RUN_AENSFCST || $DO_ATM_ENS ) then
      # RT: I don't like this way of determining the dimension, but so be it for now
      if ( -e mem001/$EXPID.bkg.eta.${nymdb}_${hhb}z.$NCSUFFIX ) then
         zeit_ci.x gcm_ens
         set ens_mres  = `getgfiodim.x mem001/$EXPID.bkg.eta.${nymdb}_${hhb}z.$NCSUFFIX`
         set ens_nlons = $ens_mres[1]
         set ens_nlats = $ens_mres[2]
         @ tfcst_hh  = 2 * $TIMEINC / 60
         gcm_ensemble.csh $EXPID $nymdb $nhmsb $tfcst_hh $ens_nlons $ens_nlats |& tee -a atm_ens.log
         if( $status) then
            echo "gcm_ensemble failed"
            exit(1)
         endif
         zeit_co.x gcm_ens
      else
         echo "cannot find $ATMENSLOC/ensmean/$EXPID.bkg.eta.${nymdb}_${hhb}z.$NCSUFFIX "
         echo "gcm_ensemble unable to run"
         exit(1)
      endif
  endif

# Run vortex track on forecast members
# ------------------------------------
  if( $RUN_AENSVTRACK ) then
     if ( $nhmsb == 210000 || $nhmsb == 090000 ) then # temporarily wired
        zeit_ci.x vtrack_ens
        atmens_vtrack.csh $EXPID $nymdb $nhmsb
        if( $status) then
           echo "vtrack_ensemble failed"
           exit(1)
        endif
        zeit_co.x vtrack_ens
     endif
  endif

# Ensemble-based adjoint-free estimate of forecast sensivitity to initial condiction
# ----------------------------------------------------------------------------------
  if( $RUN_AENSEFS || $DO_ATM_ENS ) then
     zeit_ci.x noad_efsens
     atmens_noad_efsens.csh $EXPID $nymdb $nhmsb $taub $aver $iniwrt
     if ($status) then
        echo "atmens_noad_efsens failed"
        exit(1)
     endif
     zeit_co.x noad_efsens
  endif

# Run ensemble of adjoint atmospheric GCMs
# ----------------------------------------
  if( $RUN_AENSADFC ) then
     zeit_ci.x efsens
     atmens_efsens.csh $EXPID $nymdb $nhmsb $taub $aver
     if ($status) then
        echo "atmens_efsens failed"
        exit(1)
     endif
     zeit_co.x efsens
  endif

# Calculate statitics (mean and possibly rms) from updated ensemble
# -----------------------------------------------------------------
  set arch_nymd = $nymdb
  set arch_nhms = $nhmsb
# if( $RUN_POSTFCST || $DO_ATM_ENS ) then
#     zeit_ci.x post_egcm
#     post_egcm.csh $EXPID $nymdb $nhmsb $TIMEINC $FVWORK/updated_ens
#     if ($status) then
#        echo "post_egcm failed"
#        exit(1)
#     endif
#     zeit_co.x post_egcm
# endif

# Store updated ensemble for archiving and prepare for next cycle
# ---------------------------------------------------------------
  if( $DO_ATM_ENS ) then
    cd $FVWORK/updated_ens/
    /bin/rm mem0*/*.bin  # _RT: for now, make sure to remove RSTs before setting for archive
    if ( -e ensmean ) /bin/mv ensmean ensmeanf
    if ( -e ensrms  ) /bin/mv ensrms  ensrmsf
    cd -
    /bin/mv $FVWORK/updated_ens  $ATMENS4ARCH/atmgeps4arch.${nymdb}_${hhb}
  endif
 
# Summarize timings
# -----------------
  zeit_pr.x
  /bin/cp .zeit $ATMENS4ARCH/atmgeps4arch.${nymdb}_${hhb}/$EXPID.atmens_geps_zeit.log.${nymdb}_${hhb}z.txt 

# Submit job to run DAS
# ---------------------
  if( $DO_ATM_ENS ) then
#    If made it here, all is good, then remove the working directory
#    ---------------------------------------------------------------
     /bin/rm -rf $FVWORK
     /bin/rm     $FVHOME/.ENSWORK
  endif

# Self re-submit in case ensemble-only
# ------------------------------------
  if ( $DO_ATM_ENS ) then

      cd $FVHOME/run/ageps
      /bin/mv running.$lstcases[1] done.$lstcases[1]
      set lstcases = `/bin/ls -1 standalone_ageps.*`
      if (! $status ) then
         $ATMENS_BATCHSUB atm_ens_geps.j
      endif

  endif

# Archive ensemble
# ----------------
  if ( $RUN_ARCHATMENS || $DO_ATM_ENS ) then
      set arch_hh = `echo $arch_nhms | cut -c1-2`
      atmens_arch.csh $EXPID $arch_nymd $arch_nhms $FVHOME/run/atmens/atmens_storage.arc gaeps atmgeps4arch.${nymdb}_${hhb} |& tee -a atm_ens_arch.${arch_nymd}_${arch_hh}z.log
  endif

