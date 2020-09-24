#!/bin/csh -fx
# ------------------------------
#SBATCH --account=>>>GID<<<
#SBATCH --constraint=>>>NODEFLG<<<
#SBATCH --ntasks=96
#SBATCH --ntasks-per-node=24
#SBATCH --time=6:00:00
#
#SBATCH --job-name=atm_ose
#SBATCH --output=atm_ose.log.o%j
#PBS -N atm_ose
#PBS -o atm_ose.log.o%j
#PBS -l walltime=6:00:00
#PBS -S /bin/csh
#PBS -j eo
# ------------------------------
#
# ATM_OSE driver script.
#
#  This scripts implements a driver for an OSE-type workflow relying on
#  the ensemble DAS GEOS machinery. Users should be advised that the
#  here is one of a particular type. At most only a hybrid reply to 
#  an existing ensemble can be exercised. A true OSE for a hybrid system
#  would run the full hybrid system (deterministic and ensemble DA) for
#  each member the OSE. The way to do this w/ the GEOS Hybrid system is
#  to simply run multiple independent experiments. The OSE implemented
#  here is of a particular kind, whose usefulness for the hybrid system
#  remains to be determined. (R. Todling, 22 May 2020)
#
#--------------------------------------------------------------------
 set myname = `basename $0`

# Source Run Time Environment settings
# ------------------------------------
  setenv EXPID   >>>EXPID<<<   # experiment ID
  setenv FVHOME  >>>FVHOME<<<  # experiment home directory
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
  setenv JOBGEN_CONSTRAINT >>>NODEFLG<<<
  setenv GID >>>GID<<<
  setenv group_list "SBATCH -A $GID"
  setenv ARCH `uname -s`
  setenv HOST `uname -n`
  setenv CASE    $EXPID  # experiment ID (for LSM's sake)
  setenv FVROOT  `cat $FVHOME/.FVROOT`
  setenv FVRUN   $FVHOME/run
  setenv BIGNAME `echo "$EXPID" | tr -s '[:lower:]' '[:upper:]'`
  if( (`uname -s` == "Linux") ) then
      if( `uname -m` != "ia64" ) then
         setenv FORT90L -Wl,-T
      endif
      setenv FVWORK $FVHOME/../osework.$BIGNAME
      if ($?kidwork) then  # this case, overwrite FVWORK with user-specific
         setenv FVWORK $FVHOME/../$kidwork
      endif
  endif
  /bin/mkdir -p $FVWORK            # create working directory
  echo $FVWORK >! $FVHOME/.OSEWORK  # record working directory

  setenv FVBCS $FVHOME/fvInput
  setenv EXTDATA $FVWORK/ExtData  # External data directory
  /bin/mkdir -p $EXTDATA
  /bin/touch $EXTDATA/.no_archiving
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

# A bit sad that this needs to be here instead of set in the env
# --------------------------------------------------------------
  if ( -e /etc/os-release ) then
    setenv PERL5LIB /usr/lib/perl5/5.18.2/CPAN
  endif

# MPI/OpenMP Hybrid Options
# -------------------------
  if ( -e $FVHOME/run/atmose/mkiau.rc.tmpl ) then
     set IAUX = `which mkiau.x`
  else
     set IAUX = `which makeiau.x`
  endif
  set ANAX    = `which GSIsa.x`
  set SACX    = `which sac.x`

  if ($?I_MPI_ROOT) then
     setenv ATMENS_MPIRUN "mpirun "
  else
     setenv ATMENS_MPIRUN "mpiexec_mpt "
  endif

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
  setenv VAROFFSET 180    # abs value of time off from 1st synoptic hour of var window
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
  source $FVHOME/run/atmose/AtmOSEConfig.csh

  if ( !($?ATMENS_BATCHSUB) ) then
     echo "atm_ose.j: missing batch sub command"
     exit (1)
  endif
# The following release any settings by the central ADAS
# related to overwritting the climatological error cov;
# Berr-Clim plays no role in the ensemble, but unfortunately
# GSI observer still refers to it; in some cases, the setting
# of these variables will require more doing than needed.
  if ( $?BERROR_FROMENS ) then
     unsetenv BERROR_FROMENS
  endif
  if ( $?BERROR ) then
     unsetenv BERROR
  endif
  setenv HYBRIDGSI     $ATMENSLOC
  setenv  DO_ATM_ENS    1
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
            setenv  RUN_EAANA     1
         else
            setenv  RUN_EAANA     0
         endif
         setenv  RUN_EAAOD      0
         setenv  RUN_PEANA      0
         setenv  RUN_ENS2GCM    0
         setenv  RUN_AENSFCST   0
         setenv  RUN_AENSVTRACK 0
         setenv  RUN_POSTEGCM   0
         setenv  RUN_ARCHATMENS 0
  # To trigger ensemble-only set following dates to anything but 0 (as yyyymmddhh)
  #setenv ENSONLY_BEG 2011111221
  #setenv ENSONLY_END 2011120321
  setenv ENSONLY_BEG 0
  setenv ENSONLY_END 0

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

# Check for Variational Aircraft Bias Correction
# ----------------------------------------------
  if ( (! $PREPQC ) && $ACFTBIAS == 2 ) then
     set check_acfb = `echo $OBSCLASS | grep ncep_acftpfl_bufr`
     if ($status) then
        echo "Required ncep_acftpfl_bufr missing from OBSCLASS"
        echo "Cannot perform Variational Aircraft Bias Correction"
        echo "Aborting ..."
        exit 1
     endif
  endif
# Get date/time from restarts
# ---------------------------
  if ( $ENSONLY_BEG == $ENSONLY_END ) then
     set nymdb  = `echo $RSTSTAGE4AENS/$EXPID.rst.lcv.????????_??z.bin | cut -d. -f4 | cut -c1-8`
     set hhb    = `echo $RSTSTAGE4AENS/$EXPID.rst.lcv.????????_??z.bin | cut -d. -f4 | cut -c10-11`
  else
     set nymdb  = `echo $ENSONLY_BEG | cut -c1-8`
     set hhb    = `echo $ENSONLY_BEG | cut -c9-10`
  endif
  set ddb      = `echo $nymdb | cut -c7-8`
  set nhmsb    = ${hhb}0000
  set yyyymmddhh = ${nymdb}${hhb}
  @ anafreq_sc = $TIMEINC * 60

# Define prefix and suffix of job names
  setenv JOBGEN_PFXNAME e1
  setenv JOBGEN_SFXNAME ${ddb}_${hhb}z
  if ( $?PBS_JOBID ) then
     qalter -N ${JOBGEN_PFXNAME}_atm_ose_${JOBGEN_SFXNAME} $PBS_JOBID
  else
     if ( $?SLURM_JOBID ) then
         qalter -N ${JOBGEN_PFXNAME}_atm_ose_${JOBGEN_SFXNAME} $SLURM_JOBID
     else
         echo "no JOBID found, abort"
         exit(1)
     endif
  endif

# Calculate corresponding analysis time (need care for 4dvar)
# -------------------------------------
  @ varoffset_sc = $VAROFFSET * 60
  set anadate  = `tick $nymdb $nhmsb $varoffset_sc` 
  set anymd    = $anadate[1]
  set anhms    = $anadate[2]

  set members = `ls -d $HYBRIDGSI/mem* | wc`
  set nmem = $members[1]

# Get positioned in work area
# ---------------------------
  cd $FVWORK
  if ( -e .FAILED ) then
      echo " Main: clearing past FAILURE "
      /bin/rm .FAILED
  endif

# When applicable, launch forecast for desired time
# -------------------------------------------------
  if ( ($?FCSTIMES) ) then
     set crstag = ${nymdb}_${hhb}z
     foreach fctime ( $FCSTIMES )
        set fcstdtag = {$nymdb}_${fctime}z
        if ( $fcstdtag == $crstag ) then
            cd $FVHOME/fcst
            echo "launching forecast for: $FVHOME/fcst/forecast.$fcstdtag"
            touch forecast.$fcstdtag
            $ATMENS_BATCHSUB g5fcst.j
            cd -
        endif
     end
  endif

# Run AOD analysis (or proxy of that)
# NOTE: wired time frequency for now
# -----------------------------------
  if ( $GAAS_ANA ) then
      if ( $RUN_EAAOD || $DO_ATM_ENS ) then
         zeit_ci.x eaod
         atmos_eaod.csh $EXPID $anymd $anhms 030000 |& tee -a atm_ose.log
         if( $status) then
            echo "eaod failed"
            exit(1)
         endif
         zeit_co.x eaod
      endif
  endif
 
# Run ensemble of atmospheric analyses
# ------------------------------------
  if ( $RUN_EAANA || $DO_ATM_ENS ) then
      zeit_ci.x eana
      atmos_eana.csh $EXPID $anymd $anhms |& tee -a atm_ose.log
      if( $status) then
         echo "eana failed"
         exit(1)
      endif
      zeit_co.x eana
      if( -e $FVHOME/run/${EXPID}_scheduler.j ) then
         touch $FVHOME/.DONE_MEM001_atm_ose_eana.${yyyymmddhh}
      endif
  endif

# Calculate IAU-related forcing terms
# -----------------------------------
  if ( $RUN_ENS2GCM || $DO_ATM_ENS ) then
      zeit_ci.x ens2gcm
      if ( $ATMENS_DO4DIAU ) then
         atmos_ens2gcm.csh $EXPID $nymdb $nhmsb |& tee -a atm_ose.log
      else
         atmos_ens2gcm.csh $EXPID $anymd $anhms |& tee -a atm_ose.log
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
      if ( -e $FVHOME/atmens/ensmean/$EXPID.bkg.eta.${nymdb}_${hhb}00z.$NCSUFFIX ) then
         zeit_ci.x gcm_ens
         set ens_mres  = `getgfiodim.x $FVHOME/atmens/ensmean/$EXPID.bkg.eta.${nymdb}_${hhb}00z.$NCSUFFIX`
         set ens_nlons = $ens_mres[1]
         set ens_nlats = $ens_mres[2]
         @ tfcst_hh  = 2 * $TIMEINC / 60
         gcm_ensemble.csh $EXPID $nymdb $nhmsb $tfcst_hh $ens_nlons $ens_nlats |& tee -a atm_ose.log
         if( $status) then
            echo "gcm_ensemble failed"
            exit(1)
         endif
         zeit_co.x gcm_ens
      else
         echo "cannot find $ATMENSLOC/ensmean/$EXPID.bkg.eta.${nymdb}_${hhb}00z.$NCSUFFIX "
         echo "gcm_ensemble unable to run"
         exit(1)
      endif
  endif

# Run vortex track on forecast members
# ------------------------------------
  if( $RUN_AENSVTRACK || $DO_ATM_ENS ) then
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

# Store updated ensemble for archiving and prepare for next cycle
# ---------------------------------------------------------------
  if( $DO_ATM_ENS ) then
    /bin/mv $ATMENSLOC           $FVHOME/atmose4arch.${nymdb}_${hhb}
    /bin/mv $FVWORK/updated_ens  $ATMENSLOC
  endif
 
# Calculate statitics (mean and possibly rms) from updated ensemble
# -----------------------------------------------------------------
  set arch_nymd = $nymdb
  set arch_nhms = $nhmsb
  if( ($RUN_POSTEGCM && $RUN_AENSFCST) || $DO_ATM_ENS ) then
      zeit_ci.x post_egcm
      post_egcm.csh $EXPID $nymdb $nhmsb $TIMEINC $FVHOME/atmens
      if ($status) then
         echo "post_egcm failed"
         exit(1)
      endif
      zeit_co.x post_egcm
  endif

# Summarize timings
# -----------------
  zeit_pr.x
  if ( $DO_ATM_ENS ) then
     /bin/cp .zeit $ATMENSLOC/atmose4arch.${nymdb}_${hhb}/$EXPID.atmose_zeit.log.${nymdb}_${hhb}z.txt 
  endif

# Submit job to run DAS
# ---------------------
  if( $DO_ATM_ENS ) then
     if ( ! ($ENSONLY_BEG && $ENSONLY_END) ) then
        cd $FVHOME/run
        if( -e ${EXPID}_scheduler.j ) then
           touch $FVHOME/.DONE_MEM001_atm_ose.${yyyymmddhh}
        else
           #$ATMENS_BATCHSUB >>>JOBNJ<<<
           sbatch >>>JOBNJ<<<
        endif
     endif

#    If made it here, all is good, then remove the working directory
#    ---------------------------------------------------------------
     /bin/rm -rf $FVWORK
     /bin/rm     $FVHOME/.OSEWORK
  endif

# Self re-submit in case ensemble-only
# ------------------------------------
  if ( $DO_ATM_ENS && $ENSONLY_BEG && $ENSONLY_END ) then

      if (! -d $RSTSTAGE4AENS ) mkdir -p $RSTSTAGE4AENS
      cd $RSTSTAGE4AENS
      @ tsec = $TIMEINC * 60
      /bin/rm $EXPID.rst.lcv.${nymdb}_${hhb}z.bin
      set nextseg  = `tick $nymdb ${hhb}0000 $tsec` 
      set nymdb    = `echo $nextseg[1]`
      set hhb      = `echo $nextseg[2] | cut -c1-2`
      mkdrstdate.x $nextseg
      /bin/mv d_rst $EXPID.rst.lcv.${nymdb}_${hhb}z.bin
      cd $FVHOME/run
      $ATMENS_BATCHSUB atm_ose.j

  endif

# Archive ensemble
# ----------------
  if ( $RUN_ARCHATMENS || $DO_ATM_ENS ) then
      set arch_hh = `echo $arch_nhms | cut -c1-2`
      atmens_arch.csh $EXPID $arch_nymd $arch_nhms $FVHOME/run/atmens/atmens_storage.arc eadas atmose4arch.${nymdb}_${hhb} |& tee -a atm_ose_arch.${arch_nymd}_${arch_hh}z.log
  endif

