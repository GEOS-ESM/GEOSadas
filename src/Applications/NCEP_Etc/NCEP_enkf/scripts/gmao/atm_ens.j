#!/bin/csh -fx
# ------------------------------
#SBATCH --account=>>>GID<<<
#SBATCH --constraint=>>>NODEFLG<<<
#SBATCH --ntasks=96
#SBATCH --ntasks-per-node=24
#SBATCH --time=6:00:00
#
#SBATCH --job-name=atm_ens
#SBATCH --output=atm_ens.log.o%j
#PBS -N atm_ens
#PBS -o atm_ens.log.o%j
#PBS -l walltime=6:00:00
#PBS -S /bin/csh
#PBS -j eo
# ------------------------------
#
# EnADAS driver script.
#
# This file has been automatically generated by fvsetup.
#

#--------------------------------------------------------------------
 set myname = `basename $0`

# Source Run Time Environment settings
# ------------------------------------
  setenv EXPID   >>>EXPID<<<   # experiment ID
  setenv FVHOME  >>>FVHOME<<<  # experiment home directory
  if (-e $FVHOME/run/FVDAS_Run_Config) then
      source $FVHOME/run/FVDAS_Run_Config
  else
      echo "atm_ens.j: cannot find $FVHOME/run/FVDAS_Run_Config, aborting ..."
      exit(1)
  endif
# tell which side of the machine we are on ...
  uname -a

  env

#
#                 ----------------------------------
#                  PART I - Prepare the Environment
#                 ----------------------------------

# Experiment environment
# ----------------------
# setenv JOBGEN_PARTITION preops
# setenv JOBGEN_QOS dastest
  setenv JOBGEN_CONSTRAINT >>>NODEFLG<<<
  setenv ATMENS_QNAME compute
  if ( $?JOBGEN_PARTITION ) then
     setenv ATMENS_QNAME $JOBGEN_PARTITION
  endif
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
      setenv FVWORK $FVHOME/../enswork.$BIGNAME
      if ($?kidwork) then  # this case, overwrite FVWORK with user-specific
         setenv FVWORK $FVHOME/../$kidwork
      endif
  endif
  /bin/mkdir -p $FVWORK            # create working directory
  echo $FVWORK >! $FVHOME/.ENSWORK  # record working directory

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
  if ( -e $FVHOME/run/atmens/mkiau.rc.tmpl ) then
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
  source $FVHOME/run/atmens/AtmEnsConfig.csh

  if ( !($?ATMENS_BATCHSUB) ) then
     echo "atm_ens.j: missing batch sub command"
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
  setenv STAGE4HYBGSI  $HYBRIDGSI/central   # set to /dev/null to not recenter
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
            setenv  RUN_PERTS     1
            setenv  RUN_OBVSR     1
            setenv  RUN_EAANA     1
         else
            setenv  RUN_PERTS     0
            setenv  RUN_OBVSR     0
            setenv  RUN_EAANA     0
         endif
         setenv  RUN_EAAOD      0
         setenv  RUN_PEANA      0
         setenv  RUN_ENS2GCM    0
         setenv  RUN_AENSFCST   0
         setenv  RUN_AENSVTRACK 0
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
     qalter -N ${JOBGEN_PFXNAME}_atm_ens_${JOBGEN_SFXNAME} $PBS_JOBID
  else
     if ( $?SLURM_JOBID ) then
         qalter -N ${JOBGEN_PFXNAME}_atm_ens_${JOBGEN_SFXNAME} $SLURM_JOBID
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

# Retrieve existing random perturbations (enables for reproducibility) 
# --------------------------------------------------------------------
  set lst = (`ls $ATMENSLOC/*rndperts.dates*txt` )
  if ( ! $%lst ) then
   if ( -e $ATMENSETC/given_rndperts.acq ) then
     if ( ! -e $FVWORK/.DONE_MEM001_GETRNDPERTS.$yyyymmddhh) then
      if(! -d $STAGE4HYBGSI ) mkdir -p $STAGE4HYBGSI
      set spool = "-s $FVWORK/spool"
      jobgen.pl \
             -q datamove \
             getrndperts         \
             $GID                \
             $OBSVR_WALLCLOCK    \
             "acquire -v -strict -rc $ATMENSETC/given_rndperts.acq -d $STAGE4HYBGSI $spool -ssh $anymd $anhms 060000 1" \
             $STAGE4HYBGSI       \
             $myname             \
             $FVWORK/.DONE_MEM001_GETRNDPERTS.$yyyymmddhh \
             "Main job script Failed for Get Central Analysis"

             if ( -e getrndperts.j ) then
                if ( $ATMENS_BATCHSUB == "sbatch" ) then
                   $ATMENS_BATCHSUB  -W getrndperts.j
                else
                   $ATMENS_BATCHSUB  -W block=true getrndperts.j
                endif
                touch .SUBMITTED
             else
                echo " $myname: Failed for Get Rnd-Perturbations, Aborting ... "
                touch $FVWORK/.FAILED
                exit(1)
             endif
     endif
     set lst = (`ls $STAGE4HYBGSI/*rndperts.dates*txt` )
     if ( $%lst ) then
         if ( $#lst == 1 ) then 
            /bin/mv $STAGE4HYBGSI/*rndperts.dates*txt $ATMENSLOC
         else
            ls $STAGE4HYBGSI/*rndperts.dates*txt
            echo "Main: too many rndperts-dates files found, aborting."
            exit(1)
         endif
     else
         echo "Main: failed in retrieve rndperts-dates file, aborting."
         exit(1)
     endif
   endif
  endif

# In case additive inflation, prepare the perturbations
# -----------------------------------------------------  
  if ( ($DO_ATM_ENS || $RUN_PERTS) && -e $ATMENSETC/nmcperts.rc ) then 
     setperts.csh    ${EXPID} $nmem $anymd $anhms $TIMEINC $AENSADDINFLOC |& tee -a $FVWORK/setperts.log &
     if ($status) then
         echo "Main: failed in setperts.csh, aborting."
         exit(1)
     endif
  endif

# In case central analysis located elsewhere (other than FVHOME/atmens/central)
# ------------------------------------------
  if ( -e $ATMENSETC/central_ana.rc ) then
     if ( ! -e $FVWORK/.DONE_MEM001_GETCENTRAL.$yyyymmddhh) then 
      if(! -d $STAGE4HYBGSI ) mkdir -p $STAGE4HYBGSI
      set spool = "-s $FVWORK/spool"        
      jobgen.pl \
             -q datamove \
             getcentral          \
             $GID                \
             $OBSVR_WALLCLOCK    \
             "acquire -v -strict -rc $ATMENSETC/central_ana.rc  -d $STAGE4HYBGSI $spool -ssh $anymd $anhms 060000 1" \
             $STAGE4HYBGSI       \
             $myname             \
             $FVWORK/.DONE_MEM001_GETCENTRAL.$yyyymmddhh \
             "Main job script Failed for Get Central Analysis"

             if ( -e getcentral.j ) then
                if ( $ATMENS_BATCHSUB == "sbatch" ) then
                   $ATMENS_BATCHSUB  -W getcentral.j
                else
                   $ATMENS_BATCHSUB  -W block=true getcentral.j
                endif
                touch .SUBMITTED
             else
                echo " $myname: Failed for Get Central Analysis, Aborting ... "
                touch $FVWORK/.FAILED
                exit(1)
             endif
     endif
  endif


# In case additive inflation, prepare the perturbations
# -----------------------------------------------------  
# if ( -e $ATMENSETC/nmcperts.rc ) then 
#    set myhour = `echo $anhms | cut -c1-2`
#    if ( -e $STAGE4HYBGSI/${EXPID}.rndperts.dates.${anymd}_${myhour}z.txt) then 
#       /bin/cp $STAGE4HYBGSI/${EXPID}.rndperts.dates.${anymd}_${myhour}z.txt $FVHOME/atmens/ 
#       setperts.csh    ${EXPID} $nmem $anymd $anhms $TIMEINC $AENSADDINFLOC |& tee -a $FVWORK/setperts.log &
#       if ($status) then
#          echo "Main: failed in setperts.csh, aborting."
#          exit(1)
#       endif
#     else
#       echo "Main: did not find rndperts file, aborting "
#       exit(1)
#     endif
# endif

# Run observer for mean and each ensemble member
# ----------------------------------------------
  if ( $RUN_OBVSR || $DO_ATM_ENS ) then
      zeit_ci.x obsvr
      obsvr_ensemble.csh $OBSCLASS $EXPID $anymd $anhms |& tee -a atm_ens.log
      if( $status ) then
         echo "observer failed"
         exit(1)
      endif
      zeit_co.x obsvr
  endif

# SAVING THE CONV DIAGS
# ---------------------
# TO-BE-DONE: generalized
# set yyyy = `echo ${anymd} | cut -c1-4`
# set   mm = `echo ${anymd} | cut -c5-6`
# if(! -e $FVHOME/obs/Y$yyyy/M$mm/$EXPID.diag_conv_${anymd}_${anhms}z.tar ) then
#   cd $FVWORK
#   tar -cvf $FVHOME/obs/Y$yyyy/M$mm/$EXPID.diag_conv_${anymd}_${anhms}z.tar ensmean/*diag_conv* mem*/*diag_conv* 
#   cd -
# endif

# Run AOD analysis (or proxy of that)
# NOTE: wired time frequency for now
# -----------------------------------
  if ( $GAAS_ANA ) then
      if ( $RUN_EAAOD || $DO_ATM_ENS ) then
         zeit_ci.x eaod
         atmos_eaod.csh $EXPID $anymd $anhms 030000 |& tee -a atm_ens.log
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
      atmos_eana.csh $EXPID $anymd $anhms |& tee -a atm_ens.log
      if( $status) then
         echo "eana failed"
         exit(1)
      endif
      zeit_co.x eana
      if( -e $FVHOME/run/${EXPID}_scheduler.j ) then
         touch $FVHOME/.DONE_MEM001_atm_ens_eana.${yyyymmddhh}
      endif

#     SAVING ENKF ANALYSES BEFORE RECENTERING AND INFLATION
#     -----------------------------------------------------
      set amm = `echo ${anymd} | cut -c5-6`
      set ahh = `echo ${anhms} | cut -c1-2`
      cd $FVWORK/updated_ens
      if(! -e $HYBRIDGSI/${EXPID}.atmens_eana_brec.${nymdb}_${hhb}z.tar ) then
         tar -cvf $HYBRIDGSI/${EXPID}.atmens_eana_brec.${nymdb}_${hhb}z.tar mem0*/*.ana.eta*nc4  
      endif
      cd -
  endif

# Apply additive inflation
# ------------------------
  if ( $RUN_PERTS || $DO_ATM_ENS ) then
     if ( $AENS_ADDINFLATION && -e $ATMENSETC/nmcperts.rc ) then
         set ah          = `echo ${anhms} | cut -c1-2` 
         set ayyyymmddhh = ${anymd}${ah}
         jobmonitor.csh 1 setperts.csh $FVWORK $ayyyymmddhh
         wait # for setperts to complete
     endif
  endif

# Calculate post-analysis ensemble statistics
# -------------------------------------------
  if ( $RUN_PEANA || $DO_ATM_ENS ) then
      zeit_ci.x post_eana
      post_eana.csh $EXPID $anymd $anhms |& tee -a atm_ens.log
      if( $status) then
         echo "post_eana failed"
         exit(1)
      endif
      zeit_co.x post_eana

#     Save analyses after recentering
#     -------------------------------
      set amm = `echo ${anymd} | cut -c5-6`
      set ahh = `echo ${anhms} | cut -c1-2`
      cd $FVWORK/updated_ens
      if(! -e $HYBRIDGSI/${EXPID}.atmens_eana_arec.${nymdb}_${hhb}z.tar ) then
         tar -cvf $HYBRIDGSI/${EXPID}.atmens_eana_arec.${nymdb}_${hhb}z.tar mem0*/*.ana.eta*nc4 
      endif
      cd -
  endif

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
      if ( -e $FVHOME/atmens/ensmean/$EXPID.bkg.eta.${nymdb}_${hhb}00z.$NCSUFFIX ) then
         zeit_ci.x gcm_ens
         set ens_mres  = `getgfiodim.x $FVHOME/atmens/ensmean/$EXPID.bkg.eta.${nymdb}_${hhb}00z.$NCSUFFIX`
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
    /bin/mv $ATMENSLOC           $FVHOME/atmens4arch.${nymdb}_${hhb}
    /bin/mv $FVWORK/updated_ens  $ATMENSLOC
  endif
 
# Calculate statitics (mean and possibly rms) from updated ensemble
# -----------------------------------------------------------------
  set arch_nymd = $nymdb
  set arch_nhms = $nhmsb
  if( $RUN_AENSFCST || $DO_ATM_ENS ) then
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
     /bin/cp .zeit $FVHOME/atmens4arch.${nymdb}_${hhb}/$EXPID.atmens_zeit.log.${nymdb}_${hhb}z.txt 
  endif

# Submit job to run DAS
# ---------------------
  if( $DO_ATM_ENS ) then
     if ( ! ($ENSONLY_BEG && $ENSONLY_END) ) then
        cd $FVHOME/run
        if( -e ${EXPID}_scheduler.j ) then
           touch $FVHOME/.DONE_MEM001_atm_ens.${yyyymmddhh}
        else
           #$ATMENS_BATCHSUB >>>JOBNJ<<<
           sbatch >>>JOBNJ<<<
        endif
     endif

#    If made it here, all is good, then remove the working directory
#    ---------------------------------------------------------------
     /bin/rm -rf $FVWORK
     /bin/rm     $FVHOME/.ENSWORK
  endif

# Self re-submit in case ensemble-only
# ------------------------------------
  if ( $DO_ATM_ENS && $ENSONLY_BEG && $ENSONLY_END ) then

      if (! -d $RSTSTAGE4AENS ) mkdir -p $RSTSTAGE4AENS
      mkdir -p $STAGE4HYBGSI
      cd $RSTSTAGE4AENS
      @ tsec = $TIMEINC * 60
      /bin/rm $EXPID.rst.lcv.${nymdb}_${hhb}z.bin
      set nextseg  = `tick $nymdb ${hhb}0000 $tsec` 
      set nymdb    = `echo $nextseg[1]`
      set hhb      = `echo $nextseg[2] | cut -c1-2`
      mkdrstdate.x $nextseg
      /bin/mv d_rst $EXPID.rst.lcv.${nymdb}_${hhb}z.bin
      cd $FVHOME/run
      $ATMENS_BATCHSUB atm_ens.j

  endif

# Archive ensemble
# ----------------
  if ( $RUN_ARCHATMENS || $DO_ATM_ENS ) then
      set arch_hh = `echo $arch_nhms | cut -c1-2`
      atmens_arch.csh $EXPID $arch_nymd $arch_nhms $FVHOME/run/atmens/atmens_storage.arc eadas atmens4arch.${nymdb}_${hhb} |& tee -a atm_ens_arch.${arch_nymd}_${arch_hh}z.log
  endif

