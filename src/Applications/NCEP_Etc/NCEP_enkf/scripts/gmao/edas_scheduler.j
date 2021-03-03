#!/bin/csh -fx
# ------------------------------
#SBATCH --account=g0613
#
#SBATCH --job-name=scheduler
#SBATCH --output=scheduler.log.o%j
#SBATCH --time=4:00:00
#SBATCH --ntasks=1
#PBS -N scheduler
#PBS -o scheduler.log.o%j
#PBS -l select=1:ncpus=1:mpiprocs=1
#PBS -l walltime=4:00:00
#PBS -S /bin/csh
#PBS -j eo


#--------------------------------------------------------------------
 set myname = `basename $0`

# Source Run Time Environment settings
# ------------------------------------
  setenv DDASJNAME  g5das.j
  setenv EXPID   u000_c72   # experiment ID
  setenv FVHOME  /discover/nobackup/$user/u000_c72  # experiment home directory
  setenv FVROOT  /discover/nobackup/$user/SRC/gb4/Linux  # fvDAS installation root dir
  setenv FVWDIR  /discover/nobackup/$user # root location of work directory
  setenv VAROFFSET 180    # abs value of time off from 1st synoptic hour of var window
  setenv JOBMONITOR_MAXSLEEP_MIN 120
  setenv ATMENS_BATCHSUB "qsub"

  set path = ( . $FVHOME/run $FVROOT/bin $path )

  if ( $?refreshdate ) then
     touch $FVHOME/.REFRESH.$refreshdate
  endif

# Get date/time from restarts
# ---------------------------
  set nymdb    = `echo $FVHOME/recycle/$EXPID.rst.lcv.????????_??z.bin | cut -d. -f4 | cut -c1-8`
  set ddb      = `echo $nymdb | cut -c7-8`
  set hhb      = `echo $FVHOME/recycle/$EXPID.rst.lcv.????????_??z.bin | cut -d. -f4 | cut -c10-11`
  set nhmsb    = ${hhb}0000

# Define prefix and suffix of job names
  setenv JOBGEN_PFXNAME e1
  setenv JOBGEN_SFXNAME ${ddb}_${hhb}z
  if ( $?PBS_JOBID ) then
     qalter -N ${JOBGEN_PFXNAME}_escheduler_${JOBGEN_SFXNAME} $PBS_JOBID
  else
     if ( $?SLURM_JOBID ) then
         qalter -N ${JOBGEN_PFXNAME}_escheduler_${JOBGEN_SFXNAME} $SLURM_JOBID
     else
         echo "no JOBID found, abort"
         exit(1)
     endif
  endif

# Execute scheduler
# -----------------
  edas_scheduler.csh
  if( $status ) then
      echo "scheduler cannot proceed, aborting ..."
      exit 1
  endif

# re-submit this script
# ---------------------
  cd $FVHOME/run
  $ATMENS_BATCHSUB ${EXPID}_scheduler.j
