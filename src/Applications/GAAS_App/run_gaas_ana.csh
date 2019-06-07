#!/bin/csh -x

#############################################################################
# run_gaas_ana.csh - retrieve observations for aerosol analysis
#
# !REVISION HISTORY:
#
#  23Jun2016  Todling   Created from Joe Stassi original GEOSdas.csm code
#  03Mar2017  Todling   Consistent w/ GEOSdas.csm in GEOSadas-5_16_5
#  17May2017  Stassi    Modified to call GEOSdas.csm code rather than duplicating it
#
#############################################################################
setenv MYNAME `basename $0`

if ( $#argv < 6 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - run GAAS analysis"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME expid nymd nhms nstep"
   echo " "
   echo " where"
   echo "   expid   -  experiment name"
   echo "   nymd    -  initial date of forecast, as in YYYYMMDD "
   echo "   nhms    -  initial time of forecast, as HHMMSS"
   echo "   nstep   -  THIS WILL BE MADE OBSOLETE"
   echo "   workdir -  directory where work takes place"
   echo "   egressfn-  egress file name"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME d512a 20151118 000000 1 workdir egressfn"
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of resource files        "
   echo "    ATMENSLOC     - location of current ensemble      "
   echo "    ASYNBKG       - frequency of background (minutes) "
   echo "    FVBCS         - location of fvInput               "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    GID           - group ID to run job under         "
   echo "    TIMEINC       - analysis frequency (minutes)      "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "   AODBLOCKJOB        - allows blocking batch jobs and halt "
   echo "                        before proceeding                   "
   echo " "
   echo " REMARKS"
   echo " "
   echo " SEE ALSO"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 24Jun2016      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

# Functions.csh must be somewhere in your path
# --------------------------------------------
  set Functions = ( `which Functions.csh` )
  if ( $status ) then
     echo $0": cannot find required Functions.csh"
     exit 1
  else
     source $Functions[1]
  endif

# When "Verify" is set, non-zero return code from any user defined
# csh Function causes this script to exit with the same return code; 
# see Functions.csh
# -------------------------------------------------------------------
  set Verify
  set Trace
  Echo

# Function definitions
# --------------------
  Use GEOSdas

# Input parameters
# ----------------
set expid = $1
set nymd = $2
set nhms = $3
set nstep = $4
set workdir  = $5
set egressfn = $6

set hh =  `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}

# Required environment variables
# ------------------------------
setenv FAILED 0
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1
if ( !($?MPIRUN_AOD)    ) setenv FAILED 1
if ( $FAILED ) then
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

# Defaults for other environment variables
# ----------------------------------------
if ( !($?AERO_OBSDBRC) )  setenv AERO_OBSDBRC  obsys-gaas.rc
if ( !($?AODBLOCKJOB)  )  setenv AODBLOCKJOB   1
if ( !($?CHECK_DMF)    )  setenv CHECK_DMF     1
if ( !($?DATA_QUEUE)   )  setenv DATA_QUEUE    datamove
if ( !($?DO_DMGET)     )  setenv DO_DMGET      1
if ( !($?DO4DVAR)      )  setenv DO4DVAR       0
if ( !($?GAAS_ANA)     )  setenv GAAS_ANA      1
if ( !($?IGNORE_0)     )  setenv IGNORE_0      0
if ( !($?MODIS_L2_HDF) )  setenv MODIS_L2_HDF  0
if ( !($?NCPUS_AOD)    )  setenv NCPUS_AOD     1
if ( !($?PBS_BIN)      )  setenv PBS_BIN       "/usr/slurm/bin"
if ( !($?group_list)   )  setenv group_list    "SBATCH -A $GID"

if ( $?FVSPOOL ) then
   set spool = $FVSPOOL
else
   set diren = `dirname $FVHOME`
   set spool = $diren/spool
endif

# Work directory
# --------------
setenv FVWORK $workdir
if ( -e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: Already done."
   exit(0)
endif
cd $FVWORK

# Aliases needed in GEOSdas.csm
# -----------------------------
alias fname1 'echo \!* >! $fname'   # write first line of $fname
alias fname2 'echo \!* >> $fname'   # append to $fname

# Variables needed in GEOSdas.csm (passed globally)
# -------------------------------------------------
@ numhrs = $nstep * 6
set aod_parallel_flag = 0
set bnhms = $nhms
set bnymd = $nymd
set data_queue = $DATA_QUEUE
set gaasDateBeg = $nymd
set gaasTimeBeg = $nhms
set onhms = 060000 # frequency of observation files (we pack files in 6-hr intervals)

# Call GEOSdas.csm functions
# --------------------------
set look_ahead = 0
unsetenv obsclass  # do not acquire non-GAAS obs
#Call SubmitAcquireObsJobs_( $look_ahead )
Call AerosolAnalysis_()

# If made it here, all good
# -------------------------
# Note: $AODWORK is defined in AerosolAnalysis_()
# -----------------------------------------------
if ( $egressfn == "DEFAULT" ) then
  echo " ${MYNAME}: Complete "
  touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
else
  if ( -e $AODWORK/ANAAOD_EGRESS ) then
     echo " ${MYNAME}: Complete "
     touch $egressfn
  else
     exit (1)
  endif
endif
exit(0)

