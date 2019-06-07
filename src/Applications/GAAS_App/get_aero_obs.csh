#!/bin/csh -x

#############################################################################
# get_aero_obs.csh - retrieve observations for aerosol analysis
#
# !REVISION HISTORY:
#
#  23Jun2016  Todling   Created from Joe Stassi original GEOSdas.csm code
#  13Aug2017  Todling   Merge w/ latest from Joe; but now too much entanglement
#
#############################################################################

setenv MYNAME get_aero_obs.csh

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - retrieve observations for AEROSOL analysis "
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  nymd nhms nstep"
   echo " "
   echo " where"
   echo "   nymd   -  initial date of forecast, as in YYYYMMDD "
   echo "   nhms   -  initial time of forecast, as HHMMSS"
   echo "   nstep  -  number of times to  in bkg (im) (for history)"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME  20151118 000000 1 "
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo "   obsys-gaas.rc    - database of aerosols observations"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of resource files        "
   echo "    ATMENSLOC     - location of current ensemble      "
   echo "    ASYNBKG       - frequency of background (minutes) "
   echo "    FVBCS         - location of fvInput               "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - location of work directory        "
   echo "    GID           - group ID to run job under         "
   echo "    TIMEINC       - analysis frequency (minutes)      "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
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
set nymd   = $1
set nhms   = $2
set nstep  = $3
#set expid = $1
#set nymd = $2
#set nhms = $3
#set nstep = $4
#set workdir  = $5
#set egressfn = $6

set hh =  `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}

# Required environment variables
# ------------------------------
setenv FAILED 0
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
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
Call SubmitAcquireObsJobs_( $look_ahead )

# if made it here, should be ok
# -----------------------------
touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
