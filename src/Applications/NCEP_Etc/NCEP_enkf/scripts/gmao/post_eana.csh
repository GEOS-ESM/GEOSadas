#!/bin/csh

# post_eana.csh - invokes post-ensemble-analysis calculations
#                 such as statistics and re-centering 
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  05Feb2012  Todling   Strip from original location
#  07Mar2012  El Akkraoui Remove redundant stats calculation
#  20Oct2012  Todling   Update API of stats script
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME post_eana.csh
setenv ENSFGAT 0
setenv skipTRANSF                 # no transform needed for ana-sensitivity executable
setenv skipSOLVER                 # need to run the analysis sensitivity solver
setenv skipSATBIAS "-skipSATBIAS" # no need to worry about running satellite bias correction

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - post ensemble-analysis calculations"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedure is responsible for calculating the necessary (and"
   echo "  desired) statistics from the ensemble of analysis, and recentering"
   echo "  the ensemble about the central analysis after the ensemble mean "
   echo "   analysis is available."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - location of work directory        "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    NCSUFFIX      - suffix of hdf/netcdf files (default: nc4)"
   echo "    ENSPARALLEL   - when set, runs all ensemble components in parallel"
   echo "                    (default: off)"
   echo "    AENKF_NCPUS   - when parallel ens on, this sets NCPUS for AGCM integration"
   echo " "
   echo " SEE ALSO"
   echo "    atmens_stats.csh  - calculates required/desired statistics from ensemble"
   echo " atmens_recenter.csh  - recenters ensemble (and applies additive inflation)"
   echo "  obsvr_ensfinal.csh  - calculate OmAs from mean analysis"
   echo " atmens_obsimp0hr.csh - calculate OmAs from mean analysis"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1

if ( !($?NCSUFFIX)         ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)      ) setenv ENSPARALLEL 0 


if ( !($?AENSADDINFLOC)  ) then
   if ( $AENS_ADDINFLATION ) then
      echo " ${MYNAME}: additive inflation set on, but location of perts not specified, Aborting ..."
      exit 1
   else
      setenv AENSADDINFLOC NONE
   endif
endif

if ( $ENSPARALLEL ) then
   if ( !($?AENKF_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $AENKF_NCPUS
   endif
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3
set hh     = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]

# Get positioned inside ENSWORK
# -----------------------------
cd  $ENSWORK
touch .no_archiving

# Calculate analysis ensemble mean
# --------------------------------
  atmens_stats.csh $nmem ana.eta $ENSWORK/updated_ens $nymd $nhms
  if ( $status ) then
      echo " ${MYNAME}: error in calculating ensemble mean analysis, aborting ..."
      exit(1)
  endif
  if ( -e $ENSWORK/.FOUNDINC ) then
     atmens_stats.csh $nmem inc.eta $ENSWORK/updated_ens $nymd $nhms
     if ( $status ) then
         echo " ${MYNAME}: error in calculating ensemble mean ana increment, aborting ..."
         exit(1)
     endif
  endif

# Calculate OmA's by running mean observer one final time
# -------------------------------------------------------
  obsvr_ensfinal.csh $expid $nymd $nhms $ENSWORK/updated_ens
  if ( $status ) then
       echo " ${MYNAME}: error running final observer, aborting ..."
       exit(1)
  endif

# Calculate obs impact on analysis
# --------------------------------
  atmens_obsimp0hr.csh $expid $nymd $nhms 
  if ( $status ) then
       echo " ${MYNAME}: error running obs impact on analysis, aborting ..."
       exit(1)
  endif

# Recenter ensemble
# -----------------
  if( $?STAGE4HYBGSI ) then
     if( "$STAGE4HYBGSI" != "/dev/null") then
        atmens_recenter.csh $expid $nymd $nhms ana.eta ana.eta $ENSWORK/updated_ens $STAGE4HYBGSI $ENSWORK/$AENSADDINFLOC
        if ( $status ) then
             echo " ${MYNAME}: error recentering analysis, aborting ..."
             exit(1)
        endif
        if ( -e $ENSWORK/.FOUNDINC ) then
           atmens_recenter.csh $expid $nymd $nhms inc.eta ana.eta $ENSWORK/updated_ens $STAGE4HYBGSI $ENSWORK/$AENSADDINFLOC
           if ( $status ) then
                echo " ${MYNAME}: error recentering increment, aborting ..."
                exit(1)
           endif
        endif
     else
         echo " ${MYNAME}: inadequate location for central analysis, aborting ..."
         exit(1)
     endif
  else
     echo " ${MYNAME}: cannot find location for central analysis, aborting ..."
     exit(1)
  endif

touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
exit(0)
