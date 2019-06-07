#!/bin/csh

# post_efso.csh - invokes post-ensemble-forecast sensitivity to
#                 observations calculations, diagnostics, etc.
#
# !REVISION HISTORY:
#
#    Apr2017  Todling   Initial script
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME post_efso.csh

if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - post ensemble-FSO calculations"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   taub   -  forecast interval EFSO calcualted for (in hours)"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedure is responsible for converting the output of the "
   echo "  EnKF backward integration into ODS and producing whetever other "
   echo "  diagnostic and statistic desired from the EFSO procedure. "
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
   echo " "
   echo " SEE ALSO"
   echo "   atmens_obsimp0hr.csh - calculate OmAs from mean analysis"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 13Apr2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1
if ( !($?VAROFFSET)      ) setenv FAILED 1

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
set taub  = $4
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

# Initial forecast time
# ---------------------
  @ offset_sec = 60 * $VAROFFSET
  set date0 = (`tick $nymd $nhms -$offset_sec`)
  set nymd0 = $date0[1]
  set nhms0 = $date0[2]
  set hh0   = `echo $nhms0 | cut -c1-2`

# Final forecast time
# -------------------
  @ taub_sec = 3600 * $taub
  set dateF = (`tick $nymd $nhms -$taub_sec`)
  set nymdF = $dateF[1]
  set nhmsF = $dateF[2]
  set hhF   = `echo $nhmsF | cut -c1-2`

# Convert output of EnKF to ODS
# -----------------------------
  if ( -e osense_${yyyymmddhh}.dat ) then
      foreach norm ( "txe" "twe" )
         enkf_obs2ods.x -norm $norm -o $expid.eimp3_${norm}.${nymd0}_${hh0}z+${nymdF}_${hhF}z-${nymd}_${hh}z.ods osense_${yyyymmddhh}.dat
         if ( $status ) then
              echo " ${MYNAME}: error running EnKF2ODS converted, aborting ..."
              exit(1)
         endif
         obimp_summary.pl -ktsummary -type eimp3_${norm} $nymd ${hh}0000
         /bin/cp $expid.eimp3_${norm}.${nymd0}_${hh0}z+${nymdF}_${hhF}z-${nymd}_${hh}z.ods $ENSWORK/updated_ens
      end
  else
     echo " ${MYNAME}: cannot find output osens file from EnKF, aborting ..."
     exit(1)
  endif

touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
exit(0)
