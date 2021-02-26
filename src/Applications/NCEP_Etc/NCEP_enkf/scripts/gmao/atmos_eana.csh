#!/bin/csh

# atmos_eana - call desired Ensemble Analysis strategy
#
# !REVISION HISTORY:
#
#  24Nov2011  Todling   Controls underlying ensemble analysis
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmos_eana.csh

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - call desired Ensemble Analysis strategy."
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
   echo "  Current options for ensemble analyses strategies include: "
   echo "        EnKF, EnGSI, and Simplified Ensemble."
   echo "  Only the EnKF and the Simplified Ensemble have been thoroughly"
   echo "  tested and evaluated. The triggers in each case are determined"
   echo "  depending on the presence of the following files in the ATMENSETC"
   echo "  directory:"
   echo "    EnKF:  obs1gsi_mean.rc   "
   echo "           obs1gsi_member.rc "
   echo "           atmos_enkf.nml.tmpl"
   echo "    EnGSI: gsi_mean.rc   "
   echo "           gsi_member.rc "
   echo "    Simplified Ensemble: easyeana.rc"
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of ensemble resource files   "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - location of work directory        "
   echo " "
   echo " SEE ALSO"
   echo " "
   echo "   atmos_enkf.csh - driver for EnKF analysis"
   echo "   atmos_egsi.csh - driver for ensemble of GSI analysis"
   echo "   atmos_eezy.csh - driver for simplified scheme"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENSETC)      ) setenv FAILED 1
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

if ( !($?ATMENS_ADAPTINFLATION)  ) setenv ATMENS_ADAPTINFLATION 0

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

set do_eezy  = 0  # easy ensemble (created by simply adding NMC-perts to central ana)
set do_egsi  = 0  # ensemble of GSIs
set do_enkf  = 0  # ensemble Kalman Filter
set do_letkf = 0  # local ensemble tranform Kalman Filter

if( (-e $ATMENSETC/easyeana.rc) ) set do_eezy = 1
if( (-e $ATMENSETC/gsi_ctrl.rc) ) set do_egsi = 1
if( (-e $ATMENSETC/gsi_mean.rc) && (-e $ATMENSETC/gsi_member.rc) ) set do_egsi = 1
if( (-e $ATMENSETC/obs1gsi_mean.rc) && (-e $ATMENSETC/obs1gsi_member.rc) && (-e $ATMENSETC/atmos_enkf.nml.tmpl) ) set do_enkf = 1

set done_something = 0
if ( $do_eezy ) then
  atmos_eezy.csh  $expid $nymd $nhms  
  if ( $status ) then
       echo " ${MYNAME}: EnEZY failed, aborting ... "
       exit(1)
  else
       set done_something = 1 
  endif
endif

if ( $do_egsi ) then
  atmos_egsi.csh  $expid $nymd $nhms  
  if ( $status ) then
       echo " ${MYNAME}: EnGSI failed, aborting ... "
       exit(1)
  else
       set done_something = 1 
  endif
endif

if ( $do_enkf ) then
  atmos_enkf.csh  $expid $nymd $nhms  
  if ( $status ) then
       echo " ${MYNAME}: EnKF failed, aborting ... "
       exit(1)
  else
       set done_something = 1 
  endif
endif

if ( $do_letkf ) then
   echo " ${MYNAME}: LETKF option not yet available, aborting ... "
   exit(1)
endif

# Estimate vertically varying additive inflation
# ----------------------------------------------
  if ( $ATMENS_ADAPTINFLATION ) then
     atmens_addinflation.csh $expid $nymd $nhms
  endif

#
# if made it here, then all is done
# ---------------------------------
if ( $done_something ) then
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
   echo " ${MYNAME}: Complete "
   exit(0)
else
   echo " ${MYNAME}: Somehow nothing got done! "
   exit(1)
endif
