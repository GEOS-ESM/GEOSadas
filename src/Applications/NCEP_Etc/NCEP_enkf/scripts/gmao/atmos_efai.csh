#!/bin/csh

# atmos_efai - Implements simple version of Flexible Additive Inflation
#              of Sommer & Janjic (2017).
#
# !REVISION HISTORY:
#
#  29Nov2017  Todling   Initial version
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv DRYRUN #echo

# local env vars (not to become global)
setenv MYNAME atmos_efai.csh

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - Augment Ensemble using an FAI approach"
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
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of ensemble resource files   "
   echo "    FAI_FACTOR    - flexible additive inflation factor "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - location of work directory        "
   echo " "
   echo " SEE ALSO"
   echo " "
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 29Nov2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENSETC)      ) setenv FAILED 1
if ( !($?FAI_FACTOR)     ) setenv FAILED 1
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1

if ( !($?NCSUFFIX)       ) setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3
set hh     = `echo $nhms | cut -c1-2`
set hhmn   = `echo $nhms | cut -c1-4`
set yyyymmddhh = ${nymd}${hhmn}

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhhmn ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

if (($?ASYNBKG)) then
   @ bkgfreq_hr  =  $ASYNBKG / 60
   @ bkgfreq_mn  =  $ASYNBKG - $bkgfreq_hr * 60
   set bkgfreq_hh = `echo $bkgfreq_hr |awk '{printf "%02d", $1}'`
   set bkgfreq_mm = `echo $bkgfreq_mn |awk '{printf "%02d", $1}'`
   set bkgfreq_hhmn = ${bkgfreq_hh}${bkgfreq_mm}
else
   @ bkgfreq_hhmn = 0300
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

if ( ! -d $ENSWORK/addperts ) then
   echo " ${MYNAME}: cannot find directory with NMC-like perturbations"
   exit(1)
endif
cd $ENSWORK/addperts

if (! -e $expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX ) then
  if ( -e $ENSWORK/ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX ) then
    /bin/cp $ENSWORK/ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX .
  else
     echo " ${MYNAME}: cannot find ensemble mean background file"
     exit(1)
  endif
endif

# The following is not general at this point, but should do it
# ------------------------------------------------------------
set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]
@ ic = $nmem
@ jc = 0
foreach fn (`/bin/ls $expid.nmcpert*.$NCSUFFIX`)
  @ ic++
  set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
  if(! -d mem$memtag ) mkdir mem$memtag
  if ( -e mem$memtag/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX ) then
     @ jc++
  else
     $DRYRUN dyn_recenter.x -g5 -damp -a $FAI_FACTOR -inflate $fn \
                            -o mem$memtag/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX \
                            $expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX \
                            NONE NONE
     if ( $status ) then
        echo " ${MYNAME}: failed to generated augmented member $ic, aborting ..."
        exit(1)
     else
        @ jc++
     endif
  endif
end

# Calculte stats of flexible ensemble
# -----------------------------------
#  this will serve to allow tuning spread of "model error" component
if ( ($?AENSTAT_MPIRUN) ) then
   if ( -e $ATMENSETC/mp_stats.rc ) then
     if(! -e .MP_STATS_EGRESS_bkg.eta_${nymd}${hhmn} ) then
        if (! -d ensmean ) mkdir ensmean
        if (! -d ensrms  ) mkdir ensrms
        $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc \
                         -o ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX \
                       -stdv ensrms/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX \
                        -ene ensrms/$expid.bene.err.${nymd}_${hhmn}z.$NCSUFFIX \
                        -inc ${bkgfreq_hhmn}00 \
                        -egress .MP_STATS_EGRESS_bkg.eta_${nymd}${hhmn} \
                        mem*/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX 
     endif
   endif
endif

# ---------------------------------
# if made it here, then all is done
# ---------------------------------
if ( $jc == $nmem ) then
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhhmn
   echo " ${MYNAME}: Complete "
   exit(0)
else
   echo " ${MYNAME}: Failed to generated full augmented background set "
   exit(1)
endif
