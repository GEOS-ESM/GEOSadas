#!/bin/csh

# atmens_addinflation.csh - apply adaptive inflation
#
# !REVISION HISTORY:
#
#  07Mar2012  Todling   Initial script
#  09Apr2018  Todling   Actual activation and content
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#  23Jun2020  Todling   Redef meaning of ATMENSLOC
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmens_addinflation.csh

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - adaptive (vertically-dependent) additive inflation"
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
   echo "    This procedure is responsible for estimating vertically-"
   echo "  dependent adaptive inflation parameters"
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of ensemble RC files     "
   echo "    ATMENSLOC     - location of background ensemble   "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - location of work directory        "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    NCSUFFIX      - suffix of hdf/netcdf files (default: nc4)"
   echo " "
   echo " SEE ALSO"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 09Apr2018      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENSETC)      ) setenv FAILED 1
if ( !($?ATMENSLOC)      ) setenv FAILED 1
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1

if ( !($?NCSUFFIX)         ) setenv NCSUFFIX nc4


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

if ( ! -e $ATMENSETC/dyn_recenter.rc ) then
   echo " ${MYNAME}: not all RC avail; cannot estimate adaptive inflation ..."
   echo " ${MYNAME}: will exit without error, this better not be required! ..."
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
   exit(0)
endif

# Get positioned inside ENSWORK
# -----------------------------
cd  $ENSWORK
touch .no_archiving

# Calculate obs impact on analysis
# --------------------------------
  set spread_bkg = $ATMENSLOC/ensrms/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX
  set mean_bkg   = $ATMENSLOC/ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX
  if( -e $ATMENSETC/easyeana.rc ) then
     set mean_ana   = updated_ens/mem001/$expid.ana.eta.${nymd}_${hhmn}z.$NCSUFFIX
  else
     set mean_ana   = updated_ens/ensmean/$expid.ana.eta.${nymd}_${hhmn}z.$NCSUFFIX
  endif
  set ofile      = $expid.add_infl.${nymd}_${hhmn}z.txt
  if( -e $mean_bkg && -e $mean_ana && -e $spread_bkg ) then
     if ( -e $ATMENSLOC/$expid.add_inf_rst.txt ) then
        set myrc = $ATMENSLOC/$expid.add_inf_rst.txt
     else
        set myrc = $ATMENSETC/dyn_recenter.rc  # cold start
     endif
     dyn_inflate.x -o $ofile -rc $myrc $mean_bkg $mean_ana $spread_bkg
     if ( $status ) then
          echo " ${MYNAME}: error running obs dyn_inflate.x, aborting ..."
          exit(2)
     endif
  else
     echo " ${MYNAME}: missing files, cannot estimate additive infl parameters, aborting ..."
     exit(1)
  endif
  /bin/cp $expid.add_infl.${nymd}_${hhmn}z.txt $ATMENSLOC
  /bin/cp $expid.add_infl.${nymd}_${hhmn}z.txt updated_ens/$expid.add_inf_rst.txt

touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
exit(0)
