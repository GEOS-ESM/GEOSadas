#!/bin/csh

# atmos_egsi - run ensemble of GSI analysis
#
# !REVISION HISTORY:
#
#  24Nov2011  Todling   Initial script
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmos_egsi.csh
setenv ENSFGAT 0
setenv ENSMEAN 1
setenv skipTRANSF                 # no transform needed for ana-sensitivity executable
setenv skipSOLVER                 # need to run the analysis sensitivity solver
setenv skipSATBIAS "-skipSATBIAS" # no need to worry about running satellite bias correction

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - run ensemble of GSI analysis"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "  This procedures generates an ensemble of analysis by running GSI"
   echo "  for each individual set of member-backgrounds."
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo "  gsi_mean.rc   - specified parameters to mean observer/analysis"
   echo "  gsi_member.rc - specified parameters to member observer/analysis"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    FVHOME         - location of experiment            "
   echo "    FVROOT         - location of DAS build             "
   echo "    FVWORK         - location of work directory        "
   echo " "
   echo " REMARKS"
   echo "   1) This procedure has not been properly evaluated yet."
   echo " "
   echo " SEE ALSO"
   echo "   atmos_eana.csh - entry-point of ensemble analysis"
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

# Rename each analysis following GMAO naming convension
# -----------------------------------------------------
if ( $ENSFGAT ) then
  echo " ${MYNAME}: FGAT update not implemented at this point"
  exit(1)
else # FGAT
  @ ic = 0
  while ( $ic < $nmem )
     @ ic = $ic + 1
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     /bin/mv mem${memtag}/$expid.ana.eta.${nymd}_${hh}z.$NCSUFFIX  mem${memtag}/$expid.ana.eta.${nymd}_${hh}z.mem$memtag.$NCSUFFIX
     update_ens.csh $expid $memtag ana $ENSWORK/mem${memtag} NULL $NCSUFFIX
  end
  # consistency check:
  if ( $ic != $nmem ) then
    echo " ${MYNAME}: Error, number of ensemble analysis unequal to number of members, aborting ..." 
    exit(1) 
  endif
  @ ic = 0
  setenv FOUNDINC 0
  while ( $ic < $nmem )
     @ ic = $ic + 1
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     /bin/mv  mem${memtag}/$expid.inc.eta.${nymd}_${hh}z.$NCSUFFIX  mem${memtag}/$expid.inc.eta.${nymd}_${hh}z.mem$memtag.$NCSUFFIX
     update_ens.csh $expid $memtag ana $ENSWORK/mem${memtag} NULL $NCSUFFIX
  end
  if( $ic == $nmem ) setenv FOUNDINC 1
endif # FGAT


touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
