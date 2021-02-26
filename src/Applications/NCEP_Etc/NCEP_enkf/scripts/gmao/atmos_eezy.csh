#!/bin/csh

# atmos_eezy - create ensemble of analyses by copying and interpolating
#              central analysis to make up each member. Notice this only
#              makes sense when the inflation is on and perturbations
#              will be added to these analyses to then construct a
#              legitimate ensemble set.
#
# !REVISION HISTORY:
#
#  01Oct2012  Todling   Initial script
#  12Mar2013  Todling   Special handle of bkg for single (non-dual) resolution
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#  22Jun2020  Todling   Redef meaning of ATMENSLOC
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmos_eezy.csh
setenv ENSMEAN 1

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - create ensemble from central analysis"
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
   echo "   This is the driver for the Simplified Ensemble Scheme. In "
   echo " this procedure, an ensemble of analysis is created by simply"
   echo " perturbing the central analysis with scaled NMC-like 48-24 hour"
   echo " perturbations. The resolution of the ensemble is defined in the "
   echo " resource file easyeana.rc."
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo "  easyeana.rc - specified parameters to simplified scheme"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC      - location of ensemble resource files "
   echo "    ATMENSLOC      - location of ensemble, usually FVHOME"
   echo "    FVHOME         - location of experiment            "
   echo "    FVROOT         - location of DAS build             "
   echo "    FVWORK         - location of work directory        "
   echo "    STAGE4HYBGSI   - location of where central analysis resides"
   echo " "
   echo " REMARKS"
   echo " "
   echo "  1. This option is triggered by the presence of easyeana.rc in ATMENSETC"
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
if ( !($?ATMENSETC)      ) setenv FAILED 1
if ( !($?ATMENSLOC)      ) setenv FAILED 1
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1
if ( !($?STAGE4HYBGSI)   ) setenv FAILED 1

if ( !($?ENSPARALLEL)      ) setenv ENSPARALLEL 0 

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined "
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

# Copy of directories from recycle locally
# ----------------------------------------
if (! -d $ATMENSLOC ) then
  echo "${MYNAME}: cannot find $ATMENSLOC, aboring ..."
  exit(1)
endif
set ensloc = $ATMENSLOC

set members = `/bin/ls -d $ensloc/mem* | wc`
set nmem = $members[1]

# Get positioned inside ENSWORK
# -----------------------------
cd  $ENSWORK
touch .no_archiving

# Initial check
# -------------
if (! -e $ATMENSETC/easyeana.rc ) then
   echo " ${MYNAME}: cannot proceed without easyeana.rc "
   exit(1)
endif
 
# Just in case, create ensemble-mean and rms directories
# ------------------------------------------------------
mkdir -p $ENSWORK/updated_ens
if(! -d ensmean ) then
   mkdir ensmean
   /bin/cp $ensloc/ensmean/*.$NCSUFFIX ensmean/
endif

# Just in case, interpolate central analysis to resolution of ensemble
# --------------------------------------------------------------------
set eres = `echorc.x -rc $ATMENSETC/easyeana.rc ensemble_resolution`
set cres = `echorc.x -rc $ATMENSETC/easyeana.rc central_das_resolution`
dyn2dyn.x -g5 -res $eres -o ensmean/easy.ana.eta.${nymd}_${hhmn}z.$NCSUFFIX $STAGE4HYBGSI/$expid.ana.eta.${nymd}_${hhmn}z.$NCSUFFIX
if ($status) then
   echo " ${MYNAME}: trouble converting central analysis into fake analysis members "
   exit(1)
endif

# Rename each analysis following GMAO naming convension
# -----------------------------------------------------
@ ic = 0
while ( $ic < $nmem )
   @ ic = $ic + 1
   set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
   if(! -d mem${memtag} ) then
     mkdir mem${memtag}
     /bin/cp $ensloc/mem${memtag}/*.$NCSUFFIX mem${memtag}/ 
   endif
   /bin/cp ensmean/easy.ana.eta.${nymd}_${hhmn}z.$NCSUFFIX  mem${memtag}/$expid.ana.eta.${nymd}_${hhmn}z.mem$memtag.$NCSUFFIX &
end
wait
@ ic = 0
while ( $ic < $nmem )
   @ ic = $ic + 1
   set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
   update_ens.csh $expid mem$memtag ana $ENSWORK/mem${memtag} NULL $NCSUFFIX
end
# consistency check:
if ( $ic != $nmem ) then
  echo " ${MYNAME}: Error, number of ensemble analysis unequal to number of members, aborting ... " 
  exit(1) 
endif

# In case both central and ensemble are at same resolution: copy RST/bkg to members
# ---------------------------------------------------------------------------------
if ( "$cres" == "$eres" ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic = $ic + 1
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      # Following needs revision for 4D case
      /bin/cp $RSTSTAGE4AENS/$expid.bkg06_eta_rst.${nymd}_${hhmn}z.$NCSUFFIX mem$memtag/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX 
   end
   /bin/cp $RSTSTAGE4AENS/$expid.bkg06_eta_rst.${nymd}_${hhmn}z.$NCSUFFIX ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX 
endif

touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
