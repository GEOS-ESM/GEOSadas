#!/bin/csh

# acquire_ensperts.csh - acquires NMC-like perturbations from archive
#
# !REVISION HISTORY:
#
#   19Jun2012 El Akkraoui/Todling  - initial code
#   10Sep2012 Todling   - revisit usage; doc handle
#   20Oct2012 Todling   - redefine time arg in API from hh to nhms
#                       - also address reproducibility issue
#   17Apr2013 Todling   - update to use seasonally-aware NMC-perts
#   23Jun2020 Todling   - redef meaning of ATMENSLOC
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME acquire_ensperts.csh

# need usage here
# ---------------
if ( $#argv < 5 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - acquires NMC-like ensemble perturbations"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid member nymd nhms INCLOC "
   echo " " 
   echo "   where" 
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   member -  number of member to operate on"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   INFLOC -  location to place perturbations at"
   echo " " 
   echo " DESCRIPTION" 
   echo " "
   echo "   Acquire NMC-like perturbations from database."
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC - location of ensemble resource files"
   echo "    ATMENSLOC - location of ensemble, usually FVHOME"
   echo "    FVHOME    - location of experiment            "
   echo "    FVROOT    - location of DAS build             "
   echo "    FVWORK    - location of DAS work directory    "
   echo " " 
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " " 
   echo "    NCSUFFIX  - suffix of hdf/netcdf files (default: nc4)"
   echo " " 
   echo " SEE ALSO"
   echo "    atmens_recenter.csh - recentering script uses files retrieved here"
   echo " " 
   echo " AUTHOR"
   echo "   Amal El Akkraoui (Amal.ElAkkraoui@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0

if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?ATMENSLOC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

#source $FVROOT/bin/g5_modules
set path = ( .  $FVHOME/run $FVROOT/bin $path )

set expid   = $1
set nmem    = $2
set nymd    = $3
set nhms    = $4
set ploc    = $5

set hh = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}

setenv ENSWORK $FVWORK
setenv DO_DMGET 1   # force dmget to be used when acquire retrieve files
setenv dry_run

if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

if( ! -d $ENSWORK/$ploc ) mkdir -p $ENSWORK/$ploc
cd $ENSWORK/$ploc

# Take existing dates.dat, in case it's already been generated
# This allows run to be reproducible (user needs to retrieve dates from archive)
if (-e $ATMENSLOC/$expid.rndperts.dates.${nymd}_${hh}z.txt ) then
   /bin/cp $ATMENSLOC/$expid.rndperts.dates.${nymd}_${hh}z.txt dates.dat 
endif

if (! -e dates.dat ) then 
   atmens_seasonal_date.csh $nymd $nhms $nmem
   if ($status) then
      echo "${MYNAME}: trouble in atmens_seasonal_date.csh, aborting ..."
      exit(1)
   endif
endif 
set pertimes = (`cat dates.dat`)

# Store dates for re-run and archive purposes
if (! -e $ATMENSLOC/$expid.rndperts.dates.${nymd}_${hh}z.txt ) then
   /bin/cp dates.dat $ATMENSLOC/$expid.rndperts.dates.${nymd}_${hh}z.txt
endif

# Build acquire rc file
# ---------------------
if( -e getperts.acq ) /bin/rm getperts.acq
touch getperts.acq

set pertdir  = `echorc.x -rc $ATMENSETC/nmcperts.rc nmc_perts_location`
set n = 0 
while ( $n < $nmem )
  @ n = $n + 1
  set pnymd = `echo $pertimes[$n] | cut -c1-8`
  set phh = `echo $pertimes[$n] | cut -c9-10`
  echo $pnymd
  echo $phh
  set memtag = `echo $n | awk '{printf "%03d", $1}'`
  set pertname = `echorc.x -template dummy $pnymd ${phh}0000 -rc $ATMENSETC/nmcperts.rc nmc_perts_fnametmpl`
  set fullpertname = $pertdir/$pertname 
  echo $fullpertname >> getperts.acq
end

# Now acquire required perturbations
# ----------------------------------
if ( -d $ENSWORK/$pertdir ) then   # files are online
   foreach fn (`cat getperts.acq`)
       ln -s $ENSWORK/$fn .
   end
else                               # files on arquive
   set usrspool = `echorc.x -rc $ATMENSETC/nmcperts.rc nmc_perts_spool`
   if ("$usrspool" == "null") then
     set spool = "-s $ENSWORK/spool"
   else
     set spool = "-s $usrspool"
   endif
   acquire -v -rc getperts.acq $spool -d . -strict 17760704 000000 240000 1
   if ($status) then
       echo " ${MYNAME}: cannot retrieve NMC-like perturbations"
       cat getperts.acq
       exit 1
   endif
endif

# Rename and retime-tag perturbations accordingly
# -----------------------------------------------
set n = 0
while ( $n < $nmem )
  @ n = $n + 1
  set pnymd = `echo $pertimes[$n] | cut -c1-8`
  set phh = `echo $pertimes[$n] | cut -c9-10`
  echo $pnymd
  echo $phh
  set memtag = `echo $n | awk '{printf "%03d", $1}'`
  set pertname = `echorc.x -template dummy $pnymd ${phh}0000 -rc $ATMENSETC/nmcperts.rc nmc_perts_fnametmpl`
  if ( ! -e $pertname ) then
      echo " ${MYNAME}: cannot find NMC-like perturbation $pertname"
      exit 1
  endif
  ln -sf $ENSWORK/$ploc/$pertname $expid.nmcpert.eta.${nymd}_${hh}z.$memtag.$NCSUFFIX
end

# If here, ended successfully
# ---------------------------
cd $ENSWORK
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
exit 0

