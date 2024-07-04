#!/bin/csh

setenv MYNAME geos2jedi.csh

if ( ! $?FVHOME ) then
  env
  echo " ${MYNAME}: need FVHOME env"
  exit (1)
endif

if ( ! -d $FVHOME/run/jedi ) then
   echo " ${MYNAME}: Nothing to do."
endif

if ( $#argv < 4 ) then
   echo " ${MYNAME}: invalid arg list, aborting"
   exit(1)
endif

set nymdb = $1
set nhmsb = $2
set nymda = $3
set nhmsa = $4
set hhb = `echo $nhmsb | cut -c1-2`
set hha = `echo $nhmsa | cut -c1-2`

setenv FAILED 0
if ( !($?EXPID)   )  setenv FAILED   1
if ( !($?FVWORK)  )  setenv FAILED   1

set jedi_sets = $FVHOME/run/jedi/swell/swell-geosadas.yaml
if ( ! -e $jedi_sets ) then
   setenv FAILED 1
   echo " ${MYNAME}: Error, cannot find $jedi_sets"
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

if (! -d $FVWORK/JEDIWORK.$nymda.$nhmsa ) \
mkdir -p $FVWORK/JEDIWORK.$nymda.$nhmsa

# create work directories
cd $FVWORK/JEDIWORK.$nymda.$nhmsa
foreach dir (bkg obs satbc ) 
  if (! -d $dir ) mkdir $dir
end

# Position input observations
cd obs
if ($dir == "obs" ) then
   ln -s $FVWORK/*.diag_*nc4 .
endif
cd -

# Acquire backgrounds and satbc

