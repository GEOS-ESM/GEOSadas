#!/bin/csh

setenv MYNAME jedi_swellset.csh

if ( ! $?FVHOME ) then
  env
  echo " ${MYNAME}: need FVHOME env"
  exit (1)
endif

if ( ! -d $FVHOME/run/jedi ) then 
   echo " ${MYNAME}: Nothing to do."
else
   setenv JEDIDIR $FVHOME/run/jedi
endif

if ( $#argv < 4 ) then
   echo " ${MYNAME}: invalid arg list, aborting"
   exit(1)
endif

set nymda = $1
set nhmsa = $2
set jedidir = $3
set jediwrk = $4
set hha = `echo $nhmsa | cut -c1-2`
set yyyymmddhh = ${nymda}${hha}

if (-e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

setenv FAILED 0
if ( !($?EXPID)   )  setenv FAILED   1
if ( !($?FVWORK)  )  setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

if ( !($?JEDI_RUN_ADANA)  )  setenv JEDI_RUN_ADANA  0

# Defaults
if ( $JEDI_RUN_ADANA ) then
  source  $FVHOME/run/jedi/JEDIadanaConfig.csh
else
  source  $FVHOME/run/jedi/JEDIanaConfig.csh
endif

# This needs care: TBD wired for now
source /discover/nobackup/projects/gmao/advda/rtodling/JEDI1/Jun23/jedi12/build-intel-release/modules
module unload py-pycodestyle/2.8.0

# Setup SWELL
# ===========
cd    $jediwrk/swell
ln -s $jedidir/swell/* .

if ( -e swell-geosadas-env.tmpl ) then
   if ( -e swell-geosadas-env ) /bin/rm swell-geosadas-env
   setenv YYYYMMDD_HHz ${nymda}_${hha}z
   vED -env swell-geosadas-env.tmpl -o swell-geosadas-env
else
  echo "${MYNAME}: swell-geosadas-env.tmpl not found, aborting ..."
  exit (1)
endif
swell-geosadas-set_experiment.sh

# Prep yaml for IODA converter
if ( -e swell-geosadas.yaml.tmpl ) then
   if ( -e swell-geosadas.yaml ) /bin/rm swell-geosadas.yaml
   vED -env swell-geosadas.yaml.tmpl -o swell-geosadas.yaml
else
  echo "${MYNAME}: swell-geosadas.yaml.tmpl not found, aborting ..."
  exit (1)
endif

touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh 
echo " ${MYNAME}: Complete "
exit(0)
