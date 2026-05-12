#!/bin/csh

setenv MYNAME jedi_driver.csh

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

set nymdb = $1
set nhmsb = $2
set nymda = $3
set nhmsa = $4
set yyyya = `echo nymda | cut -c1-4`
set   mma = `echo nymda | cut -c5-6`
set   dda = `echo nymda | cut -c7-8`
set   hhb = `echo $nhmsb | cut -c1-2`
set   hha = `echo $nhmsa | cut -c1-2`
set yyyymmddhh = ${nymda}${hha}

setenv NYMDA $nymda
setenv   HHA $hha

setenv FAILED 0
if ( !($?EXPID)   )  setenv FAILED   1
if ( !($?FVWORK)  )  setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

# Defaults
if ( !($?JEDI_SET)  )  setenv JEDI_SET   1
if ( !($?JEDI_RUN)  )  setenv JEDI_RUN   1
if ( !($?JEDI_MKIAU))  setenv JEDI_MKIAU 0
if ( !($?JEDI_POST) )  setenv JEDI_POST  0
if ( !($?JEDI_IAU_OVERWRITE) )  setenv JEDI_IAU_OVERWRITE  0
if ( !($?JEDI_RUN_ADANA_TEST) ) setenv JEDI_RUN_ADANA_TEST 0
if ( !($?JEDI_RUN_ADANA) ) setenv JEDI_RUN_ADANA 0
if ( !($?JEDI_OBS_OPT) ) setenv JEDI_OBS_OPT 0
if ( !($?JEDI_SWELLUSE) ) setenv JEDI_SWELLUSE 1
if ( !($?OFFLINE_IODA_DIR) ) setenv OFFLINE_IODA_DIR /dev/null

if ( $JEDI_RUN_ADANA ) then
  source  $FVHOME/run/jedi/JEDIadanaConfig.csh
else
  source  $FVHOME/run/jedi/JEDIanaConfig.csh
endif

if ( -e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

setenv JEDIWORK $FVWORK/jedi.$nymda.$nhmsa
if ( ! -d $JEDIWORK ) mkdir -p $JEDIWORK/jedi.$nymda.$nhmsa

# Setup SWELL & IODA Files
# ========================
if ( $JEDI_SWELLUSE ) then
  jedi_swellset.csh $nymda $nhmsa $JEDIDIR $JEDIWORK
  if ($status) then
     echo "${MYNAME}: failed, aborting ..."
     exit (1)
  endif

else

# If here, IODA files must be available 
# -------------------------------------
  if ( $OFFLINE_IODA_DIR == "/dev/null" || $OFFLINE_IODA_DIR == "/dev/null/" ) then

     # Convert GSI-nc4-diag files to IODA
     # ----------------------------------
     if ( $JEDI_OBS_OPT == 3 ) then
       jedi_gsi2ioda.csh $nymda $nhmsa $FVWORK $FVWORK $JEDIWORK
       if ( $status ) then
         echo "Trouble converting GSI output to IODA, aborting ..."
         exit 1
       endif
     endif

  else
     set IODADIR = $OFFLINE_IODA_DIR/${nymda}T${hha}0000Z/geos_atmosphere
     if ( ! -d $FVWORK/ioda.${nymda}_${hha}0000 ) mkdir $FVWORK/ioda.${nymda}_${hha}0000
     cd $FVWORK/ioda.${nymda}_${hha}0000
     ln -s $IODADIR/*.nc4 .
     ln -s $IODADIR/*.txt .
     cd -
  endif

endif

# Prepare env for analysis
# ------------------------
if ( $JEDI_SET ) then
   zeit_ci.x jedi_set
   jedi_set.csh $nymdb $nhmsb |& tee -a $FVWORK/$EXPID.jedi_set.log.${nymdb}_${hhb}z.txt
   if ( $status ) then
      echo " ${MYNAME}: jedi_set.csh signal failure, aborting ..."
      exit (1)
   endif
   zeit_co.x jedi_set
endif

# Run JEDI analysis
if ( $JEDI_RUN ) then
   zeit_ci.x jedi_run
   jedi_run.csh $nymdb $nhmsb |& tee -a $FVWORK/$EXPID.jedi_run.log.${nymdb}_${hhb}z.txt
   if ( $status ) then
      echo " ${MYNAME}: jedi_run.csh signal failure, aborting ..."
      exit (1)
   endif
   zeit_co.x jedi_run
   if ( $JEDI_MKIAU ) then
       zeit_ci.x jedi_mkiau
       jedi_mkiau.csh $nymdb $nhmsb |& tee -a $FVWORK/$EXPID.jedi_miau.log.${nymdb}_${hhb}z.txt
       if ( $status ) then
          echo " ${MYNAME}: jedi_mkiau.csh signal failure, aborting ..."
          exit (1)
       endif
       zeit_co.x jedi_mkiau
   endif
endif

# Wrap up
if ( $JEDI_POST ) then
   if ( ! $JEDI_RUN_ADANA ) then
      zeit_ci.x jedi_post
      jedi_post.csh $nymdb $nhmsb hofx |& tee -a $FVWORK/$EXPID.jedi_post.log.${nymdb}_${hhb}z.txt
      if ( $status ) then
         echo " ${MYNAME}: jedi_post.csh (hofx) signal failure, aborting ..."
         exit (1)
      endif
      zeit_co.x jedi_post
   endif
   if ( $JEDI_RUN_ADANA_TEST || $JEDI_RUN_ADANA ) then
      zeit_ci.x jedi_post
      jedi_post.csh $nymdb $nhmsb osen |& tee -a $FVWORK/$EXPID.jedi_post.log.${nymdb}_${hhb}z.txt
      if ( $status ) then
         echo " ${MYNAME}: jedi_post.csh (osen) signal failure, aborting ..."
         exit (1)
      endif
      zeit_co.x jedi_post
   endif
endif

# Overwrite IAU increments if desired
if ( $?SKIPGSI ) then
   if ( $SKIPGSI ) then
     setenv JEDI_IAU_OVERWRITE 1
     echo " ${MYNAME}: WARNING, No GSI analysis detected."
     echo " ${MYNAME}: WARNING, JEDI-related IAU will be used in model integration. "
   endif
endif
if ( $JEDI_MKIAU && $JEDI_IAU_OVERWRITE ) then
  zeit_ci.x jedi_up4geos
  jedi_upd4geos.csh $nymdb $nhmsb |& tee -a $FVWORK/$EXPID.jedi_upd.log.${nymdb}_${hhb}z.txt
  if ( $status ) then
     echo " ${MYNAME}: JEDI update failed, aborting ..."
     exit (1)
  endif
  zeit_co.x jedi_up4geos
endif

# If here, likely successful
# --------------------------
touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
