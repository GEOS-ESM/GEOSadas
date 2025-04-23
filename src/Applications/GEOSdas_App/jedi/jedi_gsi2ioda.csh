#!/bin/csh

setenv MYNAME jedi_gsi2ioda.csh

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

if ( $#argv < 5 ) then
   echo " ${MYNAME}: invalid arg list, aborting"
   exit(1)
endif

set nymda = $1
set nhmsa = $2
set inpdir = $3
set outdir = $4
set jediwk = $5
set hha = `echo $nhmsa | cut -c1-2`
set yyyymmddhh = ${nymda}${hha}

setenv FAILED 0
if ( !($?EXPID)   )  setenv FAILED   1
if ( !($?FVHOME)  )  setenv FAILED   1
if ( !($?FVWORK)  )  setenv FAILED   1
if ( !($?JEDI_GSI2IODA)  )  setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

if (-e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

# Defaults
if ( !($?JEDI_RUN_ADANA) ) setenv JEDI_RUN_ADANA 0
   
if ( $JEDI_RUN_ADANA ) then
  source  $FVHOME/run/jedi/JEDIadanaConfig.csh
else
  source  $FVHOME/run/jedi/JEDIanaConfig.csh
endif

if ( ! $JEDI_GSI2IODA ) then
   echo "${MYNAME}: skipping GSI2IODA ..."
   exit(0)
endif 

# This needs care: TBD wired for now
source /discover/nobackup/projects/gmao/advda/rtodling/JEDI1/Jun23/jedi12/build-intel-release/modules
module unload py-pycodestyle/2.8.0
setenv JEDIWORK $jediwk

# Convert GSI-nc4-diag files to IODA
if ( $JEDI_GSI2IODA ) then
  cd $JEDIWORK

  # link diag files for conversion
  if ( -d GSIDIAGS ) /bin/rm -r GSIDIAGS
  mkdir GSIDIAGS
  cd GSIDIAGS
  ln -sf $inpdir/$EXPID.diag_*_ges.*nc4 .
  cd -
  if ( -d GSIBIAS ) /bin/rm -r GSIBIAS
  mkdir GSIBIAS
  cd GSIBIAS
# foreach ftype ( ana.satbiaspc ana.satbias ) # following needs attention (not right date/time)
#    ln -sf $inpdir/$EXPID.$ftype.${nymda}_${hha}z.txt .
  foreach ftype ( ana_satbias_rst ana_satbiaspc_rst ) # following needs attention (not right date/time)
     /bin/cp $FVHOME/recycle/*${ftype}*txt .   # TBD
  end
# foreach fname ( `ls *.txt` ) # following needs attention (not right date/time)
#    set pfx = `echo $fname | cut -d. -f1-2` 
#    /bin/mv $fname $pfx.${nymda}_${hha}z.txt
# end
  cd - 

  cd $JEDIWORK/swell
  swell-geosadas-run_tasks.sh
  if ( $status ) then
     echo "Trouble converting GSI output to IODA, aborting ..."
     exit 1
  else
     if ( -d $JEDIWORK/swell/swell-geosadas/run/${nymda}T${hha}0000Z/geos_atmosphere ) then
        mkdir   $outdir/ioda.${nymda}_${hha}0000
        /bin/mv $JEDIWORK/swell/swell-geosadas/run/${nymda}T${hha}0000Z/geos_atmosphere/*.nc4 \
                $outdir/ioda.${nymda}_${hha}0000
        /bin/mv $JEDIWORK/swell/swell-geosadas/run/${nymda}T${hha}0000Z/geos_atmosphere/*.txt \
                $outdir/ioda.${nymda}_${hha}0000
     else
        echo "$JEDIWORK/swell/swell-geosadas/run/${nymda}T${hha}0000Z/geos_atmosphere directory not found, aborting ..."
        exit 1
     endif
   endif
endif

touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
