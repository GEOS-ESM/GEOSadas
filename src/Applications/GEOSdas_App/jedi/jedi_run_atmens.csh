#!/bin/csh -x

setenv MYNAME jedi_run_atmens.csh
setenv DRYRUN #echo

if ( ! $?FVHOME ) then
  env
  echo " ${MYNAME}: need FVHOME env"
  exit (1)
endif

if ( ! -d $FVHOME/run/jedi ) then 
   echo " ${MYNAME}: Nothing to do."
   exit (1)
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

setenv SYSFAILED 0
if ( !($?LMOD_CMD)  )  setenv SYSFAILED  1
if ( !($?LMOD_SETTARG_CMD)  )  setenv SYSFAILED  1

setenv FAILED 0
if ( !($?EXPID)   )  setenv FAILED   1
if ( !($?FVHOME)  )  setenv FAILED   1
if ( !($?FVWORK)  )  setenv FAILED   1
if ( !($?SWELL_INSTALL)  )  setenv FAILED   1
if ( !($?TIMEINC)  )  setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

if (-e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

if ( !($?JEDI_ROOT)  )  setenv FAILED   1
if ( !($?JEDI_CRTM_COEFFS) )  setenv FAILED   1

if ( $SYSFAILED ) then
  env
  echo " ${MYNAME}: not all SYSTEM required env vars defined"
  exit (1)
endif
if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

if ( ! $JEDI_RUN_EAANA ) then
   echo "${MYNAME}: skipping jedi-atmens ..."
   exit(0)
endif 

# set JEDI workdir
setenv JEDIETC  $FVHOME/run/jedi/Config
setenv JEDIWORK $jediwk

# define ISO analysis date/time
set yyyya    = `echo $nymda | cut -c1-4`
set mma      = `echo $nymda | cut -c5-6`
set dda      = `echo $nymda | cut -c7-8`
set hha      = `echo $nhmsa | cut -c1-2`
setenv JEDI_ISO_DATE_ANA  "${yyyya}-${mma}-${dda}T${hha}:00:00Z"

# set time of previous analysis (for bias stuff)
@ varlen = $TIMEINC * 60
set panadate  = `tick $nymda $nhmsa -$varlen`
set pnymda    = $panadate[1]
set pnhmsa    = $panadate[2]
set pyyyya    = `echo $pnymda | cut -c1-4`
set pmma      = `echo $pnymda | cut -c5-6`
set pdda      = `echo $pnymda | cut -c7-8`
set phha      = `echo $pnhmsa | cut -c1-2`

# set cycle initial time
@ varoff = $VAROFFSET * 60
set inidate  = `tick $nymda $nhmsa -$varoff`
set nymdi    = $inidate[1]
set nhmsi    = $inidate[2]
set yyyyi    = `echo $nymdi | cut -c1-4`
set mmi      = `echo $nymdi | cut -c5-6`
set ddi      = `echo $nymdi | cut -c7-8`
set hhi      = `echo $nhmsi | cut -c1-2`

  # get positioned
  cd $JEDIWORK

  # sanity check
  if ( -e $JEDIETC/jedi_eda.yaml && \
       -e $JEDIETC/jedi_getkf.yaml  ) then
      echo "${MYNAME}: both EDA & GETKF are set to run ..."
      echo "${MYNAME}: user should decide which, aborting ..."
      exit(1)
  endif
  # prepare yaml to run JEDI-based atmos ens
  set foundrc = 0
  if ( -e $JEDIETC/jedi_eda.yaml ) then
     setenv SUITE eda_atmos
     set myyaml = jedi_eda.yaml
     set foundrc = 1
     echo "${MYNAME}: setting up to run JEDI-EDA"
  endif
  if ( -e $JEDIETC/jedi_getkf.yaml ) then
     setenv SUITE localensembleda_2steps
     set myyaml = jedi_getkf.yaml
     set foundrc = 1
     echo "${MYNAME}: setting up to run JEDI-GETKF"
  endif
  if ( $foundrc ) then
     vED -env $JEDIETC/$myyaml -o $myyaml
  else
     echo "${MYNAME}: no JEDI ensemble settings found ..."
     exit(1)
  endif

  ###########################################################
  # The following is temporary: make sure the tarball of bkgs
  # are available for SWELL (in a cycling exp the ensemble 
  # is readily available before tar'ing - but SWELL expects
  # a tarball in the archive). 
  ###########################################################
  if ( ! -d archive/$EXPID ) then
     mkdir -p archive/$EXPID/atmens/Y$yyyyi/M$mmi
     cd $FVHOME/atmens
     mkdir $EXPID.atmens_ebkgx.${nymdi}_${hhi}z
     mv ensbkgx $EXPID.atmens_ebkgx.${nymdi}_${hhi}z
     tar cvf $JEDIWORK/archive/$EXPID/atmens/Y$yyyyi/M$mmi/$EXPID.atmens_ebkgx.${nymdi}_${hhi}z.tar \
             $EXPID.atmens_ebkgx.${nymdi}_${hhi}z
     mv $EXPID.atmens_ebkgx.${nymdi}_${hhi}z/ensbkgx .
     /bin/rm -r $EXPID.atmens_ebkgx.${nymdi}_${hhi}z
     cd -
  endif

  # create atmospheric ensemble suite
  if ( -e $myyaml ) then
     source $JEDIDIR/SWELLConfig.csh
     if ( -d ~/cylc-run/${EXPID}-${SUITE}-suite ) then
        /bin/rm -r ~/cylc-run/${EXPID}-${SUITE}-suite
     endif
     $DRYRUN swell create ${SUITE} --skip-r2d2 --override $myyaml
  else
     echo "Trouble finding $myyaml, aborting ..."
     exit 1
  endif

  # launch convert suite
  swell launch $JEDIWORK/${EXPID}-${SUITE}/${EXPID}-${SUITE}-suite -b -t PT30S
  if ( $status ) then
     echo "Trouble running JEDI atmos ensemble, aborting ..."
     exit 1
  else
#    if ( -d $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere ) then
#       if (! -d $outdir/ioda.${nymda}_${hha}0000 ) mkdir -p $outdir/ioda.${nymda}_${hha}0000
#       /bin/mv $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/*.nc4 \
#               $outdir/ioda.${nymda}_${hha}0000
#       /bin/mv $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/*.txt \
#               $outdir/ioda.${nymda}_${hha}0000
#    else
#       echo "$JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere directory not found, aborting ..."
#       exit 1
#    endif
#  endif
#endif

touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
