#!/bin/csh -x

setenv MYNAME jedi_gsi2ioda.csh
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

# Defaults
if ( !($?JEDI_RUN_ADANA) ) setenv JEDI_RUN_ADANA 0
   
if ( $JEDI_RUN_ADANA ) then
  source  $FVHOME/run/jedi/JEDIadanaConfig.csh
else
  source  $FVHOME/run/jedi/JEDIanaConfig.csh
endif

if ( !($?JEDI_ROOT)  )  setenv FAILED   1
if ( !($?JEDI_CRTM_COEFFS) )  setenv FAILED   1
if ( !($?JEDI_OBS_OPT)  )  setenv FAILED   1

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

if ( $JEDI_OBS_OPT != 3 ) then
   echo "${MYNAME}: skipping GSI2IODA ..."
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

# Convert GSI-nc4-diag files to IODA
#if ( $JEDI_OBS_OPT == 3 ) then
  cd $JEDIWORK

  # link diag files for conversion
  if ( -d GSIDIAGS ) /bin/rm -r GSIDIAGS
  mkdir GSIDIAGS
  cd GSIDIAGS
  setenv GSIDIAGS_DIR `pwd`
  ln -sf $inpdir/$EXPID.diag_*_ges.*nc4 .
  cd -
  if ( -d GSIBIAS ) /bin/rm -r GSIBIAS
  mkdir GSIBIAS
  cd GSIBIAS
  foreach ftype ( acftbias_rst ana_satbias_rst ana_satbiaspc_rst )
     /bin/cp $FVHOME/recycle/*${ftype}*txt . 
     # fix date/time - swell wants biases at syn-time ...
     set fn = (`ls *$ftype*.txt`)
     set pfx = `echo $fn[1] | cut -d. -f1-2`
     set nfn = $pfx.${pnymda}_${phha}z.txt
     if ( ! -e $nfn ) then
        ln -s $fn $nfn 
     endif
  end
  cd - 

  # get positioned
  cd $JEDIWORK

  # prepare yaml for converting ncdiag to ioda
  vED -env $JEDIETC/diag2ioda.yaml -o diag2ioda.yaml

  # create convert suite
  if ( -e diag2ioda.yaml ) then
     source $JEDIDIR/SWELLConfig.csh
     if ( -d ~/cylc-run/${EXPID}-convert_ncdiags-suite ) then
        /bin/rm -r ~/cylc-run/${EXPID}-convert_ncdiags-suite
     endif
     $DRYRUN swell create convert_ncdiags --skip-r2d2 --override diag2ioda.yaml
  else
     echo "Trouble finding diag2ioda.yaml, aborting ..."
     exit 1
  endif

  # launch convert suite
  swell launch $JEDIWORK/${EXPID}-convert_ncdiags/${EXPID}-convert_ncdiags-suite -b
  if ( $status ) then
     echo "Trouble converting GSI output to IODA, aborting ..."
     exit 1
  else
     if ( -d $JEDIWORK/${EXPID}-convert_ncdiags/run/${nymda}T${hha}0000Z/geos_atmosphere ) then
        if (! -d $outdir/ioda.${nymda}_${hha}0000 ) mkdir -p $outdir/ioda.${nymda}_${hha}0000
        /bin/mv $JEDIWORK/${EXPID}-convert_ncdiags/run/${nymda}T${hha}0000Z/geos_atmosphere/*.nc4 \
                $outdir/ioda.${nymda}_${hha}0000
        /bin/mv $JEDIWORK/${EXPID}-convert_ncdiags/run/${nymda}T${hha}0000Z/geos_atmosphere/*.txt \
                $outdir/ioda.${nymda}_${hha}0000
     else
        echo "$JEDIWORK/${EXPID}-convert_ncdiags/run/${nymda}T${hha}0000Z/geos_atmosphere directory not found, aborting ..."
        exit 1
     endif
   endif
#endif

touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
