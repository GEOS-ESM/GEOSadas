#!/bin/csh -x

if ( !($?JEDI_VERBOSE) ) then
    setenv JEDI_VERBOSE 0
else
    if ( $JEDI_VERBOSE )  set echo
endif

setenv MYNAME jedi_set.csh

# Set up for JEDI analysis

if ( $#argv < 2 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - set up environment and retrieve data needed to run JEDI analysis within GEOSadas"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  nymd nhms "
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Initial version: 18Oct2020    by: R. Todling"
   echo "     Last   modified: 18Oct2020    by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?BATCH_SUBCMD)  )  setenv FAILED   1
if ( !($?EXPID)         )  setenv FAILED   1
if ( !($?FVHOME)        )  setenv FAILED   1
if ( !($?FVWORK)        )  setenv FAILED   1
if ( !($?GID)           )  setenv FAILED   1
if ( !($?JEDI_OBS_OPT)  )  setenv FAILED   1
if ( !($?JEDI_OBS_DIR)  )  setenv FAILED   1
if ( !($?JEDI_HYBRID)   )  setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

# Defaults
if ( !($?JEDI_ANAFREQ))    setenv JEDI_ANAFREQ   21600
if ( !($?JEDI_RUN_ADANA) ) setenv JEDI_RUN_ADANA 0
if ( !($?JEDI_VAROFFSET))  setenv JEDI_VAROFFSET 10800
if ( !($?MAPLFIX)       )  setenv MAPLFIX  0

# Command line arguments
set nymdb = $1   # initial date of var window
set nhmsb = $2   # initial time of var window
set yyyyb    = `echo $nymdb | cut -c1-4`
set mmb      = `echo $nymdb | cut -c5-6`
set ddb      = `echo $nymdb | cut -c7-8`
set hhb      = `echo $nhmsb | cut -c1-2`
set yyyymmddhh = ${nymdb}${hhb}

if ( -e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

if ( ($?FVHOME) ) then
   set path = ( . $FVHOME/run $FVROOT/bin $path )
else
   set path = ( . $FVROOT/bin $path )
endif
 
set prvdate  = `tick $nymdb $nhmsb -$JEDI_ANAFREQ`
set nymdp    = $prvdate[1]
set nhmsp    = $prvdate[2]
set yyyyp    = `echo $nymdp | cut -c1-4`
set mmp      = `echo $nymdp | cut -c5-6`
set ddp      = `echo $nymdp | cut -c7-8`
set hhp      = `echo $nhmsp | cut -c1-2`

set anadate  = `tick $nymdb $nhmsb $JEDI_VAROFFSET`
set nymda    = $anadate[1]
set nhmsa    = $anadate[2]
set yyyya    = `echo $nymda | cut -c1-4`
set mma      = `echo $nymda | cut -c5-6`
set dda      = `echo $nymda | cut -c7-8`
set hha      = `echo $nhmsa | cut -c1-2`

setenv JEDI_ISO_DATE_ANA  "${yyyya}-${mma}-${dda}T${hha}:00:00Z"
setenv AYYYYMMDDHH         ${yyyya}${mma}${dda}${hha}
setenv AYYYYMMDD_HH        ${yyyya}${mma}${dda}_${hha}
setenv BYYYYYMMDDTHH0000Z  ${nymdb}T${hhb}0000Z
setenv PYYYYYMMDDTHH0000Z  ${nymdp}T${hhp}0000Z

@ h = 1
set this_date = ( $nymdb $nhmsb )
while ($h < 8) # wired to max 1 hour bkg freq
  set this_nymd  = `echo $this_date[1]`
  set this_yyyy  = `echo $this_nymd | cut -c1-4`
  set this_mm    = `echo $this_nymd | cut -c5-6`
  set this_dd    = `echo $this_nymd | cut -c7-8`
  set this_hh    = `echo $this_date[2] | cut -c1-2`
  setenv YYYYMMDD_HH00_00${h}Z  ${this_nymd}_${this_hh}00z
  setenv JEDI_ISO_DATE_00${h}  "${this_yyyy}-${this_mm}-${this_dd}T${this_hh}:00:00Z"
  set this_date = (`tick $this_nymd ${this_hh}0000 3600`)
  @ h++
end

setenv JEDIWRK $FVWORK/jedi.$nymda.$nhmsa
setenv JEDIETC $FVHOME/run/jedi/Config
touch $JEDIWRK/.no_archiving

mkdir -p $JEDIWRK/Config
mkdir -p $JEDIWRK/Data

# CONFIG:
# -------
# Get positined in Config ...
cd $JEDIWRK/Config
foreach fn ( `ls $JEDIETC/*.yaml` )
  set this = `basename $fn`
  vED -env $fn -o $this
end
foreach fn ( `ls $JEDIETC/*.nml` )
  /bin/cp $fn .
end
foreach fn ( `ls $JEDIETC/*.tmpl` )
  /bin/cp $fn .
end
if ( $JEDI_HYBRID ) then
  if ( -e $JEDIETC/hybens_info ) then
     /bin/cp $JEDIETC/hybens_info .
  else
     echo "${MYNAME}: failed to find hybens_info"
     exit (1)
  endif
endif
cd -

# Get positined in Data ...
cd $JEDIWRK/Data

foreach dir ( ana bkg atmens hofx iau obs osen inc vbc )
   if ( ! -d $dir ) mkdir -p $dir
end

# link directories (NOTE: CRTM coeffs will not live in FVHOME for long)
#foreach dir ( crtm femps fieldmetadata fieldsets fv3files inputs )
foreach dir (            fieldmetadata fieldsets fv3files inputs )
  ln -sf $FVHOME/run/jedi/Data/$dir .
end

# if adjoint analysis, retrieve IODA files
if ( $JEDI_RUN_ADANA || $JEDI_OBS_OPT == 1 ) then
  setenv NYMD  $nymda # initial date of current cycle
  setenv NHMS  $nhmsa # initial time of current cycle
  setenv ACQWORK $FVWORK
  vED -env $FVHOME/run/jedi/jedi_acquire_ioda.j -o jedi_acquire_ioda.j
  if ( $BATCH_SUBCMD == "sbatch" ) then
     sbatch -W -o jedi_ioda.log jedi_acquire_ioda.j
  else
     qsub -W block=true -o jedi_ioda.log jedi_acquire_ioda.j
  endif
  ls $FVWORK/*ioda*tar
  if ( $status ) then
    echo " ${MYNAME}: Cannot find file"
    exit 1
  endif
  tar xvf $FVWORK/*ioda*.tar
  cd obs
  /bin/ln -sf ../ioda.${nymda}_${hha}0000/*nc4 .
  cd -
  echo " ${MYNAME}: retrieved IODA files successfully"

# Also link forecast sensitivity at this time
# -------------------------------------------
  if ( ! -d $JEDIWRK/Data/inc ) mkdir -p $JEDIWRK/Data/inc
  cd $JEDIWRK/Data/inc
  ln -sf $FVWORK/jedi.fsens.eta.nc4 .
  cd -
endif # adjoint analysis

# Link IODA observation files
# ---------------------------
if ( $JEDI_OBS_OPT == 2 ) then
   pwd
   ls
   cd obs
   ln -sf $FVWORK/ioda.${nymda}_${hha}0000/* .
   cd -
   echo " ${MYNAME}: linked IODA files successfully"
endif

# ensemble & background files
setenv JEDI_GET_ENSBKG 0
if ( $JEDI_HYBRID ) then
  if ( -d $FVHOME/atmens ) then # full hybrid DAS
     cd atmens
     ln -sf $FVHOME/atmens/mem* . 
     cd -
     echo "${MYNAME}: Got ensemble from FVHOME"
     setenv JEDI_GET_ENSBKG 1
  endif
  if ( ! $JEDI_GET_ENSBKG ) then
     if ( -d $FVWORK/atmens ) then # replay hybrid DAS
        cd atmens
        ln -sf $FVWORK/atmens/mem* . 
        cd -
        echo "${MYNAME}: Got ensemble from FVWORK"
        setenv JEDI_GET_ENSBKG 1
     endif
  endif
# if ( ! $JEDI_GET_ENSBKG ) then
#    set efn = `echo $EXPID.bkg_clcv.${nymdb}_${hhb}00z.nc4` # wired template name for now
#    if ( -e $FVHOME/atmens/enstraj/mem001/$efn ) then
#       cd atmens
#       ln -sf $FVHOME/atmens/enstraj/mem* . 
#       cd -
#    else
#       setenv JEDI_GET_ENSBKG 1
#    endif
# endif
endif

cd bkg
setenv NYMD  $nymdb # initial date of current cycle
setenv NHMS  $nhmsb # initial time of current cycle
setenv NYMDP $nymdp # initial date of previous cycle
setenv NHMSP $nhmsp # initial time of previous cycle
setenv ACQWORK $JEDIWRK/Data/bkg
vED -env $FVHOME/run/jedi/jedi_acquire_bkg.j -o jedi_acquire_bkg.j
if ( $BATCH_SUBCMD == "sbatch" ) then
   sbatch -W -o jedi_acq.log jedi_acquire_bkg.j
else
   qsub -W block=true -o jedi_acq.log jedi_acquire_bkg.j
endif
set lst = `ls $EXPID.bkgcrst.*.tar`
if ( $#lst == 1 ) then
   tar xvf $lst
   /bin/rm $EXPID.bkgcrst.*.tar
   set lst = ( `ls *.bkg_clcv_rst*nc4` )
   set vexpid = `echo $lst[1] | cut -d. -f1`
   if ( $vexpid != $EXPID ) then # care for when tarball from another exp
      foreach fn ( `ls *.bkg_clcv_rst*nc4` )
         set sfx = `echo $fn | cut -d. -f2-`
         /bin/mv $fn $EXPID.$sfx
      end
   endif
   foreach fn ( `ls *.bkg_clcv_rst*nc4` )
      set sfx = `echo $fn | cut -d. -f3-`
      ln -sf $fn bkg.$sfx
   end
   # this is a hack
   if ( $JEDI_SWELLUSE ) then
      ln -sf $JEDIWRK/swell/swell-geosadas/configuration/jedi/interfaces/geos_atmosphere/*crtmsrf*nc4 .
   else
      ln -sf $FVHOME/run/jedi/Config/*crtmsrf*nc4 .
      if ($status) then
         echo "${MYNAME}: expecting crtmsrf in jedi/Config"
         exit 1
      endif
   endif
   # the following is a nedeed hack due inconsistencies in MAPL
   if ( $MAPLFIX ) then
      mkdir Ori
      foreach fn ( `ls *.bkg_clcv_rst*nc4` )
         /bin/mv $fn Ori/
         $FVHOME/run/jedi/convert_xdimydim_2_latlon.py -i Ori/$fn -o $fn 
      end
   endif
else
   echo " ${MYNAME}: failed to retrieve bkg tar ball, aborting ..."
   exit(3)
endif
setenv JEDI_GET_ENSBKG 0 # below is old stuff that needs revising, bypass for now
if( $JEDI_GET_ENSBKG ) then
   set lst = `ls *.atmens_etrj.*.tar `
   if ( $#lst == 1 ) then
      tar xvf $lst
      set vexpid = `ls -d *.atmens_etrj*z | cut -d. -f1`
      /bin/mv $vexpid.atmens_etrj*z/enstraj/mem* $JEDIWRK/Data/atmens
      /bin/rm -r $vexpid.atmens_etrj*z $vexpid.atmens_etrj*z.tar
      if ( $vexpid != $EXPID ) then # care for tarball from another exp
         cd $JEDIWRK/Data/atmens
         foreach dir (`ls -d mem*`)
             cd $dir
             foreach fn (`ls *.nc4`)
                set sfx = `echo $fn | cut -d. -f2-`
                /bin/mv $fn $EXPID.$sfx
             end
             cd -
         end
         cd $JEDIWRK/Data
      endif
   else
      echo " ${MYNAME}: failed to retrieve ensemble tar ball, aborting ..."
      exit(4)
   endif
endif

# If here, likely successful
# --------------------------
touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
