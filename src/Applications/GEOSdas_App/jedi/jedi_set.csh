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
   echo "     Last   modified: 01May2025    by: R. Todling"
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
if ( !($?JEDI_FEEDBACK_VARBC) )  setenv FAILED   1
if ( !($?OFFLINE_IODA_DIR) ) setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

# Defaults
if ( !($?JEDI_ANAFREQ))    setenv JEDI_ANAFREQ   21600
if ( !($?JEDI_RUN_ADANA) ) setenv JEDI_RUN_ADANA 0
if ( !($?JEDI_VAROFFSET))  setenv JEDI_VAROFFSET 10800
if ( !($?JEDI_VARWINDOW))  setenv JEDI_VARWINDOW 21600
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

set enddate  = `tick $nymdb $nhmsb $JEDI_VARWINDOW`
set nymde    = $enddate[1]
set nhmse    = $enddate[2]
set yyyye    = `echo $nymde | cut -c1-4`
set mme      = `echo $nymde | cut -c5-6`
set dde      = `echo $nymde | cut -c7-8`
set hhe      = `echo $nhmse | cut -c1-2`

setenv JEDI_ISO_DATE_BEG  "${yyyyb}-${mmb}-${ddb}T${hhb}:00:00Z"
setenv JEDI_ISO_DATE_ANA  "${yyyya}-${mma}-${dda}T${hha}:00:00Z"
setenv JEDI_ISO_DATE_END  "${yyyye}-${mme}-${dde}T${hhe}:00:00Z"
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

# Take care of static files needed by JEDI
# ----------------------------------------
cd $JEDIWRK
ln -sf $FVHOME/fv3-jedi .

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
cd -

# Get positioned in JEDI work dir
cd $JEDIWRK

foreach dir ( ana atmens bkg hofx iau obs osen inc vbc )
   if ( ! -d $dir ) mkdir -p $dir
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
  if ( ! -d $JEDIWRK/inc ) mkdir -p $JEDIWRK/inc
  if ( -e $FVWORK/jedi.fsens.eta.nc4 ) then
    cd $JEDIWRK/inc
    ln -sf $FVWORK/jedi.fsens.eta.nc4 .
    cd -
  endif
endif # adjoint analysis

# Link IODA observation files
# ---------------------------
if ( $JEDI_OBS_OPT == 2 ) then
   pwd
   ls
   cd obs
   if ( -d $OFFLINE_IODA_DIR/${nymda}T${nhmsa}Z/geos_atmosphere ) then
      ln -sf $OFFLINE_IODA_DIR/${nymda}T${nhmsa}Z/geos_atmosphere/* .
   else
      echo " ${MYNAME}: failed to link IODA files, aborting ..."
      exit 1
   endif
   cd -
   echo " ${MYNAME}: linked IODA files successfully"
endif

# Link IODA observation files
# ---------------------------
if ( $JEDI_OBS_OPT == 3 ) then
   pwd
   ls
   cd obs
   ln -sf $FVWORK/ioda.${nymda}_${hha}0000/* .
   cd -
   echo " ${MYNAME}: linked IODA files successfully"
endif

# If so, feedback VarBC (from previous cycle)
# -------------------------------------------
if ( $JEDI_FEEDBACK_VARBC ) then
  if ( -e $JEDIETC/VBC.BOOTSTRAP.DONE ) then # only when at least one JEDI cycle has completed

   cd obs
   # get tar-ball of varBC files from previous cycle
   setenv NYMDP  $nymdp
   setenv NHMSP  $nhmsp
   setenv ACQWORK $JEDIWRK/obs
   vED -env $FVHOME/run/jedi/jedi_acquire_vbc.j -o jedi_acquire_vbc.j
   if ( $BATCH_SUBCMD == "sbatch" ) then
      sbatch -W -o jedi_vbc.log jedi_acquire_vbc.j
   else
      qsub -W block=true -o jedi_vbc.log jedi_acquire_vbc.j
   endif
   set lstvbc = `ls *vbc*tar`
   if ($status) then
     echo "${MYNAME}: failed to retrieve vbc tar-ball"
     exit 1
   endif
  #/bin/rm *satbias*.nc4 *aircraft*csv - NOTE: there is no fully functional aircraft VarBC TBD
   /bin/rm *satbias*.nc4
   # unfold tar-ball and overwrite all bias correction files
   tar xvf *vbc*tar
   cd -

 endif
endif

# The following accommodates for the case when the satbias coeff and cov are in the same
# file - typically the stuff in R2D2 ca June 2024.
cd obs
set satbcov = `ls *.sabias_cov.*nc4`
if ( $status ) then
  echo "${MYNAME}: could not find satbias_cov"
  echo "${MYNAME}: linking satbias to satbias_cov ..."
  foreach fn ( `ls *.satbias.*nc4` )
    set prefix = `echo $fn | cut -d. -f1-2`
    ln -s $fn $prefix.satbias_cov.nc4
  end
endif
cd -
# The following accommodates for the case when the satbias coeff and cov are in the same
cd obs
if ( -e aircraft.$BYYYYYMMDDTHH0000Z.nc4 ) then 
  ln -s aircraft.$BYYYYYMMDDTHH0000Z.nc4 aircraft_temperature.$BYYYYYMMDDTHH0000Z.nc4
  ln -s aircraft.$BYYYYYMMDDTHH0000Z.nc4        aircraft_wind.$BYYYYYMMDDTHH0000Z.nc4
  set acftbias = `ls *.acftbias`
  if ( $status ) then
    set acftbias_in = `ls aircraft_abias_air.*.nc4`
    if (! $status ) then
      set ttag = `echo $acftbias_in | cut -d. -f2`
      ln -sf $acftbias_in aircraft_temperature.$ttag.acftbias
      if ( ! -e aircraft_temperature.$ttag.acftbias_cov ) then
        ln -sf aircraft_temperature.$ttag.acftbias aircraft_temperature.$ttag.acftbias_cov
      endif
    else
      echo "WARNING: No aircraft bias files where found ..."
      echo "WARNING: No aircraft bias files where found ..."
      echo "WARNING: No aircraft bias files where found ..."
    endif
  endif
else
  echo "WARNING: No aircraft obs files where found ..."
  echo "WARNING: No aircraft obs files where found ..."
  echo "WARNING: No aircraft obs files where found ..."
endif
cd -

# ensemble & background files
setenv JEDI_GET_ENSBKG 0
if ( $JEDI_HYBRID ) then
  if ( $JEDI_HYBRID == 1 ) then # lat-lon ensemble
     set ensdir = $FVHOME/atmens
     set bkgtyp = "bkg.eta"
     set nwords = 4
  else                          # cubed ensemble
     set ensdir = $FVHOME/atmens/ensbkgx
     set bkgtyp = "bkg_clcv"
     set nwords = 3
  endif
  if ( -d $ensdir ) then  # ensemble is present in FVHOME
     set this = `ls -1d $ensdir/mem*`
     @ nmem = $this[1] 
     cd $JEDIWRK
     @ nc = 0
     while ( $nc < $nmem[1] )
        @ nc = $nc + 1
        set memtag = `echo $nc | awk '{printf "%03d", $1}'`
        mkdir mem$memtag
        cd mem$memtag
        ln -sf $ensdir/mem$memtag/*.$bkgtyp.*nc4 . 
        foreach fn ( `ls *.$bkgtyp*.nc4`)
          set sfx = `echo $fn | cut -d. -f${nwords}-`
          ln -sf $fn geos.$bkgtyp.$sfx
        end
        cd -
     end
  else  # ensemble is NOT present in FVHOME (likely a replay run)
     setenv JEDI_GET_ENSBKG 1
  endif
endif

cd bkg
setenv NYMD  $nymdb # initial date of current cycle
setenv NHMS  $nhmsb # initial time of current cycle
setenv NYMDP $nymdp # initial date of previous cycle
setenv NHMSP $nhmsp # initial time of previous cycle
setenv ACQWORK $JEDIWRK/bkg
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
      set ttag = `echo $fn | cut -d. -f3-`
      set ymd = `echo $ttag | cut -c1-8`
      set hm  = `echo $ttag | cut -c10-13`
      set sfx = ${ymd}T${hm}00Z.nc4 # cope swell reinvented notation
      ln -sf $fn bkg.$sfx
   end
   cd $JEDIWRK
   ln -sf $JEDIWRK/bkg/bkg.*.nc4 .
   if ( -e $JEDIETC/convertinc_geos.yaml ) then
      set lst = (`ls bkg.*.nc4`)
      set cres  = `getgfiodim.x $lst[1]`
      @ jcres = $cres[1] + 1
      setenv JEDI_BKG_RESOL $jcres
      vED -env $JEDIETC/convertinc_geos.yaml -o $JEDIWRK/Config/convertinc_geos.yaml
   endif
   cd -
   # the following is a nedeed hack due to inconsistencies in MAPL
#  if ( $MAPLFIX ) then
#     mkdir Ori
#     foreach fn ( `ls *.bkg_clcv_rst*nc4` )
#        /bin/mv $fn Ori/
#        $FVHOME/run/jedi/convert_xdimydim_2_latlon.py -i Ori/$fn -o $fn 
#     end
#  endif
else
   echo " ${MYNAME}: failed to retrieve bkg tar ball, aborting ..."
   exit(3)
endif
if( $JEDI_GET_ENSBKG ) then
   cd $JEDIWRK/atmens
   setenv NYMD  $nymdb # initial date of current cycle
   setenv NHMS  $nhmsb # initial time of current cycle
   setenv NYMDP $nymdp # initial date of previous cycle
   setenv NHMSP $nhmsp # initial time of previous cycle
   setenv ACQWORK $JEDIWRK/atmens
   vED -env $FVHOME/run/jedi/jedi_acquire_ebkg.j -o jedi_acquire_ebkg.j
   if ( $BATCH_SUBCMD == "sbatch" ) then
      sbatch -W -o jedi_acq.log jedi_acquire_ebkg.j
   else
      qsub -W block=true -o jedi_acq.log jedi_acquire_ebkg.j
   endif
   if ( $JEDI_HYBRID == 1 ) then # lat-lon ensemble
      set tarbal = "atmens_ebkg"
      set inball = ""
      set bkgtyp = "bkg.eta"
      set nwords = 4
   else                          # cubed ensemble
      set tarbal = "atmens_ebkgx"
      set inball = "ensbkgx"
      set bkgtyp = "bkg_clcv"
      set nwords = 3
   else                          # cubed ensemble
   endif
   set lst = `ls *.$tarbal.*.tar `
   if ( $#lst == 1 ) then
      tar xvf $lst
      /bin/mv *${tarbal}*z/$inball/mem* .
      foreach dir ( `ls -d mem*` ) 
        cd $dir
        foreach fn ( `ls *.$bkgtyp*.nc4` )
          set sfx = `echo $fn | cut -d. -f${nwords}-`
          ln -sf $fn geos.$bkgtyp.$sfx
       end
       cd -
     end
     cd $JEDIWRK
     ln -sf atmens/mem* .
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
