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

set  nymda = $1
set  nhmsa = $2
set inpdir = $3
set outdir = $4
set jediwk = $5
set  yyyya = `echo $nymda | cut -c1-4`
set    mma = `echo $nymda | cut -c5-6`
set    dda = `echo $nymda | cut -c7-8`
set    hha = `echo $nhmsa | cut -c1-2`
set yyyymmddhh = ${nymda}${hha}
setenv ISO_STATES_DATE "${yyyya}-${mma}-${dda}T${hha}:00:00Z"

setenv SYSFAILED 0
if ( !($?LMOD_CMD)  )  setenv SYSFAILED  1
if ( !($?LMOD_SETTARG_CMD)  )  setenv SYSFAILED  1

setenv FAILED 0
if ( !($?EXPID)   )  setenv FAILED   1
if ( !($?FVHOME)  )  setenv FAILED   1
if ( !($?FVWORK)  )  setenv FAILED   1
if ( !($?SWELL_INSTALL)  )  setenv FAILED   1
if ( !($?TIMEINC)  )  setenv FAILED   1
if ( !($?JEDI_ROOT)  )  setenv FAILED   1
if ( !($?JEDI_CRTM_COEFFS) )  setenv FAILED   1

if ( !($?NCSUFFIX)  )  setenv NCSUFFIX   nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

if (-e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
else
   echo "${MYNAME}: Starting ... "
endif

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

setenv JEDIBUILD $JEDI_ROOT

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

if ( ! -e $FVWORK/.DONE_${MYNAME}_${SUITE}.$yyyymmddhh ) then

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
     if ( ! -d ensbkgx ) then
       echo " ${MYNAME}: Trouble finding ensbkgx dir in FVHOME/atmens, aborting ..."
       exit 1
     endif
     /bin/mv ensbkgx $EXPID.atmens_ebkgx.${nymdi}_${hhi}z
     tar cvf $JEDIWORK/archive/$EXPID/atmens/Y$yyyyi/M$mmi/$EXPID.atmens_ebkgx.${nymdi}_${hhi}z.tar \
             $EXPID.atmens_ebkgx.${nymdi}_${hhi}z
     /bin/mv $EXPID.atmens_ebkgx.${nymdi}_${hhi}z/ensbkgx .
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
     echo " ${MYNAME}: Trouble finding $myyaml, aborting ..."
     exit 1
  endif

  # launch convert suite
  swell launch $JEDIWORK/${EXPID}-${SUITE}/${EXPID}-${SUITE}-suite -b -t PT30S
  if ( $status ) then
     echo " ${MYNAME}: Trouble running JEDI atmos ensemble, aborting ..."
     exit 1
  else
     touch $FVWORK/.DONE_${MYNAME}_${SUITE}.$yyyymmddhh
     echo " ${MYNAME}: Complete "
  endif

endif # already run suite

source $JEDIBUILD/modules

  # Rename all files from suite-name to expid; make sure there are not ana.eta (lat-lon) files present
  cd $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/analysis/
  set members = `ls -d mem*`
  foreach dir ( mean variance $members )
     cd $dir
     /bin/rm *.ana.eta.*.nc4
     # rename swell-given filenames to experiment id name
     foreach fn ( `ls ${SUITE}*nc4` )
       set sfx = `echo $fn | cut -d. -f2-`
       /bin/mv $fn $EXPID.$sfx
     end
     cd -
  end

  # mimic env configuration of central ADAS within each member directory
# cd $FVWORK
# /bin/ln -sf $FVHOME/atmens/central .
# cd -
  foreach mdir ( $members )
    set this = $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/analysis/$mdir
    cd $this
    /bin/ln -sf $FVHOME/fv3-jedi .
    # reposition and rename analysis and increment files
    mkdir ana inc
    foreach dir ( ana inc )
       cd $dir
       /bin/ln -sf ../*${dir}*nc4 .
       cd -
    end
    # reposition cubed and lat-lon background files
    set this = $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/analysis/$mdir/bkg
    if ( ! -d $this ) mkdir $this
    cd $this
    foreach fn ( `ls $FVWORK/$mdir/${EXPID}.bkg.eta.*nc4` )
       /bin/ln -sf $fn .
       /bin/ln -sf $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/ebkg/$mdir/geos*nc4 .
    end
    cd -
    # create location of IAU increments
    set this = $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/analysis/$mdir/iau
    mkdir $this
  end

  # Generate all lat-lon increments for each member (PARALLELIZATION TO BE DONE)
  # the following assumes ensemble is running 3D scheme
  foreach mdir ( $members )
     set this = $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/analysis/$mdir
     cd $this
     foreach cana (`ls ana/*ana.ceta*` )
        set  yaml = diffstates_geos.yaml
        if ( -e $JEDIETC/$yaml ) then
           /bin/cp $JEDIETC/$yaml .
           set ocres = `echorc.x -rc $yaml npx`              # resolution wired in yaml
           set ncres = `getgfiodim.x $cana | grep -v GFIO` # current ensemble analysis resolution
           # reset wired resolution to ensemble resolution
           if (-e sed_file ) rm -f sed_file
           echo "s/$ocres/$ncres[1]/g"   > sed_file
           /bin/mv $yaml $yaml.hold
           sed -f sed_file  ./$yaml.hold  > ./$yaml
           /bin/rm $yaml.hold sed_file

           set cbkg = `ls bkg/geos*${nymda}_${hha}*nc4`
           set bkgfn = `basename $cbkg`
           set anafn = `basename $cana`
           setenv JEDI_CUBED_BKG $bkgfn
           setenv JEDI_CUBED_ANA $anafn
           /bin/mv $yaml $yaml.hold
           vED -env $yaml.hold -o $yaml

           $JEDI_GETINC_MPIRUN $JEDIBUILD/bin/fv3jedi_diffstates.x $yaml
           if ($status) then
              echo " ${MYNAME}: fail to run diffstates.x for member $mdir , aborting ... "
              exit 1
           endif
           /bin/mv *inc*.eta.*nc4 inc/  # JEDI diff does not place inc in subdir
        else
           echo " ${MYNAME}: missing Config/diffstates_geos.yaml file, aborting ... "
           exit 1
        endif
     end
  end

  cd $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/
  set this = $FVWORK/updated_ens/$EXPID.atmens_olog.${nymda}_${hha}z.tar
  if ( -e $this.gz ) then
     /bin/rm $this.gz
  endif
  tar cvf $this *_mem*.log
  gzip    $this

# The mean/variances at this point are calculated before recentering, so do not move them over ...
# ------------------------------------------------------------------------------------------------
# /bin/mv $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/analysis/mean/*.nc4 \
#          $FVWORK/updated_ens/ensmean
# /bin/mv $JEDIWORK/${EXPID}-${SUITE}/run/${nymda}T${hha}0000Z/geos_atmosphere/analysis/variance/*.nc4 \
#          $FVWORK/updated_ens/ensrms

touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
