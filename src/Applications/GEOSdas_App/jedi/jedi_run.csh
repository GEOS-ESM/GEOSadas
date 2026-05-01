#!/bin/csh

if ( !($?JEDI_VERBOSE) ) then
    setenv JEDI_VERBOSE 0
else
    if ( $JEDI_VERBOSE )  set echo
endif

setenv MYNAME jedi_run.csh

if ( $#argv < 2 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - run JEDI analysis "
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  nymd nhms "
   echo " "
   echo "  where "
   echo "   nymd  -  analysis synoptic date"
   echo "   nhms  -  analysis synoptic time"
   echo " " 
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Initial version: 18Oct2020    by: R. Todling"
   echo "     Last   modified: 30Apr2026    by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif


setenv FAILED 0
if ( !($?EXPID)            )  setenv FAILED   1
if ( !($?FVHOME)           )  setenv FAILED   1
if ( !($?FVWORK)           )  setenv FAILED   1
if ( !($?JEDI_HYBRID)      )  setenv FAILED   1
if ( !($?JEDI_ROOT)        )  setenv FAILED   1
if ( !($?JEDI_RUN_ANA)     )  setenv FAILED   1
if ( !($?JEDI_RUN_CNVANA)  )  setenv FAILED   1
if ( !($?JEDI_RUN_GETINC)  )  setenv FAILED   1
if ( !($?JEDI_RUN_UPDRST)  )  setenv FAILED   1
if ( !($?JEDI_VAROFFSET)   )  setenv FAILED   1
if ( !($?JEDI_ADDINC_MPIRUN) )  setenv FAILED   1
if ( !($?JEDI_CNVANA_MPIRUN) )  setenv FAILED   1
if ( !($?JEDI_CNVENS_MPIRUN) )  setenv FAILED   1
if ( !($?JEDI_CNVINC_MPIRUN) )  setenv FAILED   1
if ( !($?JEDI_FV3VAR_MPIRUN) )  setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

# Internal defaults
# -----------------
if ( !($?JEDI_RUN_ADANA_TEST) ) setenv JEDI_RUN_ADANA_TEST  0
if ( !($?JEDI_RUN_BUMP)  )      setenv JEDI_RUN_BUMP        0
if ( !($?JEDI_RUN_CNVENS)  )    setenv JEDI_RUN_CNVENS      0
if ( !($?BATCH_SUBCMD)  )       setenv BATCH_SUBCMD      sbatch

set nymdb = $1   # initial date of var window
set nhmsb = $2   # initial time of var window
set yyyyb    = `echo $nymdb | cut -c1-4`
set mmb      = `echo $nymdb | cut -c5-6`
set ddb      = `echo $nymdb | cut -c7-8`
set hhb      = `echo $nhmsb | cut -c1-2`
set yyyymmddhh = ${nymdb}${hhb}

set anadate  = `tick $nymdb $nhmsb $JEDI_VAROFFSET`
set nymda    = $anadate[1]
set nhmsa    = $anadate[2]
set yyyya    = `echo $nymda | cut -c1-4`
set mma      = `echo $nymda | cut -c5-6`
set dda      = `echo $nymda | cut -c7-8`
set hha      = `echo $nhmsa | cut -c1-2`

setenv JEDIVARLOG $EXPID.jedi_var.log.${nymdb}_${hhb}z.txt

# Build directory for JEDI built with geos
# ----------------------------------------
#setenv JEDIBUILD /gpfsm/dnb31/drholdaw/JediDev/fv3-bundle/work/build-baselibs-intel-impi-19.1.0.166-release-fv3
#setenv JEDIBUILD /discover/nobackup/projects/gmao/advda/rtodling/4OPS/geosJEDI/fv3-bundle/build-baselibs-intel-impi-19.1.0.166-release-geos

if ( -e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

# Jedi modules
# ------------
#source $MODULESHOME/init/sh
module purge
setenv  JEDIBUILD $JEDI_ROOT
if ( -e $JEDIBUILD/modules ) then
  source $JEDIBUILD/modules
else
  if ( -e $FVHOME/run/jedi/modules ) then
    source $FVHOME/run/jedi/modules
  else
    echo "${MYNAME}: unable to set env"
    exit(1)
  endif
endif
setenv JEDIETC $FVHOME/run/jedi/Config

setenv JEDIWRK $FVWORK/jedi.$nymda.${hha}0000
cd $JEDIWRK
pwd

# OOPS trace and debug logging (0 or 1)
# -------------------------------------
#export OOPS_TRACE=1
#export OOPS_DEBUG=1

if ( $BATCH_SUBCMD == "sbatch" ) then
    setenv BLOCKFLAG "-W"
else
    setenv BLOCKFLAG "-W block=true"
endif

# Convert ensemble restarts to analysis variables
# -----------------------------------------------
if ( $JEDI_RUN_CNVENS ) then
   $JEDI_CNVENS_MPIRUN $JEDIBUILD/bin/fv3jedi_convertstate.x Config/convert_ensemble.yaml
endif

# Traj is written by old version of GEOS with no tile dim, crtm surface needs the same
# ------------------------------------------------------------------------------------
#mpirun -np $JEDI_NCPUS $JEDIBUILD/bin/fv3jedi_convertstate.x Config/remove_tile_dim_crtm_file.yaml


# Generate localization coefficients (****run this only once****)
# ---------------------------------------------------------------
if ( $JEDI_RUN_BUMP ) then
   zeit_ci.x jedi_bump
   mkdir -p $FVWORK/jana/bump
   $JEDI_BUMP_MPIRUN $JEDIBUILD/bin/fv3jedi_parameters.x Config/bump_parameters.yaml
   if ( $status ) then
       echo " ${MYNAME}: failed in BUMP, aborting ..."
       exit (1)
   endif
   zeit_co.x jedi_bump
endif


# Run 3DVar/En3/4DVar FGAT
# ----------------------
if ( ! -e $FVWORK/.DONE_jedi_run_ana.csh.$yyyymmddhh) then
 if ( $JEDI_RUN_ANA ) then
   zeit_ci.x jedi_var

   if ( -e Config/geosvar.${nymda}_${hha}z.yaml ) then
      setenv MYCONF Config/geosvar.${nymda}_${hha}z.yaml
   else
#     setenv MYCONF Config/geosvar.yaml
       echo " ${MYNAME}: geosvar.${nymda}_${hha}z.yaml not found, aborting ..."
       exit (2) 
   endif
   if ( $JEDI_RUN_ADANA_TEST ) then
      setenv MYCONF Config/envarfgat4adtest.yaml
      if (! -d inc ) mkdir inc 
   endif
   if ( $JEDI_RUN_ADANA ) then
      setenv MYCONF Config/adenvarfgat.yaml
      if (! -d inc ) mkdir inc 
   endif

   if ( -e $FVHOME/run/jedi/jedi_run_var.j ) then
      sbatch -W $FVHOME/run/jedi/jedi_run_var.j
      sleep 2
   else
      $JEDI_FV3VAR_MPIRUN $JEDIBUILD/bin/fv3jedi_var.x $MYCONF |& tee -a $FVWORK/$JEDIVARLOG
      if ( $status ) then
          echo " ${MYNAME}: failed in VAR, aborting ..."
          exit (1)
      endif
   endif
   /bin/mv *inc*nc4 ./inc # somehow datapath setting in yaml is not effective at inc part
   zeit_co.x jedi_var
   
   # Converged in these many iterations
   # ----------------------------------
   set convniter = `grep JoJc $FVWORK/$JEDIVARLOG | wc`
   echo "${MYNAME}:  JEDI var converged in $convniter[1] iterations"

   # If testing Adjoint analysis ...
   # -------------------------------
   if ( $JEDI_RUN_ADANA_TEST ) then
      zeit_ci.x jedi_advar
      setenv MYCONF Config/adtest_envarfgat.yaml
      $JEDI_FV3VAR_MPIRUN $JEDIBUILD/bin/fv3jedi_var.x $MYCONF
      if ( $status ) then
          echo " ${MYNAME}: failed in test for AD VAR, aborting ..."
          exit (1)
      endif
      zeit_co.x jedi_advar
   endif
 endif
 touch $FVWORK/.DONE_jedi_run_ana.csh.$yyyymmddhh
endif

if ( $JEDI_RUN_GETINC ) then
  zeit_ci.x jedi_getinc

  # Calculate increment on the cubed offline from cubed ana and bkg
  # ATTENTION: 1. This should be parallelized.
  #            2. mkiau has been enabled to handled cubed states, so this
  #               can be bypassed at some point.
  # ---------------------------------------------------------------
  if ( $JEDI_HYBRID ) then
     setenv GETINCWORK incwork
     @ ii = 0
     foreach fn (`ls Config/diffstates_geos_*.yaml`)
        mkdir -p incwork.${ii}
        cd $GETINCWORK.${ii}
        ln -sf ../bkg .
        ln -sf ../ana .
        ln -sf $FVHOME/fv3-jedi .
        ln -sf ../$fn my.yaml
        cd -
        @ ii = $ii + 2
     end

     if ( -e $FVHOME/run/jedi/jedi_diffstates.j ) then
        vED -env $FVHOME/run/jedi/jedi_diffstates.j -o jedi_diffstates.j
     else
        echo " ${MYNAME}: missing run/jedi/jedi_diffstates.j file, aborting ... "
        exit 1        
     endif
     $BATCH_SUBCMD $BLOCKFLAG -o diffstates.log  jedi_diffstates.j

  else

     foreach cana (`ls ana/*ana.ceta*` )
        set this = `basename $cana`
        set  ttag = `echo $this | cut -d. -f4`
        set yyyys = `echo $ttag | cut -c1-4`
        set   mms = `echo $ttag | cut -c5-6`
        set   dds = `echo $ttag | cut -c7-8`
        set   hhs = `echo $ttag | cut -c10-11`
        if ( -e Config/diffstates_geos_${yyyys}${mms}${dds}_${hhs}z.yaml ) then
           $JEDI_GETINC_MPIRUN $JEDIBUILD/bin/fv3jedi_diffstates.x Config/diffstates_geos_${yyyys}${mms}${dds}_${hhs}z.yaml
        else
           echo " ${MYNAME}: missing Config/diffstates_geos_${yyyys}${mms}${dds}_${hhs}z.yaml file, aborting ... "
           exit 1
        endif
     end

  endif
  /bin/mv $EXPID.*inc*nc4 ./inc  # apparently diffstate does not listen to datapath on output

  zeit_co.x jedi_getinc
endif

if ( $JEDI_RUN_CNVANA ) then
   zeit_ci.x jedi_cvana

   # Convert analysis to restart like fields
   # ---------------------------------------
   if ( -d ana && -e Config/convertana_geos.yaml ) then
      $JEDI_CNVANA_MPIRUN $JEDIBUILD/bin/fv3jedi_convertstate.x Config/convertana_geos.yaml
#  else
#     echo " ${MYNAME}: failed in convert ana, aborting ..."
#     exit (1)
   endif

   # Create restart increment from analysis and background
   # -----------------------------------------------------
   if ( -d inc && -e Config/convertinc_geos.yaml ) then
      $JEDI_CNVINC_MPIRUN $JEDIBUILD/bin/fv3jedi_convertincrement.x  Config/convertinc_geos.yaml
#  else
#     echo " ${MYNAME}: failed in convert inc, aborting ..."
#     exit (1)
   endif

   zeit_co.x jedi_cvana
endif # JEDI_CNVANA

if ( $JEDI_RUN_UPDRST ) then
   zeit_ci.x jedi_uprst

   # Create restart increment from analysis and background
   # -----------------------------------------------------
   if ( ! -d restart ) mkdir -p restart
   /bin/cp bkg/fvcore_internal_rst restart/
   /bin/cp bkg/moist_internal_rst restart/
   $JEDI_ADDINC_MPIRUN $JEDIBUILD/bin/fv3jedi_addincrement.x Config/create_new_restart.yaml

   zeit_co.x jedi_uprst
endif # UPD_INIT_RST

# archive hofx
# ------------
cd $JEDIWORK/hofx
tar cvf $FVWORK/$EXPID.jedi_hofx.${nymdb}_${hhb}z.tar *nc4
cd -

# archive varBC
# -------------
touch $JEDIETC/VBC.BOOTSTRAP.DONE
cd $JEDIWORK/vbc
tar cvf $FVWORK/$EXPID.jedi_vbc.${nymdb}_${hhb}z.tar *satbias*nc4 *aircraft*csv
cd -

# If here, likely successful
# --------------------------
touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
