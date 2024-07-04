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
   echo "     Last   modified: 22Set2023    by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif


# Slurm
# -----
#_SBATCH -A g0613
#_SBATCH --qos=advda
#_SBATCH --job-name=geos_jedi
#_SBATCH --output=geos_jedi.o%j
#_SBATCH --nodes=9
#_SBATCH --ntasks-per-node=24
#_SBATCH --time=01:00:00
#_#SBATCH --constraint=sky

# orig from: /gpfsm/dnb31/drholdaw/JediWork/GeosRun4Ricardo

#setenv CNVENS       0
#setenv RUN_BUMP     1
#setenv RUN_ANA      0
#setenv UPD_INIT_RST 0

setenv FAILED 0
if ( !($?EXPID)            )  setenv FAILED   1
if ( !($?FVHOME)           )  setenv FAILED   1
if ( !($?FVWORK)           )  setenv FAILED   1
if ( !($?JEDI_ROOT)        )  setenv FAILED   1
if ( !($?JEDI_HYBRID)      )  setenv FAILED   1
if ( !($?JEDI_RUN_ANA)     )  setenv FAILED   1
if ( !($?JEDI_RUN_CNVANA)  )  setenv FAILED   1
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

cd $FVWORK/jedi.$nymda.${hha}0000
pwd

# OOPS trace and debug logging (0 or 1)
# -------------------------------------
#export OOPS_TRACE=1
#export OOPS_DEBUG=1

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
   mkdir -p $FVWORK/jana/Data/bump
   $JEDI_BUMP_MPIRUN $JEDIBUILD/bin/fv3jedi_parameters.x Config/bump_parameters.yaml
   if ( $status ) then
       echo " ${MYNAME}: failed in BUMP, aborting ..."
       exit (1)
   endif
endif


# Run 3DVar/En3/4DVar FGAT
# ----------------------
if ( $JEDI_RUN_ANA ) then
   if ( -e Config/geosvar.${nymda}_${hha}z.yaml ) then
      setenv MYCONF Config/geosvar.${nymda}_${hha}z.yaml
   else
      setenv MYCONF Config/geosvar.yaml
   endif
   if ( $JEDI_RUN_ADANA_TEST ) then
      setenv MYCONF Config/envarfgat4adtest.yaml
      if (! -d Data/inc ) mkdir Data/inc 
   endif
   if ( $JEDI_RUN_ADANA ) then
      setenv MYCONF Config/adenvarfgat.yaml
      if (! -d Data/inc ) mkdir Data/inc 
   endif
   if ( $JEDI_HYBRID ) then
      if ( -e Config/hybens_info ) then
        /bin/cp Config/hybens_info .
     else
        echo "${MYNAME}: failed to find hybens_info"
        exit (1)
     endif
   endif

#  if ( -e $FVHOME/run/jedi/jedi_run_var.j ) then
#     vED -env $FVHOME/run/jedi/jedi_run_var.j -o jedi_run_var.j
#     sbatch -W jedi_run_var.j
#     sleep 2
#  else
#     $JEDI_FV3VAR_MPIRUN $JEDIBUILD/bin/fv3jedi_var.x $MYCONF |& tee -a $FVWORK/$JEDIVARLOG
#     if ( $status ) then
#         echo " ${MYNAME}: failed in VAR, aborting ..."
#         exit (1)
#     endif
#  endif

   # If testing Adjoint analysis ...
   # -------------------------------
   if ( $JEDI_RUN_ADANA_TEST ) then
      setenv MYCONF Config/adtest_envarfgat.yaml
      $JEDI_FV3VAR_MPIRUN $JEDIBUILD/bin/fv3jedi_var.x $MYCONF
      if ( $status ) then
          echo " ${MYNAME}: failed in test for AD VAR, aborting ..."
          exit (1)
      endif
   endif
endif


if ( $JEDI_RUN_CNVANA ) then

   # Convert analysis to restart like fields
   # ---------------------------------------
#  if ( -d Data/ana ) then
#     $JEDI_CNVANA_MPIRUN $JEDIBUILD/bin/fv3jedi_convertstate.x Config/convertana_geos.yaml
#  else
#     echo " ${MYNAME}: failed in convert ana, aborting ..."
#     exit (1)
#  endif

   # Create restart increment from analysis and background
   # -----------------------------------------------------
   if ( -d Data/inc ) then
      if ( $JEDI_HYBRID ) then
         foreach cf ( `ls Config/convertinc_geos_*.yaml` )
            $JEDI_CNVINC_MPIRUN $JEDIBUILD/bin/fv3jedi_convertincrement.x  $cf
         end
      else
         $JEDI_CNVINC_MPIRUN $JEDIBUILD/bin/fv3jedi_convertincrement.x  Config/convertinc_geos.yaml
      endif
   else
      echo " ${MYNAME}: failed in convert inc, aborting ..."
      exit (1)
   endif

endif # JEDI_CNVANA

if ( $JEDI_RUN_UPDRST ) then

   # Create restart increment from analysis and background
   # -----------------------------------------------------
   if ( ! -d Data/restart ) mkdir -p Data/restart
   /bin/cp Data/bkg/fvcore_internal_rst Data/restart/
   /bin/cp Data/bkg/moist_internal_rst Data/restart/
   $JEDI_ADDINC_MPIRUN $JEDIBUILD/bin/fv3jedi_addincrement.x Config/create_new_restart.yaml

endif # UPD_INIT_RST

# archive hofx
# ------------
cd $JEDIWORK/Data/hofx
tar cvf $FVWORK/$EXPID.jedi_hofx.${nymdb}_${hhb}z.tar *nc4
cd -

# archive varBC
# -------------
cd $JEDIWORK/Data/vbc
tar cvf $FVWORK/$EXPID.jedi_vbc.${nymdb}_${hhb}z.tar *satbias*nc4
cd -

# If here, likely successful
# --------------------------
touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
