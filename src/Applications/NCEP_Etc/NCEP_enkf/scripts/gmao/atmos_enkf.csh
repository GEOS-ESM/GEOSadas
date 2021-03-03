#!/bin/csh

# atmos_enkf - run Atmospheric EnKF
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  09Feb2013  Todling   Allow for ensemble-specific scales
#  17Feb2013  Todling   Do not attempt to rid of mean diag files when obsimp calculated
#  27Jun2016  Todling   No longer links to diag files
#  03Mar2017  Todling   No longer links to background; 
#                       analysis files left within mem dir for handling by update
#  03Jan2018  Todling   Allow EnKF to run even when FFEn is set to run
#  05Mar2018  Todling   Allow storage of non-inflated EnKF ana when FFEn is running 
#  31Jul2018  Todling/Gu  Allow for ens own setting of anavinfo file
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#  31Mar2020  Todling   Jobmonitor to protect against faulty batch-block
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmos_enkf.csh
setenv ENSFGAT 0
setenv ENSMEAN 1
setenv skipTRANSF "-skipTRANSF"   # no transform needed for ana-sensitivity executable
setenv skipSOLVER "-skipSOLVER"   # need to run the analysis sensitivity solver
setenv skipSATBIAS "-skipSATBIAS" # no need to worry about running satellite bias correction

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - run atmospheric EnKF analysis"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "  This procedure creates an ensemble of analysis by running"
   echo "  J. Whitaker ensemble Kalman filter (EnKF). This is the same"
   echo "  software-basis as that presently used at NCEP 3DVAR-Hybrid."
   echo "  The parameter settings used for GEOS are specific to GEOS," 
   echo "  including resoltion, and vertical and horizontal localizations."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo "  atmos_enkf.nml.tmpl - sets parameters for EnKF software "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "  ATMENSETC      - location of EnKF resource files   "
   echo "  FVHOME         - location of experiment            "
   echo "  FVROOT         - location of DAS build             "
   echo "  FVWORK         - location of work directory        "
   echo "  MPIRUN_ATMENKF - define mpi command for GSIsa.x    "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "  NCSUFFIX          - suffix of hdf/netcdf files "
   echo "                      (default: nc4)"
   echo "  ENSPARALLEL       - when set, runs all ensemble components in parallel"
   echo "                      (default: off)"
   echo "  AENKF_NCPUS       - when parallel ens on, this sets NCPUS for AGCM integration"
   echo "  ATMENKF_WALLCLOCK - wall clock time to run EnKF, default 1:00:00 "
   echo "  ATMENKF_QNAME     - name of queue (default: NULL, that is, let pbs pick) "
   echo " "
   echo " SEE ALSO"
   echo "   atmos_eana.csh - entry-point of ensemble analysis"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 03Mar2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENSETC)      ) setenv FAILED 1
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1
if ( !($?MPIRUN_ATMENKF) ) setenv FAILED 1

if ( !($?NCSUFFIX)         ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)      ) setenv ENSPARALLEL 0 
if ( !($?ATMENS_DEBUG)     ) setenv ATMENS_DEBUG 0
if ( !($?ATMENKF_MPIPROCS) ) setenv ATMENKF_MPIPROCS 0
if ( !($?ATMENKF_WALLCLOCK)) setenv ATMENKF_WALLCLOCK 1:00:00 
if ( !($?ATMENKF_QNAME)    ) setenv ATMENKF_QNAME NULL

if ( $ENSPARALLEL ) then
   if ( !($?AENKF_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $AENKF_NCPUS
   endif
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3
set hh     = `echo $nhms | cut -c1-2`
set mn     = `echo $nhms | cut -c3-4`
set hhmn   = ${hh}${mn}
set yyyymmddhh = ${nymd}${hh}
set yyyymmddhhmn = ${nymd}${hhmn}

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]

# Get positioned inside ENSWORK
# -----------------------------
cd  $ENSWORK
touch .no_archiving

set doing_eezy = 0
if( (-e $ATMENSETC/easyeana.rc) ) then
   set doing_eezy = 1
endif

if ( -e $ENSWORK/.DONE_MEM001_ENKFX_${MYNAME}.$yyyymmddhh ) then
  echo " ${MYNAME}: already run EnKF executable, skipping to next step ...."
else

  # Hold analyses from Filter-free scheme in temporary location
  # NOTE: the moved files are actually symbolic links ...
  # -----------------------------------------------------------
  if ( $doing_eezy && ! -e $ENSWORK/.HOLD_${MYNAME}.eezy_ana.$yyyymmddhh ) then
     if (! -d $ENSWORK/Hold_EEZY ) mkdir $ENSWORK/Hold_EEZY
     @ ic = 0
     while ( $ic < $nmem )
        @ ic++
        set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
        if(! -d $ENSWORK/Hold_EEZY/mem$memtag ) mkdir $ENSWORK/Hold_EEZY/mem$memtag
        /bin/mv $ENSWORK/mem$memtag/*.ana.eta.*.mem$memtag.$NCSUFFIX $ENSWORK/Hold_EEZY/mem$memtag
     end
     echo " ${MYNAME}: holding filter-free analyses for now ... "
     touch $ENSWORK/.HOLD_${MYNAME}.eezy_ana.$yyyymmddhh
  endif

  # Setup observer
  # --------------
  setobsvr.csh -obsclass NONE $skipTRANSF $skipSOLVER $skipSATBIAS $FVHOME $FVWORK $nymd $nhms $EXPID

  if( -e $ATMENSETC/gmao_global_anavinfo.rc ) then
     /bin/rm anavinfo
     /bin/ln -s $ATMENSETC/gmao_global_anavinfo.rc anavinfo
  endif

  # Determine lat/lon
  # -----------------
  set ENKFNML = $ATMENSETC/atmos_enkf.nml.tmpl
  set ENKFOUT = atm_enkf
  if ( -e ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX ) then
     set mean_eta_file = ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX
  else
     if ( -e ensmean/$expid.niana.eta.${nymd}_${hhmn}z.$NCSUFFIX ) then
        set ENKFNML = $ATMENSETC/atmos_enkf_sens.nml.tmpl
        set mean_eta_file = ensmean/$expid.niana.eta.${nymd}_${hhmn}z.$NCSUFFIX
     else
        if ( -e ensmean/$expid.ana.eta.${nymd}_${hhmn}z.$NCSUFFIX ) then
           set ENKFNML = $ATMENSETC/atmos_enkf_sens.nml.tmpl
           set mean_eta_file = ensmean/$expid.ana.eta.${nymd}_${hhmn}z.$NCSUFFIX
        else
           echo " ${MYNAME}: cannnot determine ensemble dim, aborting ..."
           exit(1)
        endif
     endif
     set ENKFOUT = atm_enkf_osens
  endif
  set ens_mres  = `getgfiodim.x $mean_eta_file`
  set ens_nlons = $ens_mres[1]
  set ens_nlats = $ens_mres[2]
  set ens_nlevs = $ens_mres[3]

  # Look to see if need use ensemble scales different than those of central
  # -----------------------------------------------------------------------
  if ( -e $ATMENSETC/gmao_global_hybens_info.x${ens_nlons}y${ens_nlats}l${ens_nlevs}.rc ) then
     /bin/rm hybens_info
     ln -s $ATMENSETC/gmao_global_hybens_info.x${ens_nlons}y${ens_nlats}l${ens_nlevs}.rc hybens_info
     echo "${MYNAME}: using ensemble-specific scales"
  endif

  # Prepare namelist
  # ----------------
   /bin/rm -f sed_file
   echo "s/>>>EXPID<<</${expid}/1"                > sed_file
   echo "s/>>>YYYYMMDDHH<<</${yyyymmddhh}/1" >> sed_file
   echo "s/>>>ENS_NLATS<<</${ens_nlats}/1"       >> sed_file
   echo "s/>>>ENS_NLONS<<</${ens_nlons}/1"       >> sed_file
   echo "s/>>>ENS_NLEVS<<</${ens_nlevs}/1"       >> sed_file
   echo "s/>>>NMEM<<</${nmem}/1"                 >> sed_file
   /bin/rm -f ./enkf.nml
   sed -f sed_file  $ENKFNML  > ./enkf.nml

  # Run ensemble
  # ------------
   if( $ENSPARALLEL ) then

        if ( $ATMENKF_MPIPROCS ) then
           set mpiprocs = "-mpiprocs $ATMENKF_MPIPROCS"
        else
           set mpiprocs = ""
        endif
        jobgen.pl \
             -egress enkf.log    \
             -q $ATMENKF_QNAME $mpiprocs  \
             aenkf               \
             $GID                \
             $ATMENKF_WALLCLOCK  \
             "$MPIRUN_ATMENKF |& tee -a $ENSWORK/atm_enkf.log" \
             $ENSWORK            \
             $MYNAME             \
             $ENSWORK/.DONE_MEM001_ENKFX_${MYNAME}.$yyyymmddhh \
             "AtmosEnKF Failed"

             if ( -e aenkf.j ) then
                if ( $ATMENS_BATCHSUB == "sbatch" ) then
                   $ATMENS_BATCHSUB -W aenkf.j
                else
                   $ATMENS_BATCHSUB -W block=true aenkf.j
                endif
             else
                echo " ${MYNAME}: Failed to generate PBS jobs for AtmEnKF, Aborting ... "
                exit(1)
             endif

             # Monitor job in case block fails
             # -------------------------------
             jobmonitor.csh 1 ENKFX_${MYNAME}  $ENSWORK $yyyymmddhh
             if ($status) then
                 echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
                 exit(1)
             endif

   else

        $MPIRUN_ATMENKF |& tee -a $ENSWORK/atm_enkf.log
        if ( $status ) then
           echo " ${MYNAME}: failed, aborting ..."
           exit(1)
        else
           touch $ENSWORK/.DONE_MEM001_ENKFX_${MYNAME}.$yyyymmddhh
        endif

   endif

   # Rename log
   # ----------
   mkdir -p $ENSWORK/updated_ens
   /bin/mv atm_enkf.log $ENSWORK/updated_ens/$expid.$ENKFOUT.log.${nymd}_${hh}z.txt
   
   # When available, store for FSO
   # ------------------------------
   if (-e osense_$yyyymmddhh.dat && $ENKFOUT == "atm_enkf" ) then
       /bin/mv osense_$yyyymmddhh.dat $ENSWORK/updated_ens/$expid.atmens_osens.${nymd}_${hh}z.bin
       ln -sf $ENSWORK/updated_ens/$expid.atmens_osens.${nymd}_${hh}z.bin osense_$yyyymmddhh.dat 
       enkf_obs2ods.x -o $ENSWORK/updated_ens/$expid.atmens_osens.${nymd}_${hh}z.ods osense_${yyyymmddhh}.dat
   endif

endif # if not already done

# if made it hear, make sure EnKF is really done ...
if(! -e $ENSWORK/.DONE_MEM001_ENKFX_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: failed, aborting ..."
   exit(1)
endif

# When Filter-free scheme is running, bypassed EnKF update and reset analyses to those of FFS
# -------------------------------------------------------------------------------------------
if ( $doing_eezy && -e $ENSWORK/.HOLD_${MYNAME}.eezy_ana.$yyyymmddhh ) then

  # Recover filter-free analyses overwriting EnKF analyses
  # NOTE: to restore links, need to remove actual EnKF analyses first
  # -----------------------------------------------------------------
  if (! -d $ENSWORK/Hold_EEZY ) then
     echo " ${MYNAME}: cannot find Hold directory with filter-free analyses, aborting ..."
     exit(1)
  endif
  @ ic = 0
  while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     if( -d $ENSWORK/Hold_EEZY/mem$memtag ) then
        /bin/rm $ENSWORK/mem$memtag/*.ana.eta.*.mem$memtag.$NCSUFFIX
        /bin/mv $ENSWORK/Hold_EEZY/mem$memtag/*.ana.eta.*.mem$memtag.$NCSUFFIX $ENSWORK/mem$memtag/
     else
        echo " ${MYNAME}: cannot find filter-free analysis for member $memtag , aborting ..."
        exit(1)
     endif
  end
  /bin/rm -r $ENSWORK/Hold_EEZY
  /bin/rm $ENSWORK/.HOLD_${MYNAME}.eezy_ana.$yyyymmddhh
  echo " ${MYNAME}: cleaned up directory Hold used to hold filter-free analyses ... "

else

  # Rename each analysis following GMAO naming convension
  # -----------------------------------------------------
  @ ic = 0
  while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     update_ens.csh $expid mem$memtag   ana $ENSWORK/mem${memtag} NULL $NCSUFFIX
  end
  # consistency check:
  if ( $ic != $nmem ) then
    echo " ${MYNAME}: Error, number of ensemble analysis unequal to number of members, aborting ..." 
    exit(1) 
  endif

endif # Filter-free check

# When FSO is requested, these non-inflated analyses will be available
# The following needs to the taken carefully under when using Filter-free,
# but no harm in saving niana ...
set store4fso = `nmlread.py enkf.nml nam_enkf fso_cycling`
if ( $store4fso == "True" ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      update_ens.csh $expid mem$memtag niana $ENSWORK/mem${memtag} NULL $NCSUFFIX
   end
   # consistency check:
   if ( $ic != $nmem ) then
     echo " ${MYNAME}: Error, number of ensemble non-inflated analysis unequal to number of members, aborting ..." 
     exit(1) 
   endif
endif

# Clean up
# --------
if ( ! $ATMENS_DEBUG ) then
   if ( ! -e $ATMENSETC/odsstats_ktonly.rc ) then # if this is present, diag files are needed for ObsImp0hr
      /bin/rm ensmean/diag_*ensmean diag_*ensmean
   endif
   @ ic = 1
   while ( $ic < $nmem + 1 )
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      /bin/rm mem$memtag/diag_*mem$memtag diag_*mem$memtag
      @ ic = $ic + 1
   end
endif

touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
