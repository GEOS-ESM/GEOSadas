#!/bin/csh

# atmos_eaod.csh - invokes the AOD analysis (see usage).
#
# !REVISION HISTORY:
#
#  20Jun2014  Todling   Initial script
#  15Oct2016  Buchard/Todling add option to run GAAS over the members
#                       and the aerosol observer over the members
#  16Mar2017  Todling   Considerable revisions to script
#  20Mar2017  Todling   Allow EnKF to run over available aod/gaas-bkg cases
#                       (as in Rapid Update analysis)
#  26Mar2017  Todling   Allow for EnKF to generate approx analysis kernel
#  31Mar2020  Todling   Jobmonitor to protect against faulty batch-block
#  23Jun2020  Todling   Redef meaning of ATMENSLOC
#--------------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmos_eaod.csh

if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - entry point to ensembole AOD analysis"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms freq "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   freq   -  frequency of AOD analysis, as in HHMMSS"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedures handles the analysis of aerosols in the ensembe DAS. In its simplest form "
   echo "  this procedure makes the central DAS AOD analysis available to each of the members of the   "
   echo "  ensemble."
   echo " "
   echo "  Other options, controled by the env variable AENS_GAAS_OPT (see below) include the "
   echo "  possibility of running the PSAS-based AOD analysis for each member of the ensemble."
   echo " "
   echo "  Additionally, or alternatively, this procedure also allows for using the EnSRF to analyze"
   echo "  aerosols through an ensemble-based approach."
   echo " "
   echo "  When using the EnSRF, the option exists to analyze either AOD or the full 3D concentration "
   echo "  fields. These are controlled directly by the parameters in the EnKF namelist file (not by."
   echo "  this script); this script is, however, capable of automatically recongnizing between the two "
   echo "  options and making adequate decisions from there."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 030000 "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "  ATMENSETC     - location of ensemble RC files     "
   echo "  ATMENSLOC     - location of ensemble members      "
   echo "  FVHOME        - location of experiment            "
   echo "  FVROOT        - location of DAS build             "
   echo "  FVWORK        - location of work directory        "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "  AENKFAERO_NCPUS   - when parallel ens on, this sets NCPUS for AGCM integration"
   echo "  ATMENKFAERO_WALLCLOCK - wall-clock time to run EnKF, default 1:00:00 "
   echo "  ATMENKFAERO_QNAME - name of queue (default: NULL, that is, let pbs pick) "
   echo "  ENSACQ_WALLCLOCK  - wallclock time for acquire job (default: 2:00:00)"
   echo "  ENSPARALLEL       - when set, runs all ensemble components in parallel"
   echo "  GAAS_ANA          - triggers use of AOD analysis (default: 0; no ANA)"
   echo "  AENS_GAAS_OPT     - set options for AOD ensemble analysis: "
   echo "                      1 -> use central analysis for all members (default)"
   echo "                      2 -> run GAAS analysis for each member"
   echo "                      3 -> do (2), and EnKF-based AOD analysis (off aod.or.concentrations)"
   echo "                      4 -> EnKF-based AOD analysis (off aod.or.concentrations)"
   echo "                     (default: 1)"
   echo "  AERO_FROM_ENKF    - when specified will replace PSAS analysis with those from EnKF"
   echo "  NCSUFFIX          - suffix of hdf/netcdf files (default: nc4)"
   echo " "
   echo " RESOURCE FILES"
   echo " "
   echo "  gmao_aero_hybens_info.xNLATyNLONlNLEV.rc - opt rc allowing definition "
   echo "                                             of horizontal and vertical "
   echo "                                             EnKF localization"
   echo "                                             (DEFAULT: see enkf.nml)"
   echo " "
   echo " SEE ALSO"
   echo " "
   echo "   atmos_enkf.csh, run_gaas_ana.csh, calcaod.csh "
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 26Mar2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENSETC)      ) setenv FAILED 1
if ( !($?ATMENSLOC)      ) setenv FAILED 1
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1
if ( !($?GID)            ) setenv FAILED 1

if ( !($?AERO_FROM_ENKF)  ) setenv AERO_FROM_ENKF 0
if ( !($?ENSACQ_WALLCLOCK)) setenv ENSACQ_WALLCLOCK 2:00:00
if ( !($?GAAS_ANA)        ) setenv GAAS_ANA 0
if ( !($?AENS_GAAS_OPT)   ) setenv AENS_GAAS_OPT 1
if ( !($?NCSUFFIX)        ) setenv NCSUFFIX nc4

if ( !($?ENSPARALLEL)      ) setenv ENSPARALLEL 0
if ( !($?ATMENS_DEBUG)     ) setenv ATMENS_DEBUG 0
if ( !($?ATMENKFAERO_MPIPROCS) ) setenv ATMENKFAERO_MPIPROCS 0
if ( !($?ATMENKFAERO_WALLCLOCK)) setenv ATMENKFAERO_WALLCLOCK 1:00:00
if ( !($?ATMENKFAERO_QNAME)    ) setenv ATMENKFAERO_QNAME NULL

if ( ! $GAAS_ANA ) then
  echo " ${MYNAME}: no AOD analysis requested; nothing to do."
  exit 0
endif

if ( $ENSPARALLEL ) then
   if ( !($?AENKFAERO_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $AENKFAERO_NCPUS
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
set freq  = $4
set hh     = `echo $nhms | cut -c1-2`
set yyyymmddhh  = ${nymd}${hh}
set yyyymmdd_hh = ${nymd}_${hh}
@ freq_sec = $freq / 10000
@ freq_sec = $freq_sec * 3600

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

if ( $?FVSPOOL ) then
   set spool = "-s $FVSPOOL "
else
   set diren = `dirname $FVHOME`
   set spool = "-s $diren/spool "
endif

# if so, set prefix of output from EnKF
set anaprefix = ""
if ( $AENS_GAAS_OPT != 4 ) then
   if ( ! $AERO_FROM_ENKF ) then
        set anaprefix = "enkf_"
   endif
endif

# Block GAAS batch runs
setenv AODBLOCKJOB 1

# Get positioned where AOD analysis are brought into
setenv MYDIR $ENSWORK/gaas_ana
mkdir -p $MYDIR
cd $MYDIR

# If so, retrieve AOD analysis from central DAS
# NOTE: intentionally dettached from GAAS_ANA knob.
# In odd case of doing AOD ANA for each member, knob
# that follows should control AOD analysis and
# overwrite retrieved AOD files from central.
# ------------------------------------------------
set chemrc = $ATMENSETC/GEOS_ChemGridComp.rc
if (! -e $chemrc ) then
   echo " ${MYNAME}: cannot find $chemrc, aborting ..."
   exit(1)
endif
set doing_gaas = `echorc.x -rc $chemrc ENABLE_GAAS`
if ( "$doing_gaas" == ".TRUE." ) then
   if ( ! -e  $ENSWORK/.DONE_MEM001_ACQ_${MYNAME}.$yyyymmddhh ) then
      if ( -e $ATMENSETC/aod4aens.acq ) then
         set acqfile = $ATMENSETC/aod4aens.acq
         # retrieve AOD analysis files
         set alog = $MYDIR/aod4aens.log.$yyyymmddhh.txt
         set nstep = 2 # number of AOD analysis available in window (wired);
                       # don't have clear way to specify this presently
         set CRAP = 0
         if ( $CRAP ) then
            setenv FVSPOOL $spool
            wrap_acquire.csh $nymd $nhms $freq $nstep $acqfile aod4aens $alog
         else

         # Launch acquire job to retrieve pre-existing ensemble
         # ----------------------------------------------------
           #setenv JOBGEN_NCPUS          1
           #setenv JOBGEN_NCPUS_PER_NODE 1
           jobgen.pl \
                -expid $expid         \
                acq_aodana            \
                $GID                  \
                $ENSACQ_WALLCLOCK     \
                "acquire -v -rc $acqfile $spool -d $MYDIR -ssh $nymd $nhms $freq $nstep" \
                $ENSWORK              \
                $MYNAME               \
                $ENSWORK/.DONE_MEM001_ACQ_${MYNAME}.$yyyymmddhh \
                "Acquire existing AOD files Failed"
   
                if ( -e acq_aodana.j ) then
                   if ( $ATMENS_BATCHSUB == "sbatch" ) then
                      $ATMENS_BATCHSUB -W acq_aodana.j
                   else
                      $ATMENS_BATCHSUB -W block=true acq_aodana.j
                   endif
                else
                   echo " ${MYNAME}: Failed to generate PBS jobs to acquire existing AOD Analysis. Aborting ... "
                   touch $FVWORK/.FAILED
                   exit(1)
                endif

                # Monitor job in case block fails
                # -------------------------------
                jobmonitor.csh 1 ACQ_${MYNAME}  $ENSWORK $yyyymmddhh
                if ($status) then
                    echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
                    exit(1)
                endif
   
         endif #
      else # if doing gaas, better have corresponding acq file or else exit in error
         echo " ${MYNAME}: expected aod4aens.acq, but not found, aborting ..."
         exit(1)
      endif # rc present
   endif # acquire aod files
else # not running with GAAS: leave merrily
   echo "${MYNAME}: not using GAAS, nothing to do ..."
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
   exit(0)
endif

# Not so robust, but link AOD files to member of the ensemble
set lst_aod_files = (`/bin/ls *.aod_*.$NCSUFFIX`)
if ($status) then
    echo " ${MYNAME}: expected aod analysis files but found none, aborting ..."
    exit(1)
endif

if ( $AENS_GAAS_OPT == 1 ) then
   cd $ENSWORK
   foreach mdir (`/bin/ls -d mem*`)
     set nnn = `echo $mdir | cut -c4-6`
     cd $mdir
     foreach fn ( $lst_aod_files )
       set pfx = `echo $fn | cut -d. -f1-4`
       ln -sf $MYDIR/$fn $pfx.mem$nnn.$NCSUFFIX
     end
     cd -
   end
endif

if ( $AENS_GAAS_OPT > 1 ) then
  set members = `/bin/ls -d $ATMENSLOC/mem* | wc`
  set nmem = $members[1]

# Retrieve AOD-related observations ...
# -------------------------------------
  setenv SAVEWORK $FVWORK
  setenv FVWORK   $MYDIR
  cd $FVWORK

  if ( -e $FVHOME/run/obsys-gaas.rc ) then
     /bin/cp $FVHOME/run/obsys-gaas.rc .
  else
     echo " ${MYNAME}: missing RC file, cannot retrieve AOD obs"
     exit(1)
  endif

  get_aero_obs.csh  $nymd $nhms 1
  set newdate = `tick $nymd $nhms  $freq_sec`
  get_aero_obs.csh  $newdate[1] $newdate[2] 1
   
# OPT==2: Run PSAS analysis on each ensemble member
# -------------------------------------------------
  if ( $AENS_GAAS_OPT == 2 || $AENS_GAAS_OPT == 3 ) then

      echo " Analyzing AOD using PSAS ... "
      @ n = 0
      while ($n < $nmem )
         @ n = $n + 1
         set nnn = `echo $n | awk '{printf "%03d", $1}'`
         if( ! -d mem$nnn ) mkdir mem$nnn
         if ( ! -e $ENSWORK/.DONE_MEM${nnn}_run_gaas_ana.csh.$yyyymmddhh ) then
            cd $MYDIR/mem$nnn
            ln -sf $MYDIR/*ods .
            ln -sf $MYDIR/006  .
            ln -sf $MYDIR/*rc  .
            ln -sf $ATMENSLOC/mem$nnn/*.gaas_bkg.sfc.*$NCSUFFIX  .
            cd -
            run_gaas_ana.csh  $EXPID $nymd $nhms 1 $MYDIR/mem$nnn $ENSWORK/.DONE_MEM${nnn}_run_gaas_ana.csh.$yyyymmddhh &
         else
            echo " ${MYNAME}: AOD analysis already complete for member $nnn"
         endif
      end
      wait

#     Monitor status of ongoing jobs
#     ------------------------------
      jobmonitor.csh $nmem run_gaas_ana.csh $ENSWORK $yyyymmddhh
      if ($status) then
          echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
          exit(1)
      endif

#     Make sure all finished successfully
#     -----------------------------------
      @ n = 0
      while ( $n < $nmem )
         @ n = $n + 1
         set nnn = `echo $n | awk '{printf "%03d", $1}'`
         if ( ! -e $ENSWORK/.DONE_MEM${nnn}_run_gaas_ana.csh.$yyyymmddhh ) then
            echo " ${MYNAME}: AOD analysis not yet available for member $nnn"
            exit(1)
         endif
      end

#     Rename all AOD analysis-related to conventional name and move to location within ENSWORK
#     ----------------------------------------------------------------------------------------
      echo "${MYNAME}: moving PSAS AOD member analyses to ENSWORK ..."
      @ n = 0
      while ($n < $nmem )
         @ n = $n + 1
         set nnn = `echo $n | awk '{printf "%03d", $1}'`
         if( ! -d $MYDIR/mem$nnn   ) mkdir $MYDIR/mem$nnn
         if( ! -d $ENSWORK/mem$nnn ) mkdir $ENSWORK/mem$nnn
         cd $MYDIR/mem$nnn
         if ( $AENS_GAAS_OPT == 2 ) then
            foreach fn ( `/bin/ls $expid.aod_f.sfc.*.$NCSUFFIX ` )
               if(-e $ENSWORK/mem$nnn/$fn) /bin/rm $ENSWORK/mem$nnn/$fn  # make sure to remove existing files: remove so not to overwrite possible links
               /bin/mv $fn $ENSWORK/mem$nnn/$fn
            end
         endif
         foreach fn ( `/bin/ls $expid.aod_[a,d,k].sfc.*.$NCSUFFIX ` )
            set pfx = `echo $fn | cut -d. -f1-4`
            set rname = $pfx.mem$nnn.$NCSUFFIX
            if(-e $ENSWORK/mem$nnn/$rname) /bin/rm $ENSWORK/mem$nnn/$rname  # make sure to remove existing files: remove so not to overwrite possible links
            /bin/mv $fn $ENSWORK/mem$nnn/$rname
         end
         cd -
      end

#     Now proceed with usual update by moving AOD files to updated_ens directory
#     and then link the files to the member locations of the present cycle
#     --------------------------------------------------------------------------
      echo "${MYNAME}: moving PSAS AOD member analyses to updated_ens ..."
      @ n = 0
      while ($n < $nmem )
         @ n = $n + 1
         set nnn = `echo $n | awk '{printf "%03d", $1}'`
         if( ! -d mem$nnn ) mkdir mem$nnn
         cd $MYDIR/mem$nnn
         update_ens.csh $expid mem$nnn aaero $ENSWORK/mem${nnn} NULL $NCSUFFIX
         cd -
      end

#     First move all AOD-related files to main member location within ENSWORK
#     -----------------------------------------------------------------------
      if ( $AENS_GAAS_OPT == 3 && $AERO_FROM_ENKF ) then   # in case EnKF analysis to be used
         cd $ENSWORK/updated_ens
         @ n = 0
         while ($n < $nmem )
            @ n = $n + 1
            set nnn = `echo $n | awk '{printf "%03d", $1}'`
            cd mem$nnn
            foreach fn ( `/bin/ls $expid.aod_[a,d,k].sfc.*.$NCSUFFIX ` )
               /bin/mv $fn psas.$fn  #    move PSAS analyses out of the way
            end
            cd -
         end
         cd $MYDIR
         # Note: at this point all links in ENSWORK/memNNN to AOD files will have become dummy
         #       running the EnKF will re-establish the links
      endif

  endif # AENS_GAAS_OPT=2 || 3

# OPT==3: EnKF-based AOD ensemble analysis
# ----------------------------------------
  if ( $AENS_GAAS_OPT == 3 || $AENS_GAAS_OPT == 4 ) then

      # All work happens in own directory
      if( ! -d $MYDIR/ensmean ) mkdir $MYDIR/ensmean
      cd $MYDIR/ensmean
    
       echo ""
       echo "${MYNAME}: Analyzing AOD using EnKF"
       echo ""

       # Apply AOD observer to the mean of the ensemble
       # ----------------------------------------------
       if ( ! -e $ENSWORK/.DONE_ENSMEAN_run_gaas_ana.csh.$yyyymmddhh ) then
          ln -sf $ATMENSLOC/ensmean/$expid.bkg.eta.*z.$NCSUFFIX .
          ln -sf $ATMENSLOC/ensmean/*.gaas_bkg.sfc.*$NCSUFFIX .
          ln -sf $ATMENSLOC/ensmean/*.abkg.eta.*$NCSUFFIX .
          ln -sf $MYDIR/*ods .
          ln -sf $MYDIR/*rc  .
          ln -sf $MYDIR/006  .

          # run observer for current member
          run_gaas_ana.csh  $EXPID $nymd $nhms 1 $MYDIR/ensmean $ENSWORK/.DONE_ENSMEAN_run_gaas_ana.csh.$yyyymmddhh

          # rename PSAS output ...
          foreach fn ( `/bin/ls $expid.aod_[a,d,k].sfc.*.$NCSUFFIX ` )
               /bin/mv $fn psas.$fn               #    move PSAS analyses out of the way
          end

          # re-run observer only to get AOD instead of log(AOD)
          cd aod.$nymd.$nhms
          /bin/cp $ATMENSETC/ana_aodmean.rc  .
          /bin/cp $ATMENSETC/obs_aodmean.rc obs.rc
          ln -sf ../*aod.obs*ods .
          foreach fn ( `/bin/ls *.aod.obs*ods` )
             set sfx   = `echo $fn   | cut -d. -f4-`
             set tyme  = `echo $sfx  | cut -d. -f1 | cut -c1-11`
             set enymd = `echo $tyme | cut -c1-8`
             set ehh   = `echo $tyme | cut -c10-11`
             set gaas_fn = `/bin/ls *gaas_bkg.sfc.${tyme}*`
             ln -sf $fn ensmean_logaod_post_anal.obs.ods
             ana_aod.x -rc ana_aodmean.rc -o ../$expid.ensmean_aod.obs.${enymd}_${ehh}z.ods $gaas_fn > aod_ensmean.log
          end

          cd $MYDIR/ensmean
       else
          echo " ${MYNAME}: AOD mean observer completed."
       endif
 
       echo "${MYNAME}: running AOD member observers ..."
       set odsfn = ( `/bin/ls *.ensmean_aod.obs*.*ods` )
       cd $MYDIR
       /bin/rm mem*/*aod_a*.$NCSUFFIX
       /bin/rm mem*/*aod_d*.$NCSUFFIX
       /bin/rm mem*/*aod_k*.$NCSUFFIX
       @ n = 0
       while ( $n < $nmem )
          @ n = $n + 1
          set nnn = `echo $n | awk '{printf "%03d", $1}'`

          if( ! -d mem$nnn ) mkdir mem$nnn
          cd mem$nnn
          if(   -d aod.$nymd.$nhms ) /bin/rm -r aod.$nymd.$nhms
          mkdir -p aod.$nymd.$nhms
          foreach fn ( $odsfn )
             set sfx   = `echo $fn   | cut -d. -f4-`
             set tyme  = `echo $sfx  | cut -d. -f1 | cut -c1-11`
             set enymd = `echo $tyme | cut -c1-8`
             set ehh   = `echo $tyme | cut -c10-11`

             # link background AOD files into member directory
             foreach gaas_fn (`/bin/ls $ENSWORK/mem$nnn/*gaas_bkg.sfc.${tyme}*`)
                ln -sf $gaas_fn .
             end

             # link background aerosol 3d fields into member directories
             foreach abkg_fn (`/bin/ls $ENSWORK/mem$nnn/*.abkg.eta.${tyme}*`)
                ln -sf $abkg_fn .
             end

             cd aod.$nymd.$nhms   # get positioned where member observer will run

             # link in ens-mean observation file
             ln -sf $MYDIR/ensmean/$fn ensmean_aod_post_anal.obs.ods

             # Copy GAAS resources files needed to run observer
             /bin/cp $FVHOME/run/gaas/GAAS_AerRegistry.rc .
             /bin/cp $FVHOME/run/gaas/GAAS_AodRegistry.rc .
             /bin/cp $FVHOME/run/gaas/gaas.rc .

             # run observer for this member
             if ( ! -e $ENSWORK/.DONE_MEM${nnn}_gaas_observer.csh.${enymd}${ehh} ) then
                # copy/overwrite specific resources
                if ( -e $ATMENSETC/obs_aodmember.rc ) then 
                   /bin/cp $ATMENSETC/obs_aodmember.rc obs.rc
                else
                  echo " ${MYNAME}: cannot find obs_aodens.rc, aborting ..."
                  exit(1)
                endif
                if ( -e $ATMENSETC/ana_aodmember.rc ) then 
                   ln -sf $ATMENSETC/ana_aodmember.rc .          
                else
                  echo " ${MYNAME}: cannot find ana_aodmember.rc, aborting ..."
                  exit(1)
                endif
                # run observer for current member
                set gaas_fn = `/bin/ls ../*gaas_bkg.sfc.${tyme}*`
                set aod_f   = $expid.aod_f.sfc.${enymd}_${ehh}00z.$NCSUFFIX
                set aod_o   = aer_omf.ext_Nc.${enymd}_${ehh}z.ods
                if ( -e $aod_f ) /bin/rm $aod_f
                if ( -e $aod_o ) /bin/rm $aod_o
                ana_aod.x -rc ana_aodmember.rc -f $aod_f -o $aod_o $gaas_fn > aod.${tyme}.log 
                if ( ! -e ANAAOD_EGRESS ) then  
                   echo " ${MYNAME}: ANAAOD - observer - failed, aborting ..."
                   exit(1)
                endif
                /bin/mv $aod_f ../
                /bin/mv $aod_o ../
                touch $ENSWORK/.DONE_MEM${nnn}_gaas_observer.csh.${enymd}${ehh}
             endif

             cd - # back to member directory

          end # loop over available ODS observer files
          cd $MYDIR
       end  # loop over ensemble members
 
       echo ""
       echo "${MYNAME}: Analyzing full 3D-aerosol fields using EnKF"
       echo ""

       # Back to GAAS work directory
       # ---------------------------
       cd $MYDIR

       # Loop over synoptic cases
       # ------------------------
       foreach fn ( $odsfn )
          set sfx   = `echo $fn   | cut -d. -f4-`
          set tyme  = `echo $sfx  | cut -d. -f1 | cut -c1-11`
          set enymd = `echo $tyme | cut -c1-8`
          set ehh   = `echo $tyme | cut -c10-11`
          set eyyyymmddhh  = ${enymd}${ehh}

          foreach kernel (  ".false." ".true." )
            set xtag = ""
            if ( $kernel == ".true." ) set xtag = "KERNEL"

            if ( ! -e $ENSWORK/.DONE_MEM001_ENKFXAERO${xtag}_${MYNAME}.$eyyyymmddhh ) then

               # Determine lat/lon
               # -----------------
               set mean_eta_file =  ensmean/$expid.abkg.eta.${enymd}_${ehh}z.$NCSUFFIX
               if ( ! -e $mean_eta_file ) then
                  echo " ${MYNAME}: cannot determine dims of aero-analysis, aborting ..."
                  exit(1)
               endif
               set ens_mres  = `getgfiodim.x $mean_eta_file`
               set ens_nlons = $ens_mres[1]
               set ens_nlats = $ens_mres[2]
               set ens_nlevs = $ens_mres[3]

               # Look to see if need use ensemble scales different than those of central
               # -----------------------------------------------------------------------
               if ( -e $ATMENSETC/gmao_aero_hybens_info.x${ens_nlons}y${ens_nlats}l${ens_nlevs}.rc ) then
                    if ( -e hybens_locinfo ) /bin/rm hybens_info
                    ln -s $ATMENSETC/gmao_aero_hybens_info.x${ens_nlons}y${ens_nlats}l${ens_nlevs}.rc hybens_info
               else
                 if ( -e $FVHOME/run/gmao_global_hybens_info.x${ens_nlons}y${ens_nlats}l${ens_nlevs}.rc ) then
                    if ( -e hybens_locinfo ) /bin/rm hybens_info
                    ln -s $FVHOME/run/gmao_global_hybens_info.x${ens_nlons}y${ens_nlats}l${ens_nlevs}.rc hybens_info
                    echo "${MYNAME}: using ensemble-specific loc scales "
                 endif
               endif

               # parameters needed for aer in enkf.nml file
               # ------------------------------------------
               /bin/rm -f sed_file
               echo "s/>>>EXPID<<</${expid}/1"            > sed_file
               echo "s/>>>YYYYMMDDHH<<</${eyyyymmddhh}/1">> sed_file  # need to add 2nd time
               echo "s/>>>ENS_NLATS<<</${ens_nlats}/1"   >> sed_file
               echo "s/>>>ENS_NLONS<<</${ens_nlons}/1"   >> sed_file
               echo "s/>>>ENS_NLEVS<<</${ens_nlevs}/1"   >> sed_file
               echo "s/>>>NMEM<<</${nmem}/1"             >> sed_file
               echo "s/>>>DO_AVK<<</${kernel}/1"         >> sed_file
               /bin/rm -f ./enkf.nml
               sed -f sed_file  $ATMENSETC/aero_enkf_nml.tmpl  > ./enkf.nml

               # Run ensemble
               # ------------
               echo "${MYNAME}: running EnKF-aero analysis at ${eyyyymmddhh} ..."
               if( $ENSPARALLEL ) then
   
                  if ( $ATMENKFAERO_MPIPROCS ) then
                     set mpiprocs = "-mpiprocs $ATMENKFAERO_MPIPROCS"
                  else
                     set mpiprocs = ""
                  endif
                  jobgen.pl \
                       -egress aenkfaero${xtag}.$eyyyymmddhh.log    \
                       -q $ATMENKFAERO_QNAME $mpiprocs  \
                       aenkfaero${xtag}    \
                       $GID                \
                       $ATMENKFAERO_WALLCLOCK  \
                       "$MPIRUN_ATMENKFAERO |& tee -a $MYDIR/aenkfaero${xtag}.$eyyyymmddhh.log" \
                       $MYDIR              \
                       $MYNAME             \
                       $ENSWORK/.DONE_MEM001_ENKFXAERO${xtag}_${MYNAME}.$eyyyymmddhh \
                       "AtmosEnKFaero Failed"
  
                       if ( -e aenkfaero.j ) then
                          if ( $ATMENS_BATCHSUB == "sbatch" ) then
                             $ATMENS_BATCHSUB -W aenkfaero${xtag}.j
                          else
                             $ATMENS_BATCHSUB -W block=true aenkfaero${xtag}.j
                          endif
                       else
                          echo " ${MYNAME}: Failed to generate PBS jobs for AtmEnKFAero, Aborting ... "
                          exit(1)
                       endif

                       # Monitor job in case block fails
                       # -------------------------------
                       jobmonitor.csh 1 ENKFXAERO${xtag}_${MYNAME}  $ENSWORK $yyyymmddhh
                       if ($status) then
                           echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
                           exit(1)
                       endif

               else

                  $MPIRUN_ATMENKFAERO |& tee -a $ENSWORK/aenkfaero${xtag}.$eyyyymmddhh.log
                  if ( $status ) then
                     echo " ${MYNAME}: failed, aborting ..."
                     exit(1)
                  else
                     touch $ENSWORK/.DONE_MEM001_ENKFXAERO${xtag}_${MYNAME}.$eyyyymmddhh
                  endif

               endif # ENSPARALLEL

            endif # ENKFAERO

            # At this point, EnKF-AERO better have ran successfully
            # -----------------------------------------------------
            if(! -e $ENSWORK/.DONE_MEM001_ENKFXAERO${xtag}_${MYNAME}.$eyyyymmddhh ) then
               echo " ${MYNAME}: failed, aborting ..."
               exit(1)
            endif

          end # Kernel

       end # loop over syn cases

#      Derive AOD from Concentrations for diagnostic purposes 
#      ------------------------------------------------------
       cd $MYDIR/mem001
       set lst_aana_eta = `/bin/ls *.aana.eta.*`
       cd $MYDIR
       foreach concfn ( $lst_aana_eta )
          set pfx   = `echo $concfn | cut -d. -f1-3`
          set enymd = `echo $concfn | cut -d. -f4 | cut -c1-8`
          set ehh   = `echo $concfn | cut -d. -f4 | cut -c10-11`
          set enhms = ${ehh}0000
          atmens_calcaod.csh  $expid $enymd $enhms aana aod_a $MYDIR 
          if ($status) then
             echo "${MYNAME}: error calculating AOD from Concentrations, aborting ..."
             exit(3)
          endif
       end

#      First move all AOD-related files to main member location within ENSWORK
#      -----------------------------------------------------------------------
       @ n = 0
       echo "${MYNAME}: moving EnKF aero analysis to ENSWORK/mem directories ..."
       while ($n < $nmem )
          @ n = $n + 1
          set nnn = `echo $n | awk '{printf "%03d", $1}'`
          if( ! -d mem$nnn ) mkdir mem$nnn
          if( ! -d $ENSWORK/mem$nnn ) mkdir -p $ENSWORK/mem$nnn
          cd $MYDIR/mem$nnn
          if ( $AENS_GAAS_OPT > 2  ) then
             foreach fn ( `/bin/ls $expid.aod_f.sfc.*.$NCSUFFIX` )
                set pfx = `echo $fn | cut -d. -f1-4`
                if( -e $ENSWORK/mem$nnn/$fn) /bin/rm $fn $ENSWORK/mem$nnn/$fn
                /bin/mv $fn $ENSWORK/mem$nnn/$pfx.mem$nnn.$NCSUFFIX
             end
          endif
          foreach fn ( `/bin/ls $expid.aod_[a,d,k].sfc.*.mem$nnn.$NCSUFFIX` )
             if( ${anaprefix} != "enkf_" && -e $ENSWORK/mem$nnn/$fn ) /bin/rm $ENSWORK/mem$nnn/$fn  # make sure to remove existing files: remove so not to overwrite possible links
             /bin/mv $fn $ENSWORK/mem$nnn/${anaprefix}$fn
          end
          foreach fn ( `/bin/ls $expid.aana.eta.*.mem$nnn.$NCSUFFIX $expid.aker.eta.*.mem$nnn.$NCSUFFIX` )
             if( ${anaprefix} != "enkf_" && -e $ENSWORK/mem$nnn/$fn ) /bin/rm $ENSWORK/mem$nnn/$fn  # make sure to remove existing files: remove so not to overwrite possible links
             /bin/mv $fn $ENSWORK/mem$nnn/${anaprefix}$fn
          end
          cd -
       end

#      Now proceed with usual update by moving AOD files to updated_ens directory
#      and then link the files to the member locations of the present cycle
#      --------------------------------------------------------------------------
       foreach upd_this ( "aaero" "aconc" )
          echo "${MYNAME}: moving EnKF $upd_this analysis to update_ens/mem directories ..."
          @ n = 0
          while ($n < $nmem )
             @ n = $n + 1
             set nnn = `echo $n | awk '{printf "%03d", $1}'`
             if( ! -d mem$nnn ) mkdir mem$nnn
             cd $MYDIR/mem$nnn
             update_ens.csh ${anaprefix}$expid mem$nnn $upd_this $ENSWORK/mem${nnn} NULL $NCSUFFIX
             cd -
          end
       end # upd_this (aod/conc)

  endif # OPT==3 || OPT==4

endif # OPT>1

# If made it made it this far, should be fine
# -------------------------------------------
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)

