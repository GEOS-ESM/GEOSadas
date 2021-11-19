#!/bin/csh

# gcm_ensemble - runs ensemble of atmospheric GCMs
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  05Nov2011  Todling   Parallelized
#  11Nov2011  Todling   Each forecast has now its own rsts
#  15Nov2011  Todling   Revisit calc of mean/rms - via script now
#  21Nov2011  Todling   Add rsts bootstrap capability
#  31Jan2012  Todling   Add rsts regrid capability
#  22Apr2012  Todling   Updates to recent configuration of GCM
#  25May2012  Todling   Updates to Ganymed-1_0 GCM
#  05Oct2012  Todling   Move archiving and positioning of updated ens to main
#  20Oct2012  Todling   Move stats calculation to post_egcm
#  28Jan2013  Todling   - Add handle to allow extending fcst beyond cycle interval
#                       - Add handle to allow output at final time
#  12Mar2013  Todling   Implement options for distribute multi-work jobs
#  23Mar2013  Todling   Implement opt to link rst from central in case of easy-ana
#  28Feb2014  El Akkraoui  Revisit logic for re-submit of distributed jobs
#  15Sep2014  Todling   Add memtag to AGCM.rc for replay mode
#  10Aug2016  RT/EL Akkraoui Copy fv-layout to member directory
#  10Aug2016  Todling   Allow ensemble to have different fv-layout if desired
#  03Mar2017  Todling   Trigger for 4DIAU
#  21Mar2017  Todling   Edit GAAS_GridComp to set member AOD analysis when applicable
#  13Apr2017  Todling   Add knob for GEOS EPS
#  04Aug2018  Todling   Revisit/Update regridding option/mechanism
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#  03May2020  Todling   Logic not to over-subscribe node
#  22Jun2020  Todling   Add ability to run a control member (also cleaned up)
#-------------------------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME gcm_ensemble.csh

if ( $#argv < 6 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - run multiple copies of (atmospheric) GCM "
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms tfcst nlons nlats"
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  initial date of forecast, as in YYYYMMDD "
   echo "   time   -  initial time of forecast, as HHMMSS"
   echo "   tfcst  -  forecast length in hours"
   echo "   nlons  -  number of longitudes in bkg (im) (for history)"
   echo "   nlats  -  number of latitudes  in bkg (jm) (for history)"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "   This procedure runs multiple copies of the, presently "
   echo "   atmospheric, GCM. These integrations are, in principle, "
   echo "   forced with an ensemble of IAU-increments)."
   echo " "
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091018 210000 12 144 91"
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo "   CAP.rc.tmpl      - determine length of integration"
   echo "   AGCM.rc.tmpl     - defines specific restarts, and GCM parameters"
   echo "   HISTAENS.rc.tmpl - defines output of ensemble of GCMs"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of resource files        "
   echo "    ATMENSLOC     - location of current ensemble      "
   echo "    ASYNBKG       - frequency of background (minutes) "
   echo "    FVBCS         - location of fvInput               "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - location of work directory        "
   echo "    GID           - group ID to run job under         "
   echo "    MPIRUN_ENSGCM - define mpi command for GEOSgcm.x  "
   echo "    RSTSTAGE4AENS - location of restarts              "
   echo "    TIMEINC       - analysis frequency (minutes)      "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMGEPS        - trigger for GEOS EPS              "
   echo "    NCSUFFIX       - suffix of hdf/netcdf files (default: nc4)"
   echo "    ENSPARALLEL    - when set, runs all ensemble components in parallel "
   echo "                     (default: off)"
   echo "    ENSGCM_NCPUS   - when parallel ens on, this sets NCPUS for AGCM integration"
   echo "    ENSCTRLONLY    - allows running control member only"
   echo "    AENS_GCM_DSTJOB- distribute multiple works within smaller jobs"
   echo "    AGCM_WALLCLOCK - wall clock time to run agcm, default 1:00:00 "
   echo "    AGCM_QNAME     - name of queue (default: NULL, that is, let BATCH pick) "
   echo "    ATMENS_DO4DIAU - trigger to run 4DIAU "
   echo "    REGRID_QOS     - qos for regrid"
   echo "    RSTEXT         - defines extension for model rst files: nc4 or bin"
   echo " "
   echo " REMARKS"
   echo " "
   echo "   1. When atmens_rst_regrid.rc is present in ATMENSETC"
   echo "      this script will regrid the restart in that file "
   echo "      to the desired resolution; these will not be recycled."
   echo "   2. When running GEOS EPS the length of forecast and history"
   echo "      can be controlled by dropping the following two files:"
   echo "          CAP_hh.rc.tmpl (or simply CAP.rc.tmpl)"
   echo "          HISTAGEPS_hh.rc.tmpl (or simply HISTAGEPS.rc.tmpl)"
   echo "      in the FVHOME/run/ageps directory."
   echo " "
   echo " SEE ALSO"
   echo "   atmos_ens2gcm.csh - calculation of IAU increments"
   echo "   atm_ens_geps.j    - main job script controlling GEPS"
   echo "   gcm_ensset_rc.csh - set resource files"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 20Apr2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?ATMENSLOC)     ) setenv FAILED 1
if ( !($?ASYNBKG)       ) setenv FAILED 1
if ( !($?FVBCS)         ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1
if ( !($?HYBRIDGSI)     ) setenv FAILED 1
if ( !($?REGRID_QOS)    ) setenv REGRID_QOS compute
if ( !($?RSTEXT)        ) setenv RSTEXT bin
if ( !($?RSTSTAGE4AENS) ) setenv FAILED 1
if ( !($?TIMEINC)       ) setenv FAILED 1

if ( !($?ATMGEPS)       ) setenv ATMGEPS 0
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)   ) setenv ENSPARALLEL 0
if ( !($?ENSCTRLONLY)   ) setenv ENSCTRLONLY 0
if ( !($?AENS_GCM_DSTJOB) ) setenv AENS_GCM_DSTJOB 0
if ( !($?AGCM_WALLCLOCK)) setenv AGCM_WALLCLOCK 1:00:00
if ( !($?AGCM_QNAME)    ) setenv AGCM_QNAME NULL
if ( !($?ATMENS_DO4DIAU)) setenv ATMENS_DO4DIAU 0
if ( !($?ATMENS_IGNORE_CHKPNT)) setenv ATMENS_IGNORE_CHKPNT 0

if ( $ENSPARALLEL ) then
   if ( !($?MPIRUN_ENSGCM) ) setenv FAILED 1
   if ( !($?ENSGCM_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $ENSGCM_NCPUS
   endif
   if ( !($?ENSGCM_NCPUS_PER_NODE) ) then
     if ( $ENSGCM_NCPUS_PER_NODE > 0 ) then
        setenv JOBGEN_NCPUS_PER_NODE $ENSGCM_NCPUS_PER_NODE
     endif
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymdb = $2
set nhmsb = $3
set tfcst = $4
set nlons = $5
set nlats = $6
set hhb   = `echo $nhmsb | cut -c1-2`
set yyyymmddhh = ${nymdb}${hhb}

setenv ENSWORK $FVWORK
if (-e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif
setenv EXPID $expid  # this variable is usually defined, but make sure since vED needs it!

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

@ tfcst_sc = $tfcst * 3600
set enddate = `tick $nymdb $nhmsb  $tfcst_sc`
   set nymde = $enddate[1]
   set nhmse = $enddate[2]
   set hhe   = `echo $nhmse | cut -c1-2`

@ ioskip = $TIMEINC * 60 # TBD: this is ok for IAU, but is it general enough?
set iobeg = `tick $nymdb $nhmsb  $ioskip`
    set ionymdb = $iobeg[1]
    set ionhmsb = $iobeg[2]
    set iohhb   = `echo $iobeg[2] | cut -c1-2`
    set ionymde = $nymde
    set ionhmse = $nhmse

set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]

# Quick checks
# ------------
 which lnbcs_ens
 if( $status ) then
     echo " ${MYNAME}: cannot find lnbcs_ens (should be in $FVHOME/run, aborting ..."
     exit(1)
 endif

# Get positioned inside ENSWORK
# -----------------------------
cd  $ENSWORK
touch .no_archiving

@ bkgfreq_hr  =  $ASYNBKG / 60
@ bkgfreq_mn  =  $ASYNBKG - $bkgfreq_hr * 60
set bkgfreq_hh = `echo $bkgfreq_hr |awk '{printf "%02d", $1}'`
set bkgfreq_mm = `echo $bkgfreq_mn |awk '{printf "%02d", $1}'`
set bkgfreq_hhmn = ${bkgfreq_hh}${bkgfreq_mm}
set bkgfreq_nhms = ${bkgfreq_hhmn}00

setenv LINK_RST $HYBRIDGSI 

# Clean up: make sure no bkg.eta/sfc files exist in the mem$memtag directories
# ----------------------------------------------------------------------------
# TBD be careful ...

# Check on need to regrid
# -----------------------
if (-e $ATMENSETC/atmens_rst_regrid.rc ) then
    if( -e $ENSWORK/.DONE_RST_REGRID ) /bin/rm $ENSWORK/.DONE_RST_REGRID
    set grs_regrid = ( `grep internal_rst $ATMENSETC/atmens_rst_regrid.rc  | grep -vE "^[ ]*\#" | cut -d: -f2 | sed -e's/^[ ]*\(.*[^ ]\)[ ]*$/\1/' -e 's/_rst//'` )

    # loop over regridded files and link them to sub-workdir
    # ------------------------------------------------------
    mkdir -p $ENSWORK/rst2regrid
    cd $ENSWORK/rst2regrid
    set rstotrgd = `echo $#grs_regrid`
    @ id = 0
    while ( $id < $rstotrgd )
         @ id++
         /bin/ln -sf $RSTSTAGE4AENS/*.$grs_regrid[$id]_rst.${nymdb}_${hhb}z.$RSTEXT .
    end

    # now regrid
    # ----------
    set inpdir   = $ENSWORK/rst2regrid
    set outdir   = $ENSWORK/rst2regrid/new
    set rstver   = `echorc.x -rc $ATMENSETC/atmens_rst_regrid.rc BC_TAG`
    set ens_nlon = `echorc.x -rc $ATMENSETC/AGCM.rc.tmpl AGCM_IM`
    set ens_nlat = `echorc.x -rc $ATMENSETC/AGCM.rc.tmpl AGCM_JM`
    set ens_nlev = `echorc.x -rc $ATMENSETC/AGCM.rc.tmpl AGCM_LM`
    @ six_nlon = 6 * $ens_lon
    if ( $ens_nlat == $six_nlat ) then
      set eres = C$ens_lon
    else
      if ( $ens_lon ==   72 ) set eres = "a"
      if ( $ens_lon ==  144 ) set eres = "b"
      if ( $ens_lon ==  288 ) set eres = "c"
      if ( $ens_lon ==  576 ) set eres = "d"
      if ( $ens_lon == 1152 ) set eres = "e"
      if ( $ens_lon == 2304 ) set eres = "f"
    endif
    # note: the following is not designed to convert from old to new version of RSTs
    regrid.pl -qos $REGRID_QOS -ymd $nymdb -hr $hhb -grout $eres -levsout $ens_nlev -outdir $outdir -d $inpdir \
              -nobkg -nolbl -nolcv \
              -expid $EXPID -tagin $rstver -oceanin CS -tagout $rstver -rs 3 -oceanout CS -grpID $GID -zoom 4 -np
    if ($status) then
         echo " ${MYNAME}: Failed to regrid RST files, Aborting ... "
         exit(1)
    else
         @ id = 0
         mkdir $ENSWORK/rst2regrid/Hold
         while ( $id < $rstotrgd ) # clean up: remove links
              @ id++
              /bin/mv $ENSWORK/rst2regrid/*$grs_regrid[$id]_rst*.$RSTEXT $ENSWORK/rst2regrid/Hold/
              /bin/mv $ENSWORK/rst2regrid/new/*$grs_regrid[$id]_rst* $ENSWORK/rst2regrid/
         end
         touch $ENSWORK/.DONE_RST_REGRID
    endif
    cd $ENSWORK

else # no regridding ...

    # check to see if doing easy case (simplified scheme)
    # ---------------------------------------------------
    if ( -e $ATMENSETC/easyeana.rc ) then
       # check resolution
       set ens_res = `echorc.x -rc $ATMENSETC/easyeana.rc ensemble_resolution`
       set cnt_res = `echorc.x -rc $ATMENSETC/easyeana.rc central_das_resolution`
       # if ensemble and central run at same resolution ...
       #  simply link RSTs from central, i.e., all members begin from same RSTs
       # ----------------------------------------------------------------------
       if ( "$ens_res" == "$cnt_res" ) then
           setenv LINK_RST $RSTSTAGE4AENS
       endif
    endif

endif

# When a control member is participating ...
# ------------------------------------------
if ( -d $ENSWORK/ensctrl ) then
   if (! -e $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh ) then
    
      # set up GCM resources and input data
      # -----------------------------------
      gcm_ensset_rc.csh $expid $nymdb $nhmsb $tfcst $nlons $nlats ensctrl
    
      cd $ENSWORK/ensctrl

      if( -e cap_restart ) /bin/rm cap_restart
      echo $nymdb $nhmsb > cap_restart

      # Run model
      # ---------
      if( $ENSPARALLEL ) then
    
          jobgen.pl \
               -egress EGRESS -q $AGCM_QNAME \
               -xc "update_ens.csh $expid ensctrl bkg $ENSWORK/ensctrl NULL $NCSUFFIX" \
               agcm_ensctrl          \
               $GID                  \
               $AGCM_WALLCLOCK       \
               "$MPIRUN_ENSGCM |& tee -a $ENSWORK/agcm_ensctrl.log" \
               $ENSWORK/ensctrl \
               $MYNAME               \
               $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh \
               "Atmos GCM Failed"
    
          if ( -e agcm_ensctrl.j ) then
             $ATMENS_BATCHSUB agcm_ensctrl.j
             touch $ENSWORK/.SUBMITTED
          else
             echo " ${MYNAME}: Failed to generate BATCH job for control Atmos GCM, Aborting ... "
             exit(1)
          endif

          # Monitor job in case block fails
          # -------------------------------
          jobmonitor.csh 1 ENSCTRL_${MYNAME} $ENSWORK $yyyymmddhh
          if ($status) then
             echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
             exit(1)
          endif
    
     else
    
          $MPIRUN_ENSGCM |& tee -a $ENSWORK/agcm_ensctrl.log
          /bin/ls ./EGRESS
          set model_status = $status
          if ( $model_status ) then
             echo " ${MYNAME}: failed to run GCM for control, aborting ..."
             touch $ENSWORK/.FAILED
             exit(1)
          endif
          touch $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh
    
          # Store updated ensemble
          # ----------------------
          update_ens.csh $expid ensctrl bkg $ENSWORK/ensctrl NULL $NCSUFFIX
    
     endif

     # Reposition GCM diagnostics
     # --------------------------
     update_ens.csh $expid ensctrl diag $ENSWORK/ensctrl HISTORY.rc $NCSUFFIX

     if(! -e $ENSWORK/.DONE_UPDATE_RST_ENSCTRL.$yyyymmddhh ) then
        gcm_ensrst_wrap.csh $expid $ionymdb $ionhmsb ensctrl
        if($status) then
          echo "${MYNAME}: error wrapping up control model run output, aborting"
          exit(1)
        else
           touch $ENSWORK/.DONE_UPDATE_RST_ENSCTRL.$yyyymmddhh
        endif
     endif
    
     cd -
  endif

  # In only control member begin exercised, all done
  if ( $ENSCTRLONLY ) then
     if ( -e $ENSWORK/.DONE_UPDATE_RST_ENSCTRL.$yyyymmddhh ) then
        touch $ENSWORK/.DONE_ENSFCST
     endif
  endif
endif

# If not all done yet ...
# -----------------------
if(! -e .DONE_ENSFCST ) then
  # Loop over members
  # REMARK: For parallelization purposes the inside of the
  #         loop below should moved into a separate script.
  # -------------------------------------------------------
  /bin/rm $ENSWORK/agcm_poe.*
  /bin/rm $ENSWORK/agcm_machfile*

  if ( $ENSPARALLEL ) then
     set nfiles = `/bin/ls $ENSWORK/.DONE_MEM*_${MYNAME}.$yyyymmddhh | grep -v ENSCTRL | wc -l`
     echo "${MYNAME}: number of already available files  ${nfiles}"
     @ ntodo = $nmem - $nfiles 
  endif 
  @ ipoe = 0
  @ npoe = 0
  @ fpoe = 0
  @ ic = 0
  while ( $ic < $nmem )

     # Get positioned
     # --------------
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     cd mem${memtag}

     if(! -e $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh ) then

        gcm_ensset_rc.csh $expid $nymdb $nhmsb $tfcst $nlons $nlats mem$memtag

        # Run ensemble
        # ------------
        if( -e cap_restart ) /bin/rm cap_restart
        echo $nymdb $nhmsb > cap_restart

        if( $ENSPARALLEL ) then
             @ fpoe++
             if ( $AENS_GCM_DSTJOB != 0 ) then # case of multiple jobs within few larger ones
                # collect multiple gcm calls into jumbo file
                if ( $ipoe < $AENS_GCM_DSTJOB ) then # nmem better devide by AENS_GCM_DSTJOB
                   @ ipoe++
                   set this_script_name = `pwd`/agcm_mem${memtag}.j
                   echo $this_script_name >> $ENSWORK/agcm_poe.$npoe
                   chmod +x $ENSWORK/agcm_poe.$npoe
                endif
                set machfile = "-machfile $ENSWORK/agcm_machfile$npoe.$ipoe"
             else
                set machfile = ""
             endif

             jobgen.pl \
                  -egress EGRESS -q $AGCM_QNAME $machfile \
                  -xc "update_ens.csh $expid mem$memtag bkg $ENSWORK/mem${memtag} NULL $NCSUFFIX" \
                  agcm_mem${memtag}     \
                  $GID                  \
                  $AGCM_WALLCLOCK       \
                  "$MPIRUN_ENSGCM |& tee -a $ENSWORK/agcm_mem${memtag}.log" \
                  $ENSWORK/mem${memtag} \
                  $MYNAME               \
                  $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh \
                  "Atmos GCM Failed"


             if ( $AENS_GCM_DSTJOB != 0 ) then
                if ( -e agcm_mem${memtag}.j ) then
                   chmod +x agcm_mem${memtag}.j
                else
                   echo " ${MYNAME}: AGCM Failed to generate BATCH jobs for Member ${memtag}, Aborting ... "
                   touch $ENSWORK/.FAILED
                   exit(1)
                endif

                if ( ($ipoe == $AENS_GCM_DSTJOB) || (($fpoe == $ntodo) && ($ipoe < $AENS_GCM_DSTJOB) ) ) then
                   set this_ntasks_per_node = `facter processorcount`
                   @ ncores_needed = $ENSGCM_NCPUS / $this_ntasks_per_node
                   if ( $ncores_needed == 0 ) then
                     @ myncpus = $this_ntasks_per_node
                   else
                     if ( $ENSGCM_NCPUS == $ncores_needed * $this_ntasks_per_node ) then
                        @ myncpus = $ENSGCM_NCPUS
                     else
                        @ myncpus = $ENSGCM_NCPUS / $this_ntasks_per_node
                        @ module = $myncpus * $this_ntasks_per_node - $ENSGSI_NCPUS
                        if ( $module != 0 ) @ myncpus = $myncpus + 1
                        @ myncpus = $myncpus * $this_ntasks_per_node
                     endif
                   endif
                   @ myncpus = $ipoe * $myncpus
                   #_ @ myncpus = $ipoe * $ENSGCM_NCPUS
                   setenv JOBGEN_NCPUS $myncpus
                   jobgen.pl \
                        -q $AGCM_QNAME \
                        agcm_dst${npoe}     \
                        $GID                \
                        $AGCM_WALLCLOCK    \
                        "job_distributor.csh -machfile $ENSWORK/agcm_machfile$npoe -usrcmd $ENSWORK/agcm_poe.$npoe -usrntask $ENSGCM_NCPUS -njobs $ipoe" \
                        $ENSWORK  \
                        $MYNAME             \
                        $ENSWORK/.DONE_POE${npoe}_${MYNAME}.$yyyymmddhh \
                        "AGCM Failed for Member ${npoe}"
                   if (! -e agcm_dst${npoe}.j ) then
                      echo " ${MYNAME}: AGCM Failed to generate DST BATCH jobs for Member ${memtag}, Aborting ... "
                      touch $ENSWORK/.FAILED
                      exit(1)
                   endif
                   /bin/mv agcm_dst${npoe}.j $ENSWORK/
                   # this job is really not monitored; the real work done by agcm_mem${memtag}.j is monitored
                   $ATMENS_BATCHSUB $ENSWORK/agcm_dst${npoe}.j
                   touch .SUBMITTED
                   @ ipoe = 0 # reset counter
                   @ npoe++
                endif 
             else
                if ( -e agcm_mem${memtag}.j ) then
                   $ATMENS_BATCHSUB agcm_mem${memtag}.j
                   touch $ENSWORK/.SUBMITTED
                else
                   echo " ${MYNAME}: Failed to generate BATCH jobs for Atmos GCM, Aborting ... "
                   exit(1)
                endif
             endif # <DSTJOB>

        else

             $MPIRUN_ENSGCM |& tee -a $ENSWORK/agcm_mem${memtag}.log
             /bin/ls ./EGRESS
             set model_status = $status
             if ( $model_status ) then
                echo " ${MYNAME}: failed to run GCM for member $memtag, aborting ..."
                touch $ENSWORK/.FAILED
                exit(1)
             endif
             touch $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh

             # Store updated ensemble
             # ----------------------
             update_ens.csh $expid mem$memtag bkg $ENSWORK/mem${memtag} NULL $NCSUFFIX

        endif

     endif # if -e DONE_ENSFCST

     # back to work directory
     cd ../

  end # end loop over members

  # Monitor status of ongoing jobs
  # ------------------------------
  if ( $ENSPARALLEL ) then
     jobmonitor.csh $nmem $MYNAME $ENSWORK $yyyymmddhh
     if ($status) then
         echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
         exit(1)
     endif
  endif
 
  # Check that forecasts have taken place successfully
  # TBD: make this an independent script to be used elsewhere
  # --------------------------------------------------
  @ ic = 0
  @ idone = 0
  while ( $ic < $nmem )
       # Get positioned
       # --------------
       @ ic++
       set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
       if ( -e $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh ) then
          @ idone = $idone + 1
       endif
  end

  if ( $idone == $nmem ) touch .DONE_ENSFCST
endif # check .DONE_ENSFCST

# when done and doing bootstrap, make to reset to regular resource file
# ---------------------------------------------------------------------
if ( (-e .DONE_ENSFCST) && (-e $ATMENSETC/AGCM.BOOTSTRAP.rc.tmpl) ) then
  /bin/mv $ATMENSETC/AGCM.BOOTSTRAP.rc.tmpl $ATMENSETC/AGCM.BOOTSTRAP.rc.tmpl.DONE
  echo " ${MYNAME}: done with bootstraping restarts"
endif
if ( (-e .DONE_ENSFCST) && (-e $ATMENSETC/GAAS.BOOTSTRAP) ) then
  /bin/mv $ATMENSETC/GAAS.BOOTSTRAP $ATMENSETC/GAAS.BOOTSTRAP.DONE
  echo " ${MYNAME}: done with bootstraping GAAS"
endif


# Handle extra history output (beyond bkg-related history)
# --------------------------------------------------------
if (! -e $ENSWORK/.DONE_GCM_DIAG_UPD ) then
  @ ic = 0
  while ( $ic < $nmem )
     # Get positioned
     # --------------
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     cd mem${memtag}
     update_ens.csh $expid mem$memtag diag $ENSWORK/mem${memtag} HISTORY.rc $NCSUFFIX
     cd -
  end
  touch $ENSWORK/.DONE_GCM_DIAG_UPD
endif

# Now that forecasts finished cleanly, rename all restarts
# --------------------------------------------------------
if ( "$LINK_RST" == "$HYBRIDGSI" ) then # won't do it in easy-ana case when central_res = ensmble_res
   @ ic = 0
   while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     if (! -e $ENSWORK/.DONE_UPDATE_RST_MEM${memtag}.$yyyymmddhh ) then

        gcm_ensrst_wrap.csh $expid $ionymdb $ionhmsb mem$memtag
        if($status) then
          echo "${MYNAME}: error wrapping up member $memtag output, aborting"
          exit(1)
        else
           touch $ENSWORK/.DONE_UPDATE_RST_MEM${memtag}.$yyyymmddhh
        endif

     endif

   end
endif # recycling of RSTs

# clean up
# --------
/bin/rm agcm_dst*
/bin/rm agcm_poe*

# made it down here, all done
# ---------------------------
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
