#!/bin/csh

# atmos_egsi - runs ensemble of GSI analysis
#
# !TO DO:
#
# !REVISION HISTORY:
#
#  24Nov2011  Todling   Initial script
#  20Jun2020  Todling   Revampped (based on observer)
#  23Jun2020  Todling   Redef meaning of ATMENSLOC
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars
setenv MYNAME atmos_egsi.csh
setenv skipTRANSF                 # no transform needed for ana-sensitivity executable
setenv skipSOLVER                 # need to run the analysis sensitivity solver
setenv skipSATBIAS "-skipSATBIAS" # no need to worry about running satellite bias correction

# The following env variable allows generation of a fake ensemble
# ---------------------------------------------------------------
if ( !($?SIMULATE_ENSEMBLE) ) setenv SIMULATE_ENSEMBLE 0

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - run control GSI and various other instances (as members) of GSI"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms "
   echo " "
   echo " where"
   echo "   expid     -  usual experiment name, e.g., b541iau"
   echo "   nymd      -  date of analysis, as in YYYYMMDD"
   echo "   nhms      -  time of analysis, as in HHMMSS"
   echo " "
   echo " DESCRIPTION"
   echo "    This procedure implements a driver for an ensemble of GSI analyses."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME ncep_prep_bufr,ncep_1bamua_bufr b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED RESOURCE FILES "
   echo " "
   echo "  Depending on the ensemble strategy under consideration:"
   echo "    gsi_cntrl.rc    - specify parameters to run control GSI analysis"
   echo "    gsi_memMMM.rc   - specify parameters to run MMM-member GSI analysis"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES "
   echo " "
   echo "    ATMENSETC       - location of resource files        "
   echo "    ATMENSLOC       - location of atmos-ensemble        "
   echo "    FVHOME          - location of experiment            "
   echo "    FVROOT          - location of DAS build             "
   echo "    FVWORK          - location of work directory        "
   echo "    GID             - group id to run job under         "
   echo "    MPIRUN_ENSANA   - define mpi command for GSIsa.x    "
   echo "    OBSCLASS        - set of observation classes        "
   echo "    TIMEINC         - analysis frequency (minutes)      "
   echo "    VAROFFSET       - offset time from initial synoptic time (minutes)"
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES "
   echo " "
   echo "    NCSUFFIX         - suffix of hdf/netcdf files (default: nc4)"
   echo "    ENSCTRLMEM       - run control analysis (default: 1)"
   echo "    ENSCTRLONLY      - run control GSI analysis only "
   echo "    ENSPARALLEL      - when set, runs all ensemble components in parallel "
   echo "                       (default: off)"
   echo "    ENSGSI_NCPUS     - when parallel ens on, this sets NCPUS for Observer calculation"
   echo "    ENSGSIDIAGS_NCPUS- number of CPUS given to gsidiags"
   echo "    AENS_OBSVR_DSTJOB- distribute multiple works within smaller jobs"
   echo "    OBSVR_WALLCLOCK  - wall clock time to run observer, default 1:00:00 "
   echo "    OBSVR_QNAME      - name of queue (default: NULL, that is, let BATCH pick) "
   echo " "
   echo " SEE ALSO"
   echo " "
   echo "   setobsvr.csh - prepare observations to be used by observer (based on fvssi) "
   echo "   gsidiags     - creates diag-files from GSI output (as used in central DAS)"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 20Jun2020      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif
 
setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ACFTBIAS)      ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?ATMENSLOC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1
if ( !($?MPIRUN_ENSANA) ) setenv FAILED 1
if ( !($?OBSCLASS)      ) setenv FAILED 1
if ( !($?TIMEINC)       ) setenv FAILED 1
if ( !($?VAROFFSET)     ) setenv FAILED 1

if ( !($?AENS_ADDINFLATION) ) setenv AENS_ADDINFLATION 0
if ( !($?AENS_OBSVR_DSTJOB) ) setenv AENS_OBSVR_DSTJOB 0
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?ENSCTRLMEM)    ) setenv ENSCTRLMEM 1  # control typically required, but can be bypassed
if ( !($?ENSCTRLONLY)   ) setenv ENSCTRLONLY 0 # useful for testing and other settings
if ( !($?ENSPARALLEL)   ) setenv ENSPARALLEL 0
if ( !($?OBSVR_WALLCLOCK))setenv OBSVR_WALLCLOCK 1:00:00
if ( !($?OBSVR_QNAME))    setenv OBSVR_QNAME NULL
if ( !($?STRICT)         ) setenv STRICT 1
if ( !($?ENSGSIDIAGS_NCPUS) ) setenv ENSGSIDIAGS_NCPUS 16

if ( $ENSPARALLEL ) then
   if ( !($?ENSGSI_NCPUS) ) then
     setenv FAILED 1 
   else
     setenv JOBGEN_NCPUS $ENSGSI_NCPUS
   endif
endif


if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid    = $1
set nymd     = $2
set nhms     = $3
set hh0      = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh0}

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

cd $ENSWORK

if ( -e $ENSWORK/.FAILED ) /bin/rm $ENSWORK/.FAILED   # otherwise it doesn't rerun

# Copy of directories from recycle locally
# ----------------------------------------
if (! -d $ATMENSLOC ) then
  echo "${MYNAME}: cannot find $ATMENSLOC, aboring ..."
  exit(1)
endif
set ensloc = $ATMENSLOC

# Check to make sure observer is really needed
# --------------------------------------------
set do_ctrl_gsi = 0
if( (-e $ATMENSETC/gsi_ctrl.rc) ) set do_ctrl_gsi = 1
set do_egsi_members = 0
set lst = `ls -1 $ATMENSETC/gsi_mem???.rc`
foreach fn ($lst)
   @ do_egsi_members++
end
if ( $do_ctrl_gsi == 0 ) then
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
   echo "${MYNAME}: nothing to do"
   exit(0)
endif
if ( $do_egsi_members == 0 ) then
   setenv ENSCTRLONLY 1
endif

set recana = "NO"
if( (-e $ATMENSETC/gsi_ctrl.rc) ) set recana = "YES"

set members = `/bin/ls -d $ensloc/mem* | wc`
set nmem = $members[1]
if ($nmem != $do_egsi_members ) then
   echo "${MYNAME}: inconsistent number of members and analysis RC files, aborting "
   exit(1)
endif

@ anafreq_sc   = $TIMEINC   * 60
@ varoffset_sc = $VAROFFSET * 60
set beg_ana = `tick $nymd $nhms -$varoffset_sc`
set end_ana = `tick $beg_ana[1] $beg_ana[2] $anafreq_sc`
set hhb     = `echo $beg_ana[2] | cut -c1-2`

# Run control analysis
# --------------------
if ( $ENSCTRLMEM ) then
   /bin/rm $ENSWORK/gsi_ensctrl.log
   if (! -e $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh ) then

      # get positioned
      # --------------
      mkdir ensctrl
      cd ensctrl
      ln -sf $ensloc/ensctrl/*bkg*.$NCSUFFIX .
      ln -sf $ensloc/ensctrl/*.txt .
    
      # set GSI_GridComp.rc
      # -------------------
      /bin/rm -f sed_file
      echo "s/>>>EXPID<<</${expid}/1"         > sed_file
      echo "s/>>>IOBBKGD<<</${beg_ana[1]}/1" >> sed_file
      echo "s/>>>IOBBKGT<<</${beg_ana[2]}/1" >> sed_file
      echo "s/>>>IOEBKGD<<</${end_ana[1]}/1" >> sed_file
      echo "s/>>>IOEBKGT<<</${end_ana[2]}/1" >> sed_file
      echo "s/>>>RECANA<<</${recana}/1"      >> sed_file
      echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> sed_file
      echo "s/>>>ANADATE<<</${nymd}/1"       >> sed_file
      echo "s/>>>ANATIME<<</${nhms}/1"       >> sed_file
      /bin/rm -f ./GSI_GridComp.rc
      sed -f sed_file $ATMENSETC/GSI_GridComp.rc.tmpl     > ./GSI_GridComp_ensctrl.rc
      ln -sf                   ./GSI_GridComp_ensctrl.rc      GSI_GridComp_member.rc
      ln -sf GSI_GridComp_ensctrl.rc GSI_GridComp.rc
    
      # setup observer
      # --------------
      set strict = ""
      if ( $STRICT ) set strict = "-strict"
#     if ( ! -e $ENSWORK/.DONE.setobsvr.csh.$yyyymmddhh ) then
      setobsvr.csh $strict -obsclass $OBSCLASS $skipTRANSF $skipSOLVER $skipSATBIAS $FVHOME $FVWORK/ensctrl $nymd $nhms $expid
         if ( $status ) then
            echo "${MYNAME}: Trouble getting observations, aborting"
         else
            touch $ENSWORK/.DONE.setobsvr.csh.$yyyymmddhh
         endif
#     endif


      # append obsys to member RC: when separate GSI_GridComp files control control and members,
      # the members file has to be appended with the observing system at this point (not earlier)
      if ( -e $ATMENSETC/GSI_GridComp_ensctrl.rc.tmpl ) then
         if ( ! -e $ENSWORK/.DONE_${MYNAME}.append_obsys.$yyyymmddhh ) then
            if ( $OBSCLASS != "NONE" ) then
                 append_gsigcrc.pl $ENSWORK/obsys.rc GSI_GridComp_member.rc
                 if ( $status ) then
                      echo "${MYNAME}: trouble appending obs table to GSI_GridComp.rc(member) "
                      exit(1)
                 endif
                 touch $ENSWORK/.DONE_${MYNAME}.append_obsys.$yyyymmddhh
            endif
         endif
      endif
    
      # prepare resources
      # -----------------
      if( -e gsiparm.anl.tmpl ) /bin/rm gsiparm.anl.tmpl
      if( -e $ATMENSETC/gsi_ctrl.rc ) then
         ln -sf $ATMENSETC/gsi_ctrl.rc  gsiparm.anl.tmpl
      else
         echo "${MYNAME}: no gsi-rc files found for control, aborting"
         exit(1)
      endif
    
      /bin/rm -f sed_file
      # control aircraft bias correction
      switch( $ACFTBIAS )
      case 0:
           echo "s/>>>AIRCFT_BIAS<<<//g"   >> sed_file
           echo 'Not using aircraft bias correction in GSI'
           breaksw
      case 1:
           echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc_ext=.true.,/g"  >> sed_file
           echo 'Setting aircraft_t_bc_ext to true, using external bias correction'
           breaksw
      case 2:
           echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc=.true.,/g"  >> sed_file
           echo 'Setting aircraft_t_bc to true, using VV.VV^2 bias correction'
           breaksw
      case 3:
           echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc_pof=.true.,/g"  >> sed_file
           echo 'Setting aircraft_t_bc_pof to true, using POF bias correction'
           breaksw
      default:
           echo "s/>>>AIRCFT_BIAS<<<//g"   >> sed_file
           echo 'Using default setting, not using aircraft bias correction in GSI'
           breaksw
      endsw
      if ( $nymd < 20100701 ) then
           echo "s/>>>USE_PREPB_SATWND<<</.true./g"  >> sed_file
      else
           echo "s/>>>USE_PREPB_SATWND<<</.false./g" >> sed_file
      endif
      if( -e gsiparm.anl ) /bin/rm gsiparm.anl
      sed -f sed_file  ./gsiparm.anl.tmpl  > ./gsiparm.anl
    
      if( -e $ATMENSETC/gmao_global_anavinfo.rc ) then
        /bin/rm anavinfo
        /bin/ln -s $ATMENSETC/gmao_global_anavinfo.rc anavinfo
      endif
      if( -e satbias ) then
           # since setobsvr.csh brings in these files w/ satbias/bang names link them to proper names
           ln -sf satbias satbias_in
      else
           echo " ${MYNAME}: Unable to find satbias/bang files to run control-analysis, Aborting ... "
           exit(1)
      endif
      if( -e atbang ) then
           # since setobsvr.csh brings in these files w/ satbias/bang names link them to proper names
           ln -sf satbang satbias_angle
      endif
      if ( $ACFTBIAS ) then
         if ( -e acftbias ) then 
            /bin/ln -sf acftbias aircftbias_in
         else
            touch aircftbias_in  # this should be done only one and never again, a bit unsafe
         endif
      endif
    
      set yzradbc = `nmlread.py gsiparm.anl SETUP newpc4pred`
      if ( ! $status ) then
         if ( $yzradbc == "True" ) then
            ln -sf satbiaspc satbias_pc
         endif
      endif
    
      # create links GSI needs to observations files
      # --------------------------------------------
      match_obcls_obsys.pl $nymd $nhms GSI_GridComp.rc gsiparm.anl
    
      # For now, control must have conventional obs ...
      # -----------------------------------------------
      set oblist = (`echorc.x -rc GSI_GridComp.rc -template dummy $nymd $nhms -ncol 3 observation_files`)
      setenv NOCONVOBS 1 
      foreach obfile ( $oblist )
         if ( $obfile =~ *.prepbufr.* ) then
            setenv NOCONVOBS 0 
         endif
      end
      if ( $NOCONVOBS ) then
           echo " ${MYNAME}: Unable to find prepbufr/conv-obs-file, Aborting ... "
           exit(1)
      endif
    
      # run observer
      # ------------
      if ( $ENSPARALLEL ) then
    
           jobgen.pl \
                -egress GSI_EGRESS  -q $OBSVR_QNAME \
                -xc "gsidiags -ncpus $ENSGSIDIAGS_NCPUS $nymd $nhms $expid set" \
                gsi_ensctrl         \
                $GID                \
                $OBSVR_WALLCLOCK    \
                "$MPIRUN_ENSANA |& tee -a $ENSWORK/gsi_ensctrl.log" \
                $ENSWORK/ensctrl    \
                $MYNAME             \
                $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh \
                "Control Analysis Failed "
    
                if ( -e gsi_ensctrl.j ) then
                   if ( $ATMENS_BATCHSUB == "sbatch" ) then
                      $ATMENS_BATCHSUB -W gsi_ensctrl.j
                   else
                      $ATMENS_BATCHSUB -W block=true gsi_ensctrl.j
                   endif
                else
                   echo " ${MYNAME}: Failed to generate BATCH jobs for Control Analysis, Aborting ... "
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
    
           $MPIRUN_ENSANA |& tee -a $ENSWORK/gsi_ensctrl.log
           if ( $status ) then
              touch $ENSWORK/.FAILED
              echo " ${MYNAME}: Control Analysis Failed, Aborting ... "
              exit(1)
           endif
           touch $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh
    
           # cat all pe files into diag files
           # --------------------------------
           gsidiags -ncpus $ENSGSIDIAGS_NCPUS $nymd $nhms $expid set
    
      endif
      if ( -e $ENSWORK/gsi_ensctrl.log ) cat fort.2* >> $ENSWORK/gsi_ensctrl.log
    
      cd ../
    
      set fn = gsi_ensctrl.log
      set nobs = (`grep "Jo Global" $fn  | cut -c25-35`)
      if ( $nobs[1] == 0 ) then
           /bin/rm $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh
           touch   $ENSWORK/.FAILED
           echo " ${MYNAME}: control analysis FAILED to execute properly, aborting ..."
           exit(1)
      else
           /bin/mv $fn $expid.$fn.${nymd}_${hh0}z.txt
           if(! -d $ENSWORK/updated_ens/ensctrl ) mkdir -p $ENSWORK/updated_ens/ensctrl
           tar -cvf updated_ens/$expid.atmens_olog.${nymd}_${hh0}z.tar $expid.gsi_*z.txt
           gzip updated_ens/$expid.atmens_olog.${nymd}_${hh0}z.tar
           cd $ENSWORK/ensctrl
           foreach fn (`ls *ana.eta*.$NCSUFFIX *xinc.eta*.$NCSUFFIX `)
              set pfx = `echo $fn | cut -d. -f1-4` 
              set sfx = `echo $fn | cut -d. -f5-` 
              if(! -e $pfx.ensctrl.$sfx ) /bin/mv $fn $pfx.ensctrl.$sfx
           end
           if(-e pcpbias_out)    /bin/mv pcpbias_out    $ENSWORK/updated_ens/ensctrl/$expid.ana.pcpbias.${nymd}_${hh0}z.txt
           if(-e aircftbias_out) /bin/mv aircftbias_out $ENSWORK/updated_ens/ensctrl/$expid.ana.aircftbias.${nymd}_${hh0}z.txt
           if(-e satbias_pc.out) /bin/mv satbias_pc.out $ENSWORK/updated_ens/ensctrl/$expid.ana.satbias.${nymd}_${hh0}z.txt
           if(-e satbias_out)    /bin/mv satbias_out    $ENSWORK/updated_ens/ensctrl/$expid.ana.satbias.${nymd}_${hh0}z.txt
           if(-e satbang)        /bin/cp satbang        $ENSWORK/updated_ens/ensctrl/$expid.ana.satbang.${nymd}_${hh0}z.txt
           cd -
      endif

   endif # DONE_ENSCRTL
endif # ENSCRTLANA

# Now perform some general checks:
# -------------------------------
#  1) determine if running only control analysis
#  2) determine if control analysis has finished running successfully
#
if ( $ENSCTRLONLY ) then
   echo " ${MYNAME}: done running control analysis only"
   exit(0)
endif

if (! -e $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: control analysis not available, bypass observer members"
   exit(1)
endif


# Determine number of observers that can run concurrently given total
# number of CPUS and required number of CPUS per observer call
# -------------------------------------------------------------------
set nx = `echorc.x -rc ensctrl/GSI_GridComp.rc "NX"`
set ny = `echorc.x -rc ensctrl/GSI_GridComp.rc "NY"`
@ myncpus = $nx * $ny

# Run observer for each member
# ----------------------------
#/bin/rm *.bkg.eta.*  *bkg.sfc.*
setenv LOCAL_ACQUIRE 1 # this will make sure acquire of satbias/bang not via BATCH
set ipoe = 0
set npoe = 0
/bin/rm $ENSWORK/egsi_poe.*
/bin/rm $ENSWORK/egsi_machfile*

if ( $ENSPARALLEL ) then
   set nfiles = `/bin/ls $ENSWORK/.DONE_MEM*_${MYNAME}.$yyyymmddhh | grep -v ENSCTRL | wc -l`
   echo "${MYNAME}: number of already available files  ${nfiles}"
   @ ntodo = $nmem - $nfiles 
endif 

set fpoe = 0
set n = 0
while ( $n < $nmem )

  @ n = $n + 1
  set nnn = `echo $n | awk '{printf "%03d", $1}'`
  if (! -e $ENSWORK/.DONE_MEM${nnn}_${MYNAME}.$yyyymmddhh ) then
     if ( -e $ENSWORK/gsi_mem${nnn}.log) /bin/rm $ENSWORK/gsi_mem${nnn}.log
     # get positioned
     # --------------
     mkdir mem${nnn}
     cd mem${nnn}
     ln -sf $ensloc/mem${nnn}/*bkg*.$NCSUFFIX .
     ln -sf $ensloc/mem${nnn}/*.txt .

     # setup observer
     # --------------
     setobsvr.csh -obsclass NONE $skipTRANSF $skipSOLVER $skipSATBIAS $FVHOME $FVWORK/mem${nnn} $nymd $nhms $expid

     /bin/rm pe*_01
     /bin/rm obsdiags.*

     # prepare resources
     # -----------------
     if( -e gsiparm.anl.tmpl ) /bin/rm gsiparm.anl.tmpl
     if( -e $ATMENSETC/gsi_mem${nnn}.rc ) then
        ln -sf $ATMENSETC/gsi_mem${nnn}.rc   gsiparm.anl.tmpl
     else
        echo "${MYNAME}: no gsi-rc file to control member ${nnn}, aborting"
        exit(1)
     endif
     if( -d Obsloc ) /bin/rm  Obsloc
     ln -sf $ENSWORK/ensctrl  Obsloc

     # control aircraft bias correction
     /bin/rm -f sed_file
     switch( $ACFTBIAS )
     case 0:
          echo "s/>>>AIRCFT_BIAS<<<//g"   >> sed_file
          echo 'Not using aircraft bias correction in GSI'
          breaksw
     case 1:
          echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc_ext=.true.,/g"  >> sed_file
          echo 'Setting aircraft_t_bc_ext to true, using external bias correction'
          breaksw
     case 2:
          echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc=.true.,/g"  >> sed_file
          echo 'Setting aircraft_t_bc to true, using VV.VV^2 bias correction'
          breaksw
     case 3:
          echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc_pof=.true.,/g"  >> sed_file
          echo 'Setting aircraft_t_bc_pof to true, using POF bias correction'
          breaksw
     default:
          echo "s/>>>AIRCFT_BIAS<<<//g"   >> sed_file
          echo 'Using default setting, not using aircraft bias correction in GSI'
          breaksw
     endsw
     if ( $nymd < 20100701 ) then
          echo "s/>>>USE_PREPB_SATWND<<</.true./g" >> sed_file
     else
         echo "s/>>>USE_PREPB_SATWND<<</.false./g" >> sed_file
     endif
     sed -f sed_file  ./gsiparm.anl.tmpl  > ./gsiparm.anl
     ln -sf ../ensctrl/GSI_GridComp_member.rc GSI_GridComp.rc

     # create links GSI needs to observations files
     # --------------------------------------------
     match_obcls_obsys.pl $nymd $nhms GSI_GridComp.rc gsiparm.anl

     /bin/rm anavinfo
     ln -s ../ensctrl/anavinfo .

     # since setobsvr.csh brings in these files w/ satbias/bang names link them to proper names
     if( -e satbias ) then
          ln -sf satbias satbias_in
     else
          echo " ${MYNAME}: Unable to find satbias files to run control-analysis, Aborting ... "
          exit(1)
     endif
     if( -e satbang ) then
          ln -sf satbang satbias_angle
     endif
     if ( $ACFTBIAS ) then
        if ( -e acftbias ) then 
           /bin/ln -sf acftbias aircftbias_in
        else
           touch aircftbias_in  # this should be done only one and never again, a bit unsafe
        endif
     endif
  
     set yzradbc = `nmlread.py gsiparm.anl SETUP newpc4pred`
     if ( ! $status ) then
        if ( $yzradbc == "True" ) then
           ln -sf satbiaspc satbias_pc
        endif
     endif
   
     # run observer
     # ------------
     if ( $ENSPARALLEL ) then
     
        @ fpoe++

        if ( $AENS_OBSVR_DSTJOB != 0 ) then # case of multiple jobs within few larger ones
           # collect multiple observer calls into jumbo file
           if ( $ipoe < $AENS_OBSVR_DSTJOB ) then  # nmem better devide by AENS_OBSVR_DSTJOB
              @ ipoe++
              set this_script_name = `pwd`/gsi_mem${nnn}.j
              echo $this_script_name >> $ENSWORK/egsi_poe.$npoe
              chmod +x $ENSWORK/egsi_poe.$npoe
           endif
           set machfile = "-machfile $ENSWORK/egsi_machfile$npoe.$ipoe"
        else
           set machfile = ""
        endif

        jobgen.pl \
             -egress GSI_EGRESS -q $OBSVR_QNAME $machfile \
             -xc "cat fort.2* >> $ENSWORK/gsi_mem${nnn}.log; gsidiags -ncpus $ENSGSIDIAGS_NCPUS $nymd $nhms $expid set" \
             gsi_mem${nnn}       \
             $GID                \
             $OBSVR_WALLCLOCK    \
             "$MPIRUN_ENSANA |& tee -a $ENSWORK/gsi_mem${nnn}.log" \
             $ENSWORK/mem${nnn}  \
             $MYNAME             \
             $ENSWORK/.DONE_MEM${nnn}_${MYNAME}.$yyyymmddhh \
             "Observer Failed for Member ${nnn}"

             if ( $AENS_OBSVR_DSTJOB != 0 ) then
                if ( -e gsi_mem${nnn}.j ) then
                   chmod +x gsi_mem${nnn}.j
                else
                   echo " ${MYNAME}: Observer Failed to generate BATCH jobs for Member ${nnn}, Aborting ... "
                   touch $ENSWORK/.FAILED
                   exit(1)
                endif

                if ( ($ipoe == $AENS_OBSVR_DSTJOB) || (($fpoe == $ntodo ) && ($ipoe < $AENS_OBSVR_DSTJOB) ) ) then
                   set this_ntasks_per_node = `facter processorcount`
                   @ ncores_needed = $ENSGSI_NCPUS / $this_ntasks_per_node
                   if ( $ncores_needed == 0 ) then
                     @ myncpus = $this_ntasks_per_node
                   else
                     if ( $ENSGSI_NCPUS == $ncores_needed * $this_ntasks_per_node ) then
                        @ myncpus = $ENSGSI_NCPUS
                     else
                        @ myncpus = $ENSGSI_NCPUS / $this_ntasks_per_node
                        @ module = $myncpus * $this_ntasks_per_node - $ENSGSI_NCPUS
                        if ( $module != 0 ) @ myncpus = $myncpus + 1
                        @ myncpus = $myncpus * $this_ntasks_per_node
                     endif
                   endif
                   @ myncpus = $ipoe * $myncpus
                   #_ @ myncpus = $ipoe * $ENSGSI_NCPUS
                   setenv JOBGEN_NCPUS $myncpus
                   jobgen.pl \
                        -q $OBSVR_QNAME     \
                        egsi_dst${npoe}    \
                        $GID                \
                        $OBSVR_WALLCLOCK    \
                        "job_distributor.csh -machfile $ENSWORK/egsi_machfile$npoe -usrcmd $ENSWORK/egsi_poe.$npoe -usrntask $ENSGSI_NCPUS -njobs $ipoe " \
                        $ENSWORK  \
                        $MYNAME             \
                        $ENSWORK/.DONE_POE${npoe}_${MYNAME}.$yyyymmddhh \
                        "Observer Failed for Member ${npoe}"
                   if (! -e egsi_dst${npoe}.j ) then
                      echo " ${MYNAME}: Observer Failed to generate DST BATCH jobs for Member ${nnn}, Aborting ... "
                      touch $ENSWORK/.FAILED
                      exit(1)
                   endif
                   /bin/mv egsi_dst${npoe}.j $ENSWORK/
                   $ATMENS_BATCHSUB $ENSWORK/egsi_dst${npoe}.j
                   touch .SUBMITTED
                   @ ipoe = 0 # reset counter
                   @ npoe++
                endif 
             else
                if ( -e gsi_mem${nnn}.j ) then
                   $ATMENS_BATCHSUB gsi_mem${nnn}.j
                   touch .SUBMITTED
                else
                   echo " ${MYNAME}: Observer Failed to generate BATCH jobs for Member ${nnn}, Aborting ... "
                   touch $ENSWORK/.FAILED
                   exit(1)
                endif
             endif # <poe>

     else

             $MPIRUN_ENSANA |& tee -a $ENSWORK/gsi_mem${nnn}.log
             if ( $status ) then
                echo " ${MYNAME}: Observer Failed for Member ${nnn}, Aborting ... "
                exit(1)
             endif
             set nobs = `grep "Jo Global" gsi_mem${nnn}.log | cut -c25-35`
             if ( $nobs == 0 ) then
                echo "${MYNAME}: found zero obs in observer, aborting ..."
                touch $ENSWORK/.FAILED
             else
                touch $ENSWORK/.DONE_MEM${nnn}_${MYNAME}.$yyyymmddhh
             endif

             # cat all pe files into diag files
             # --------------------------------
             gsidiags -ncpus $ENSGSIDIAGS_NCPUS $nymd $nhms $expid set

     endif

     cd ../
  endif # DONE_MEM
end

# Monitor status of ongoing jobs
# ------------------------------
if ( $ENSPARALLEL ) then
   jobmonitor.csh $nmem $MYNAME  $ENSWORK $yyyymmddhh
   if ($status) then
       echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
       exit(1)
   endif
endif

# Back to main directory
# ----------------------
cd $ENSWORK

# Clean up
# --------
set n = 0
while ( $n < $nmem )
  @ n = $n + 1
  set nnn = `echo $n | awk '{printf "%03d", $1}'`
  if ( -e .DONE_MEM${nnn}_${MYNAME}.$yyyymmddhh ) then # make sure all went well
      cd mem$nnn
      /bin/rm pe*_01
      /bin/rm pe*.obs_setup
      /bin/rm obsdiags.*
      foreach fn (`ls *ana.eta*.$NCSUFFIX *xinc.eta*.$NCSUFFIX `)
         set pfx = `echo $fn | cut -d. -f1-4`
         set sfx = `echo $fn | cut -d. -f5-`
         if (! -e $pfx.mem$nnn.$sfx ) /bin/mv $fn $pfx.mem$nnn.$sfx 
      end
      cd -
      /bin/rm egsi_dst*
      /bin/rm egsi_poe*
  else
     sleep 20 # allow for system-delay
     if (! -e .DONE_MEM${nnn}_${MYNAME}.$yyyymmddhh ) then # check for file one more time before giving up
         echo "${MYNAME}: cannot find .DONE_MEM${nnn}_${MYNAME}.${yyyymmddhh}, aborting"
         exit(1)
     endif
  endif
end

# Just for monitoring purposes: echo global fits for control and members
# ----------------------------------------------------------------------
grep "Jo Global" $ENSWORK/gsi_*.log

# Rename all observer log files
foreach fn ( `/bin/ls gsi_mem*.log` )
   /bin/mv $fn $expid.$fn.${nymd}_${hh0}z.txt
end
# Make sure all obs counts are meaningful
@ nc = 0
foreach fn ( `/bin/ls $expid.*.${nymd}_${hh0}z.txt` )
  set nobs = (`grep "Jo Global" $fn  | cut -c25-35`)
  if ( $nobs[1] == 0 ) then
       set nnn = `echo $fn | cut -c8-10`
       /bin/rm $ENSWORK/.DONE_MEM${nnn}_${MYNAME}.$yyyymmddhh
       touch   $ENSWORK/.FAILED
  else
       @ nc++
  endif
end
# make sure all expected observer logs are 
@ nmemp1 = $nmem + 1 # members plus control
if ( $nc == $nmemp1 ) then 
   if ( ! -e updated_ens ) mkdir updated_ens
   if ( -e updated_ens/$expid.atmens_olog.${nymd}_${hh0}z.tar.gz ) then 
      gunzip updated_ens/$expid.atmens_olog.${nymd}_${hh0}z.tar.gz
      tar -rvf updated_ens/$expid.atmens_olog.${nymd}_${hh0}z.tar $expid.gsi_mem*z.txt
      gzip updated_ens/$expid.atmens_olog.${nymd}_${hh0}z.tar
   else
      touch   $ENSWORK/.FAILED
   endif
   cd -
else
   touch   $ENSWORK/.FAILED
endif
# Store analysis and rename as needed
cd $ENSWORK
@ ic = 0
while ($ic < $nmem) 
  @ ic++
  set nnn = `echo $ic | awk '{printf "%03d", $1}'` 
  update_ens.csh $expid mem$nnn   ana $ENSWORK/mem${nnn} NULL $NCSUFFIX
  cd mem$nnn
  if(-e pcpbias_out)    /bin/mv pcpbias_out    $ENSWORK/updated_ens/mem$nnn/$expid.ana.pcpbias.${nymd}_${hh0}z.txt
  if(-e aircftbias_out) /bin/mv aircftbias_out $ENSWORK/updated_ens/mem$nnn/$expid.ana.aircftbias.${nymd}_${hh0}z.txt
  if(-e satbias_pc.out) /bin/mv satbias_pc.out $ENSWORK/updated_ens/mem$nnn/$expid.ana.satbiaspc.${nymd}_${hh0}z.txt
  if(-e satbias_out)    /bin/mv satbias_out    $ENSWORK/updated_ens/mem$nnn/$expid.ana.satbias.${nymd}_${hh0}z.txt
  if(-e satbang)        /bin/cp satbang        $ENSWORK/updated_ens/mem$nnn/$expid.ana.satbang.${nymd}_${hh0}z.txt
  cd -
end
#
if ( -e $ENSWORK/.FAILED ) then
   echo " ${MYNAME}: observer FAILED to execute properly, aborting ..."
   exit(1)
endif
# if made it here, then exit nicely
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
