#!/bin/csh

# obsvr_ensfinal - runs observer after enAna to get OmAs
#
# This is run the observer after the ensemble analysis 
# to get an approximate sense of what the OmA's are. 
# Since the input fields to this final-pass observer correspond
# to an update of mean backgrounds this is considered approximate.
#
# !REVISION HISTORY:
#
#  15Apr2013  Todling   Initial script
#  18Jun2014  Todling   Revisit FGAT (now works)
#  21Aug2015  Todling   Allow obsvr-final to run independently of members setting 
#  31Jul2018  Todling/Gu  Allow for ens own setting of anavinfo file
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#  31Mar2020  Todling   Jobmonitor to protect against faulty batch-block
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars
setenv MYNAME obsvr_ensfinal.csh
setenv skipTRANSF                 # no transform needed for ana-sensitivity executable
setenv skipSOLVER                 # need to run the analysis sensitivity solver
setenv skipSATBIAS "-skipSATBIAS" # no need to worry about running satellite bias correction

# The following env variable allows generation of a fake ensemble
# ---------------------------------------------------------------
if ( !($?SIMULATE_ENSEMBLE) ) setenv SIMULATE_ENSEMBLE 0

if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - run observer over mean analysis to get OmAs"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms anadir "
   echo " "
   echo " where"
   echo "   expid     -  usual experiment name, e.g., b541iau"
   echo "   nymd      -  date of analysis, as in YYYYMMDD"
   echo "   nhms      -  time of analysis, as in HHMMSS"
   echo "   anadir    -  directory holding ensemble analyses"
   echo " "
   echo " DESCRIPTION"
   echo "    This procedure runs the observer after the ensemble analysis "
   echo "  to get an approximate estimate of the OmA's. Since the input"
   echo "  fields to this final-pass observer correspond to an update"
   echo "  of the mean backgrounds this is considered to give an approximate"
   echo "  estimate only."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 ENSWORK/updated_ens "
   echo " "
   echo " REQUIRED RESOURCE FILES "
   echo " "
   echo "  This program is triggered by the presence of the following "
   echo "  resource files: "
   echo "   obs1gsi_member.rc             - specify member observer parameters"
   echo "   GSI_GridComp_ensfinal.rc.tmpl - specify parameter for final obsvr "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES "
   echo " "
   echo "    ATMENSETC       - location of resource files        "
   echo "    FVHOME          - location of experiment            "
   echo "    FVROOT          - location of DAS build             "
   echo "    FVWORK          - location of work directory        "
   echo "    GID             - group id to run job under         "
   echo "    MPIRUN_ENSANA   - define mpi command for GSIsa.x    "
   echo "    TIMEINC         - analysis frequency (minutes)      "
   echo "    VAROFFSET       - offset time from initial synoptic time (minutes)"
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES "
   echo " "
   echo "    NCSUFFIX         - suffix of hdf/netcdf files (default: nc4)"
   echo "    ENSPARALLEL      - when set, runs all ensemble components in parallel "
   echo "                       (default: off)"
   echo "    ENSGSI_NCPUS     - when parallel ens on, this sets NCPUS for Observer calculation"
   echo "    OBSVR_WALLCLOCK  - wall clock time to run observer, default 1:00:00 "
   echo "    OBSVR_QNAME      - name of queue (default: NULL, that is, let pbs pick) "
   echo " "
   echo " SEE ALSO"
   echo " "
   echo "   post_eana - the post-analysis will call this procedure"
   echo " "
   echo " REMARKS"
   echo " "
   echo "   post_eana - the post-analysis will call this procedure"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 15Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif
 
setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1
if ( !($?MPIRUN_ENSANA) ) setenv FAILED 1
if ( !($?TIMEINC)       ) setenv FAILED 1
if ( !($?VAROFFSET)     ) setenv FAILED 1
if ( !($?STAGE4HYBGSI)  ) setenv FAILED 1 # TBD hack
if ( !($?OBSCLASS)      ) setenv FAILED 1

setenv DOFGAT 1    # default to ana at frequency of bkg

if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)   ) setenv ENSPARALLEL 0
if ( !($?OBSVR_WALLCLOCK))setenv OBSVR_WALLCLOCK 1:00:00
if ( !($?OBSVR_QNAME))    setenv OBSVR_QNAME NULL
if ( !($?STRICT)        ) setenv STRICT 1

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
set anadir   = $4
set hha      = `echo $nhms | cut -c1-2`
set hhmna    = `echo $nhms | cut -c1-4`
set yyyymmddhhmn = ${nymd}${hhmna}

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhhmn ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

cd $ENSWORK

# Check to make sure observer is really needed
# --------------------------------------------
set do_obsvr = 0
if((-e $ATMENSETC/obs1gsi_member.rc) && -e $ATMENSETC/GSI_GridComp_ensfinal.rc.tmpl ) set do_obsvr = 1
if((-e $ATMENSETC/obs1gsi_final.rc)  && -e $ATMENSETC/GSI_GridComp_ensfinal.rc.tmpl ) set do_obsvr = 1
if ( $do_obsvr == 0 ) then
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhhmn
   echo "${MYNAME}: nothing to do"
   exit(0)
endif

@ anafreq_sc   = $TIMEINC   * 60
@ varoffset_sc = $VAROFFSET * 60
set beg_ana = `tick $nymd $nhms -$varoffset_sc`
set end_ana = `tick $beg_ana[1] $beg_ana[2] $anafreq_sc`

# Run mean analysis
# -----------------
/bin/rm $ENSWORK/obs_ensfinal.log
if (! -e $ENSWORK/.DONE_MEM001_ENSFINAL_${MYNAME}.$yyyymmddhhmn ) then

   # get positioned
   # --------------
   mkdir ensfinal
   cd ensfinal
   ln -sf $ENSWORK/ensmean/*bkg*.$NCSUFFIX .
   ln -sf  $anadir/ensmean/*ana*.$NCSUFFIX .

   # calculate analysis increment
   # ----------------------------
   if ( $DOFGAT ) then
      if(-e myinc.eta.$NCSUFFIX) /bin/rm myinc.eta.$NCSUFFIX
      dyndiff.x -g5 $expid.bkg.eta.${nymd}_${hhmna}z.$NCSUFFIX $expid.ana.eta.${nymd}_${hhmna}z.$NCSUFFIX \
                -o myinc.eta.$NCSUFFIX
      if ($status) then
          echo "${MYNAME}: error running dyndiff, aborting ..."
          exit (1)
      endif
   endif # DOFGAT

   # update each of the backgrounds with the same increment (as in 3dVar)
   # --------------------------------------------------------------------
   foreach fn ( `ls *.bkg.eta.*.$NCSUFFIX` )
      set this_nymd = `echo $fn | cut -d. -f4 | cut -c1-8`
      set this_hhmn = `echo $fn | cut -d. -f4 | cut -c10-13`
      if ( ! -e $expid.ana.eta.${this_nymd}_${this_hhmn}z.$NCSUFFIX ) then
         if ( $DOFGAT ) then # when FGAT, it will update off-synoptic bkg w/ same inc as central inc
            reset_time.x myinc.eta.$NCSUFFIX $this_nymd ${this_hhmn}00 -9
            if ($status) then
                echo "${MYNAME}: error running reset_time.x, aborting ..."
                exit (1)
            endif
            #dyn_recenter.x -g5 -noremap -o $expid.ana.eta.${this_nymd}_${this_hhmn}z.$NCSUFFIX myinc.eta.$NCSUFFIX NONE $fn
            dynp.x -s $fn -p myinc.eta.$NCSUFFIX -a -1 -ainc -g5 -pureadd -realp -os $expid.ana.eta.${this_nymd}_${this_hhmn}z.$NCSUFFIX
            if ( $status ) then
               echo "${MYNAME}: error calculating analysis, aborting ..."
               exit(1)
            endif
         else               # when not FGAT, will fake off-synoptic ana as being same as synoptic time ana
            /bin/cp $expid.ana.eta.${nymd}_${hhmna}z.$NCSUFFIX $expid.ana.eta.${this_nymd}_${this_hhmn}z.$NCSUFFIX
            reset_time.x $expid.ana.eta.${this_nymd}_${this_hhmn}z.$NCSUFFIX $this_nymd ${this_hhmn}00 -9
         endif # DOFGAT
      endif
   end

   # set GSI_GridComp.rc
   # -------------------
   /bin/rm -f sed_file
   echo "s/>>>EXPID<<</${expid}/1"         > sed_file
   echo "s/>>>IOBBKGD<<</${beg_ana[1]}/1" >> sed_file
   echo "s/>>>IOBBKGT<<</${beg_ana[2]}/1" >> sed_file
   echo "s/>>>IOEBKGD<<</${end_ana[1]}/1" >> sed_file
   echo "s/>>>IOEBKGT<<</${end_ana[2]}/1" >> sed_file
   echo "s/>>>RECANA<<</NO/1"             >> sed_file
   echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> sed_file
   echo "s/>>>ANADATE<<</${nymd}/1"       >> sed_file
   echo "s/>>>ANATIME<<</${nhms}/1"       >> sed_file
   /bin/rm -f ./GSI_GridComp.rc
   sed -f sed_file $ATMENSETC/GSI_GridComp_ensfinal.rc.tmpl  > ./GSI_GridComp_ensfinal.rc
   ln -sf GSI_GridComp_ensfinal.rc GSI_GridComp.rc

#  Append observation table to the grid-comp
#   -----------------------------------------
   if ( $OBSCLASS != "NONE" ) then
        append_gsigcrc.pl $FVWORK/obsys.rc GSI_GridComp.rc
        if ( $status ) then
             echo "${MYNAME}: trouble appending obs table to GSI_GridComp.rc "
             exit(1)
        endif
   endif

   # setup observer
   # --------------
   setobsvr.csh -obsclass NONE $skipTRANSF $skipSOLVER $skipSATBIAS $FVHOME $FVWORK/ensfinal $nymd $nhms $expid
   #set strict = ""
   #if ( $STRICT ) set strict = "-strict"
   #setobsvr.csh $strict -obsclass $OBSCLASS $skipTRANSF $skipSOLVER $skipSATBIAS $FVHOME $FVWORK/ensfinal $nymd $nhms $expid

   # link up with directory holding the observation files
   # ----------------------------------------------------
   /bin/rm Obsloc
   ln -sf $ENSWORK/ensmean                Obsloc

   /bin/rm pe*_01
   /bin/rm obsdiags.*

   # prepare resources
   # -----------------
   if( -e gsiparm.anl ) /bin/rm gsiparm.anl
   if ( -e $ATMENSETC/obs1gsi_final.rc ) then
       ln -sf $ATMENSETC/obs1gsi_final.rc    gsiparm.anl.tmpl
   else
       ln -sf $ATMENSETC/obs1gsi_member.rc    gsiparm.anl.tmpl
   endif
#  ln -sf $ATMENSETC/obs1gsi_mean.rc      gsiparm.anl.tmpl
   ln -sf ../ensmean/obs_input.*          .
   ln -sf ../ensmean/satbias_in           .
   ln -sf ../ensmean/aircftbias_in        .
   ln -sf ../ensmean/satbias_angle        .
   /bin/rm anavinfo
   ln -sf ../ensmean/anavinfo             .

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
        echo "s/>>>USE_PREPB_SATWND<<</.true./g" >> sed_file
   else
       echo "s/>>>USE_PREPB_SATWND<<</.false./g" >> sed_file
   endif
   sed -f sed_file  ./gsiparm.anl.tmpl  > ./gsiparm.anl

   # create links GSI needs to observations files
   # --------------------------------------------
   match_obcls_obsys.pl $nymd $nhms GSI_GridComp.rc gsiparm.anl

   # run observer
   # ------------
   if ( $ENSPARALLEL ) then

        jobgen.pl \
             -egress GSI_EGRESS  -q $OBSVR_QNAME \
             -xc "gsidiags -jiter 1 -tag anl -suffix _ensfinal -catonly $nymd $nhms $expid set" \
             obs_ensfinal        \
             $GID                \
             $OBSVR_WALLCLOCK    \
             "$MPIRUN_ENSANA |& tee -a $ENSWORK/obs_ensfinal.log" \
             $ENSWORK/ensfinal   \
             $MYNAME             \
             $ENSWORK/.DONE_MEM001_ENSFINAL_${MYNAME}.$yyyymmddhhmn \
             "Observer Failed for Mean "

             if ( -e obs_ensfinal.j ) then
                if ( $ATMENS_BATCHSUB == "sbatch" ) then
                   $ATMENS_BATCHSUB -W obs_ensfinal.j
                else
                   $ATMENS_BATCHSUB -W block=true obs_ensfinal.j
                endif
             else
                echo " ${MYNAME}: Observer Failed to generate PBS jobs for Mean, Aborting ... "
                exit(1)
             endif

             # Monitor status of ongoing jobs (block job not always working)
             # ------------------------------
             jobmonitor.csh 1 ENSFINAL_${MYNAME}  $ENSWORK $yyyymmddhhmn
             if ($status) then
                 echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
                 exit(1)
             endif

   else

        $MPIRUN_ENSANA |& tee -a $ENSWORK/obs_ensfinal.log
        if ( $status ) then
           touch $ENSWORK/.FAILED
           echo " ${MYNAME}: Mean Observer Failed, Aborting ... "
           exit(1)
        endif
        touch $ENSWORK/.DONE_MEM001_ENSFINAL_${MYNAME}.$yyyymmddhhmn

        # cat all pe files into diag files
        # --------------------------------
        gsidiags -jiter 1 -tag anl -suffix _ensfinal -catonly $nymd $nhms $expid set

        # clean up
        # --------
        /bin/rm pe00*  # very dangerous

   endif
   if ( -e $ENSWORK/obs_ensfinal.log ) cat fort.2* >> $ENSWORK/obs_ensfinal.log

   cd ../
endif

# Retag log file
# --------------
/bin/mv obs_ensfinal.log $expid.obs_ensfinal.log.${nymd}_${hha}z.txt
if ( -e updated_ens/$expid.atmens_olog.${nymd}_${hha}z.tar.gz ) then
   gunzip updated_ens/$expid.atmens_olog.${nymd}_${hha}z.tar.gz
   tar -rvf updated_ens/$expid.atmens_olog.${nymd}_${hha}z.tar $expid.obs_ensfinal*z.txt
   gzip updated_ens/$expid.atmens_olog.${nymd}_${hha}z.tar
endif

# Now perform some general checks:
# -------------------------------
if (! -e $ENSWORK/.DONE_MEM001_ENSFINAL_${MYNAME}.$yyyymmddhhmn ) then
   echo " ${MYNAME}: observer mean final not available, aborting ..."
   exit(1)
endif

# if made it here, then exit nicely
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhhmn
echo " ${MYNAME}: Complete "
exit(0)
