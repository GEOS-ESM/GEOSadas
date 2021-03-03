#!/bin/csh

setenv DRYRUN "check"
setenv DRYRUN echo
setenv DRYRUN 

# atmens_efsens.csh - run forecast sensitivty for each member of the
#                     ensemble. 
#
# !REVISION HISTORY:
#
#  23Apr2017  Todling   Initial script
#  03May2020  Todling   Logic not to over-subscribe node
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmens_efsens.csh

if ( $#argv < 5 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - run forecast sensitivty for each ensemble member"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms taub aver"
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  initial date of forecast, as in YYYYMMDD"
   echo "   nhms   -  initial time of forecast, as in HHMMSS"
   echo "   taub   -  forecast interval EFSO calcualted for (in hours)"
   echo "   aver   -  verification type (ana,asm,or niana) "
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedure is controls fvsens calls to the members of the "
   echo "  ensemble. "
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 24 asm setrc"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    AENSTAT_MPIRUN   - mp_stats MPI command line         "
   echo "    ATMENSETC        - location of experiment            "
   echo "    FVWORK           - location of work directory        "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    NCSUFFIX          - suffix of hdf/netcdf files (default: nc4)"
   echo "    DATADIR           - location where original data resides"
   echo "                        (default: /archive/u/user)"
   echo "    SRCEXPID          - original experiment (use when other than expid)"
   echo "                        (default: expid)"
   echo " "
   echo " SEE ALSO"
   echo "   fvsens - driver for model adjoint runs"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Created modified: 23Apr2017   by: R. Todling"
   echo "     Last modified: 24Apr2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?AENSTAT_MPIRUN)) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1

if ( !($?ATMENS_FSO_JGRAD)  ) setenv ATMENS_FSO_JGRAD 0  # 1= use forecast error 

if ( !($?ATMENS_FSO_AVRFY)  ) setenv ATMENS_FSO_AVRFY 0  # 0= use central analysis  
                                                         # 1= use ensemble mean analysis
if ( !($?ATMENS_FSO_MFCST)  ) setenv ATMENS_FSO_MFCST 0  # 0= use central fcsts
                                                         # 1= use mean of ens forecast
                                                         # 2= use adj-derived sensitivty from central
if ( !($?SRCEXPID)      ) setenv SRCEXPID NULL
if ( !($?DATADIR)       ) setenv DATADIR $ARCHIVE
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4 

if ( $ENSPARALLEL ) then
   if ( !($?ENSGCMADJ_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $ENSGCMADJ_NCPUS
   endif
endif

setenv FVINPUT $FVBCS

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid  = $1
set nymd   = $2
set nhms   = $3
set ftau   = $4
set aver   = $5

if ( "$SRCEXPID" == "NULL" ) setenv SRCEXPID $expid

@ ana_offset = 3 * 3600
@ sixhours   = 6 * 3600
@ ftau_sec   = $ftau * 3600

# Current analysis time
# ---------------------
set yyyy = `echo $nymd | cut -c1-4`
set mm   = `echo $nymd | cut -c5-6`
set hh   = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}

setenv ENSWORK $FVWORK

if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: Already complete."
   exit (0)
endif

if ( ! -d $ENSWORK ) mkdir -p $ENSWORK
cd $ENSWORK

# Initial analysis time
# ---------------------
set date0 = (`tick $nymd $nhms $ana_offset`)
set anymd = $date0[1]
set anhms = $date0[2]
set ayyyy = `echo $anymd | cut -c1-4`
set amm   = `echo $anymd | cut -c5-6`
set ahh   = `echo $anhms | cut -c1-2`
set ayyyymmddhh = ${anymd}${ahh}

# Verification time
# -----------------
set av0date = ( `tick $anymd $anhms $ftau_sec` )
set av0nymd = $av0date[1]
set av0nhms = $av0date[2]
set av0yyyy = `echo $av0nymd | cut -c1-4`
set av0mm   = `echo $av0nymd | cut -c5-6`
set av0hh   = `echo $av0nhms | cut -c1-2`
set av0yyyymmddhh = ${av0nymd}${av0hh}

# Current times of forecasts from background and analysis
# -------------------------------------------------------
set fadate = ( $nymd $nhms )
set fanymd = $fadate[1]
set fanhms = $fadate[2]
set fayyyy = `echo $fanymd | cut -c1-4`
set famm   = `echo $fanymd | cut -c5-6`
set fadd   = `echo $fanymd | cut -c7-8`
set fahh   = `echo $fanhms | cut -c1-2`
set fayyyymmddhh = ${fanymd}${fahh}

set fbdate = (`tick $nymd $nhms -$sixhours`)
set fbnymd = $fbdate[1]
set fbnhms = $fbdate[2]
set fbyyyy = `echo $fbnymd | cut -c1-4`
set fbmm   = `echo $fbnymd | cut -c5-6`
set fbdd   = `echo $fbnymd | cut -c7-8`
set fbhh   = `echo $fbnhms | cut -c1-2`
set fbyyyymmddhh = ${fbnymd}${fbhh}

# Following cycle time
# ---------------------
set nanadate = (`tick $nymd $nhms $sixhours`)
set nnymd = $nanadate[1]
set nnhms = $nanadate[2]
set nyyyy = `echo $nnymd | cut -c1-4`
set nmm   = `echo $nnymd | cut -c5-6`
set nhh   = `echo $nnhms | cut -c1-2`
set nyyyymmddhh = ${nnymd}${nhh}

# Verification from cycle above 
# -----------------------------
set vnanadate = (`tick $nnymd $nnhms $ftau_sec`)
set vnnymd = $vnanadate[1]
set vnnhms = $vnanadate[2]
set vnyyyy = `echo $vnnymd | cut -c1-4`
set vnmm   = `echo $vnnymd | cut -c5-6`
set vnhh   = `echo $vnnhms | cut -c1-2`
set vnyyyymmddhh = ${vnnymd}${vnhh}

set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]

  if (! -d $ENSWORK/updated_ens ) then
     echo " ${MYNAME}: cannot find AGCM_apert.rc.tmpl file, Aborting ... "
     exit(1)
  endif
  if ( $ENSPARALLEL ) then
     set nfiles = `/bin/ls $ENSWORK/.DONE_MEM*_${MYNAME}.$yyyymmddhh | wc -l`
     echo "${MYNAME}: number of already available files  ${nfiles}"
     @ ntodo = $nmem - $nfiles
  endif

# Loop over members of the ensemble
# ---------------------------------
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

     /bin/cp  $FVHOME/run/ageps/*.tmpl .
     /bin/cp  $FVHOME/run/ageps/*.rc   .
     /bin/cp  $FVHOME/run/ageps/*.acq  .

     if (! -e CAP_apert.rc.tmpl ) then
        echo " ${MYNAME}: cannot find CAP_apert.rc.tmpl file, Aborting ... "
        exit(1)
     endif

     if ( -e AGCM_apert.rc.tmpl ) then
        set rundt = `grep "^HEARTBEAT_DT:" CAP_apert.rc.tmpl | cut -d: -f2`
     else
        echo " ${MYNAME}: cannot find AGCM_apert.rc.tmpl file, Aborting ... "
        exit(1)
     endif

     if ( -e ana.acq.tmpl ) then
        setenv ACQWORK $ENSWORK/mem$memtag
        set ana_acq_tmpl = ana.acq.tmpl
        set ana_acq = $ENSWORK/mem$memtag/ana.acq
        vED -env $ana_acq_tmpl -o $ana_acq
     endif

     # Get prognostic files from updated directory
     # -------------------------------------------
     ln -sf $ENSWORK/updated_ens/mem$memtag/*.prog.eta.*$NCSUFFIX .

        if( $ENSPARALLEL ) then
             @ fpoe++
             if ( $AENS_GCMADJ_DSTJOB != 0 ) then # case of multiple jobs within few larger ones
                # collect multiple gcm calls into jumbo file
                if ( $ipoe < $AENS_GCMADJ_DSTJOB ) then # nmem better devide by AENS_GCMADJ_DSTJOB
                   @ ipoe++
                   set this_script_name = `pwd`/agcmadj_mem${memtag}.j
                   echo $this_script_name >> $ENSWORK/agcmadj_poe.$npoe
                   chmod +x $ENSWORK/agcmadj_poe.$npoe
                endif
                set machfile = "-machfile $ENSWORK/agcmadj_machfile$npoe.$ipoe"
             else
                set machfile = ""
             endif

#                 -xc "update_ens.csh $expid mem$memtag bkg $ENSWORK/mem${memtag} NULL $NCSUFFIX" \
             jobgen.pl \
                  -egress EGRESS -q $AGCMADJ_QNAME $machfile \
                  agcmadj_mem${memtag}     \
                  $GID                  \
                  $AGCMADJ_WALLCLOCK       \
                  "fvsens $ENSWORK/mem$memtag $expid $av0nymd $av0nhms $av0hh $rundt |& tee -a $ENSWORK/agcmadj_mem${memtag}.log" \
                  $ENSWORK/mem${memtag} \
                  $MYNAME               \
                  $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh \
                  "Atmos GCMADJ Failed"


             if ( $AENS_GCMADJ_DSTJOB != 0 ) then
                if ( -e agcmadj_mem${memtag}.j ) then
                   chmod +x agcmadj_mem${memtag}.j
                else
                   echo " ${MYNAME}: AGCMADJ Failed to generate PBS jobs for Member ${memtag}, Aborting ... "
                   touch $ENSWORK/.FAILED
                   exit(1)
                endif

                if ( ($ipoe == $AENS_GCMADJ_DSTJOB) || (($fpoe == $ntodo) && ($ipoe < $AENS_GCMADJ_DSTJOB) ) ) then
                   set this_ntasks_per_node = `facter processorcount`
                   @ ncores_needed = $ENSGCMADJ_NCPUS / $this_ntasks_per_node
                   if ( $ncores_needed == 0 ) then
                    @ myncpus = $this_ntasks_per_node
                   else
                     if ( $ENSGCMADJ_NCPUS == $ncores_needed * $this_ntasks_per_node ) then
                        @ myncpus = $ENSGCMADJ_NCPUS
                     else
                        @ myncpus = $ENSGCMADJ_NCPUS / $this_ntasks_per_node
                        @ module = $myncpus * $this_ntasks_per_node - $ENSGCMADJ_NCPUS
                        if ( $module != 0 ) @ myncpus = $myncpus + 1
                        @ myncpus = $myncpus * $this_ntasks_per_node
                     endif
                   endif
                   @ myncpus = $ipoe * $myncpus
                   #_ @ myncpus = $ipoe * $ENSGCMADJ_NCPUS
                   setenv JOBGEN_NCPUS $myncpus
                   jobgen.pl \
                        -egress AGCMADJ_DST_EGRESS -q $AGCMADJ_QNAME \
                        agcmadj_dst${npoe}     \
                        $GID                \
                        $AGCMADJ_WALLCLOCK    \
                        "job_distributor.csh -machfile $ENSWORK/agcmadj_machfile$npoe -usrcmd $ENSWORK/agcmadj_poe.$npoe -usrntask $ENSGCMADJ_NCPUS -njobs $ipoe" \
                        $ENSWORK  \
                        $MYNAME             \
                        $ENSWORK/.DONE_POE${npoe}_${MYNAME}.$yyyymmddhh \
                        "AGCMADJ Failed for Member ${npoe}"
                   if (! -e agcmadj_dst${npoe}.j ) then
                      echo " ${MYNAME}: AGCMADJ Failed to generate DST PBS jobs for Member ${memtag}, Aborting ... "
                      touch $ENSWORK/.FAILED
                      exit(1)
                   endif
                   /bin/mv agcmadj_dst${npoe}.j $ENSWORK/
                   # this job is really not monitored; the real work done by agcmadj_mem${memtag}.j is monitored
                   $ATMENS_BATCHSUB $ENSWORK/agcmadj_dst${npoe}.j
                   touch .SUBMITTED
                   @ ipoe = 0 # reset counter
                   @ npoe++
                endif 
             else
                if ( -e agcmadj_mem${memtag}.j ) then
                   $ATMENS_BATCHSUB agcmadj_mem${memtag}.j
                   touch $ENSWORK/.SUBMITTED
                else
                   echo " ${MYNAME}: Failed to generate PBS jobs for Atmos GCMADJ, Aborting ... "
                   exit(1)
                endif
             endif # <DSTJOB>

        endif

        cd -
   end

  # Monitor status of ongoing jobs
  # ------------------------------
  if ( $ENSPARALLEL ) then
     jobmonitor.csh $nmem $MYNAME $ENSWORK $yyyymmddhh
     if ($status) then
         echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
         exit(1)
     endif
  endif

  cd $ENSWORK
# Loop over members ...
# nnymd
  foreach dir (`/bin/ls -d mem*`)
     foreach ftype ( Jgradf fsens )
        /bin/mv $dir/$expid.${ftype}*$NCSUFFIX updated_ens/$dir/
     end
     foreach ftype ( Jnorm )
        /bin/mv $dir/$expid.${ftype}*txt updated_ens/$dir/
     end
  end

#  Apply extended time tag to Jnorm/Jgrad/fsens

#  update_ens.csh $expid mem$memtag fsens $ENSWORK/mem${memtag} NULL $NCSUFFIX

# end loop over members

# If made it to here ...
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
exit(0)
