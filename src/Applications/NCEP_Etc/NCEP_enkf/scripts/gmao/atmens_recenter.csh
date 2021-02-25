#!/bin/csh

# atmens_recenter - recenter members of ensemble around desired mean
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  19Nov2011  Todling   Acting as independent script
#  27Jan2012  RT/El Akkraoui  Add damp to recentering
#  21Apr2012  Todling   Implement option to apply additive inflation
#  22Apr2012  Todling   Parallelized recentering
#  05Feb2013  Todling   Knob to bypass recentering from main
#  09Feb2013  Todling   Rename inflating perturbation files
#  11Feb2013  El Akkraoui Bug fix: add check to job monitor status
#  12Mar2013  Todling   Implement options for distribute multi-work jobs
#  29May2013  El Akkraoui Bug fix: avoid multiple recentering when jobs crash
#  28Feb2014  El Akkraoui  Revisit logic for re-submit of distributed jobs
#  12Mar2014  Todling   Update interface to main re-center program
#  30Jun2014  Todling   Implement energy-based adaptive inflation proceure
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#  04Jun2020  Todling   Revise parallelization strategy
#  23Jun2020  Todling   Redef meaning of ATMENSLOC
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmens_recenter.csh
setenv ENSMEAN 1

if ( $#argv < 8 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - recenter ensemble around desired mean"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms ftype1 ftype2 ensloc cenloc infloc"
   echo " "
   echo "  where"
   echo "    expid  -  usual experiment name, e.g., u000_c92"
   echo "    nymd   -  date of analysis, as in YYYYMMDD"
   echo "    nhms   -  time of analysis, as in HHMMSS"
   echo "    ftype1 -  member file type, e.g,, inc.eta"
   echo "    ftype2 -  mean   file type, e.g,, ana.eta"
   echo "    ensloc -  location of ensemble members and mean"
   echo "    cenloc -  location of centering mean" 
   echo "    infloc -  location of perturbations for additive inflation"
   echo " "
   echo " DESCRIPTION: "
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME u000\_c92 20091019 000000 inc.eta ana.eta \\"
   echo "         /discover/nobackup/$user/u000\_c92/atmens \\"
   echo "         /archive/u/$user/b541iau/ana/Y2009/M10"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of ensemble RC files     "
   echo "    ATMENSLOC     - location of ensemble backgrounds  "
   echo "    ENSWORK       - location of work directory        "
   echo "    FVROOT        - location of DAS build             "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    ADDINF_FACTOR      - inflation factor (such as 0.25)"
   echo "    AENS_ADDINFLATION  - when set, apply additive inflation to analyzed members"
   echo "    AENS_DONORECENTER  - allow bypassing recentering"
   echo "    AENS_RECENTER_DSTJOB  - distribute multiple works within smaller jobs"
   echo "    ASYNBKG            - background frequency (when adaptive inflation on)"
   echo "    CENTRAL_BLEND      - 0 or 1=to blend members with central (def: 1)"
   echo "    FVHOME             - location of experiment            "
   echo "    NCSUFFIX           - suffix of hdf/netcdf files        "
   echo "    ENSPARALLEL        - when set, runs all ensemble components in parallel"
   echo "                         (default: off)"
   echo "    ENSRECENTER_NCPUS  - when parallel ens on, this sets NCPUS for recentering"
   echo "                         (NOTE: required when ENSPARALLEL is on)"
   echo "    RECENTER_WALLCLOCK - wall clock time to run dyn_recenter, default 0:10:00 "
   echo "    RECENTER_QNAME     - name of queue (default: NULL, that is, let pbs pick) "
   echo " "
   echo " REMARKS "
   echo " "
   echo "  WARNING: This will overwrite each ensemble member file"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENSETC)      ) setenv FAILED 1
if ( !($?ATMENSLOC)      ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1

if ( !($?AENS_ADDINFLATION)  ) setenv AENS_ADDINFLATION 0
if ( !($?AENS_DONORECENTER)  ) setenv AENS_DONORECENTER 0
if ( !($?AENS_RECENTER_DSTJOB) ) setenv AENS_RECENTER_DSTJOB 0
if ( !($?CENTRAL_BLEND)      ) setenv CENTRAL_BLEND 1
if ( !($?NCSUFFIX)           ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)        ) setenv ENSPARALLEL 0
if ( !($?RECENTER_WALLCLOCK) ) setenv RECENTER_WALLCLOCK 0:10:00

if ( $ENSPARALLEL ) then
   if ( !($?RECENTER_QNAME) ) then
      echo "${MYNAME}: error, env var RECENTER_QNAME not defined"
      setenv FAILED 1
   endif
   if ( ! $AENS_RECENTER_DSTJOB ) then
      setenv JOBGEN_NCPUS_PER_NODE 2
   endif
   if ( !($?ENSRECENTER_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $ENSRECENTER_NCPUS
   endif
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid  = $1
set nymd   = $2
set nhms   = $3
set ftype1 = $4
set ftype2 = $5
set ensloc = $6
set cenloc = $7
set infloc = $8
set hh     = `echo $nhms | cut -c1-2`
set hhmn   = `echo $nhms | cut -c1-4`
set yyyymmddhh = ${nymd}${hh}

if ( -e $ENSWORK/.DONE_${MYNAME}_${ftype1}_${ftype2}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

#source $FVROOT/bin/g5_modules
if ( ($?FVHOME) ) then
   set path = ( . $FVHOME/run $FVROOT/bin $path )
else
   set path = ( .             $FVROOT/bin $path )
endif

cd $ensloc

if ( -e $ATMENSLOC/$expid.add_infl.${nymd}_${hh}z.txt ) then
   set rec_rcfile = $ATMENSLOC/$expid.add_infl.${nymd}_${hh}z.txt
else
   if ( -e $ATMENSETC/dyn_recenter.rc ) then
      set rec_rcfile = $ATMENSETC/dyn_recenter.rc
   else
      set rec_rcfile = "NONE"
   endif
endif

if ( $CENTRAL_BLEND ) then
   set blend = "-damp"
else
   set blend = "NONE"
endif
if ((-e $ATMENSETC/ene_adaptinf.rc) && (-e $ATMENSETC/mp_stats.rc)) then
   if ( !($?ASYNBKG) )        setenv FAILED 1
   if ( !($?AENSTAT_MPIRUN) ) setenv FAILED 1
   if ( $FAILED ) then
     env
     echo " ${MYNAME}: not all required env vars defined for adaptive inflation procedure"
     exit 1
   endif

   @ bkgfreq_hrs  =  $ASYNBKG / 60
   set prerecene = `echorc.x -template $expid $nymd $nhms -rc $ATMENSETC/ene_adaptinf.rc ene_fname_template`
   if ( ! -e ./ensrms/$prerecene ) then
      $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o ensmean/$expid.ana_pre_recenter.eta.${nymd}_${hh}z.$NCSUFFIX \
                      -stdv ensrms/$expid.ana_pre_recenter.eta.${nymd}_${hh}z.$NCSUFFIX \
                      -ene  ensrms/$prerecene -inc ${bkgfreq_hrs}000 \
                      -egress .NULL_EGRESS_${nymd}${nhms} mem*/$expid.ana.eta.${nymd}_${hh}z.$NCSUFFIX
      if ( -e .NULL_EGRESS_${nymd}${nhms} && -e ./ensrms/$prerecene ) then
          # all went well ... clean up
          /bin/rm .NULL_EGRESS_${nymd}${nhms}
      else
          echo "${MYNAME}: failed to calculate energy-based spread for pre-recentered analysis, aborting ..."
          exit 2
      endif
   endif

   set addinf_factor = `ene_adaptinf.x -rc $ATMENSETC/ene_adaptinf.rc -minecho ensrms/$prerecene`
   if(-e ENE_ADAPTINF_EGRESS) then
      # all is normal; remove file in case job get resubmitted
      /bin/rm ENE_ADAPTINF_EGRESS
   else
      echo "${MYNAME}: trouble getting adaptive inflation factor, aborting ..."
      exit(1)
   endif
else
   if ( !($?ADDINF_FACTOR) ) then
      set addinf_factor = "NONE"
   else
      set addinf_factor = $ADDINF_FACTOR
   endif
endif
echo "${MYNAME}: On ${nymd} ${nhms} inflation factor = $addinf_factor"

# expect to find typical ensemble arrangement below this (ensloc) level ...
set members = `/bin/ls -d mem* | wc`
set nmem = $members[1]
set addinflation =

# Check on possible work left to be done
# --------------------------------------
if ( $ENSPARALLEL ) then
   set nfiles = `/bin/ls $ENSWORK/.DONE_MEM*_${MYNAME}_${ftype1}_${ftype2}.$yyyymmddhh | wc -l`
   echo "${MYNAME}: number of already available files  ${nfiles}"
   @ ntodo = $nmem - $nfiles 
endif 
@ fpoe = 0

/bin/rm $ENSWORK/recenter_poe.*
@ ipoe = 0
@ npoe = 0
@ ic = 0
while ( $ic < $nmem )
   @ ic = $ic + 1
   set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
   if ( -e $ENSWORK/.DONE_MEM${memtag}_${MYNAME}_${ftype1}_${ftype2}.$yyyymmddhh ) continue

   cd mem$memtag

   # overwrite each ensemble analysis with corresponding recentered analysis
   set failed = 0
   if (! -e $expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX ) then
      set failed = 1
      echo " ${MYNAME}: somehow can no longer find $expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX, aborting ..."
      touch $ensloc/.FAILED
   endif
   if (! -e ../ensmean/$expid.${ftype2}.${nymd}_${hhmn}z.$NCSUFFIX ) then
      set failed = 1
      echo " ${MYNAME}: somehow can no longer find ensmean/$expid.${ftype2}.${nymd}_${hhmn}z.$NCSUFFIX, aborting ..."
   endif
   if (! -e $cenloc/$expid.${ftype2}.${nymd}_${hhmn}z.$NCSUFFIX ) then
      set failed = 1
      echo " ${MYNAME}: somehow can no longer find $cenloc/$expid.${ftype2}.${nymd}_${hhmn}z.$NCSUFFIX, aborting ..."
   endif
   if ( $AENS_ADDINFLATION ) then
        if (! -e $infloc/$expid.nmcpert.eta.${nymd}_${hh}z.$memtag.$NCSUFFIX ) then
           set failed = 1
           echo " ${MYNAME}: somehow can not find $infloc/$expid.nmcpert.eta.${nymd}_${hh}z.$memtag.$NCSUFFIX, aborting ..."
        endif
   endif
   if( $failed ) then
      touch $ensloc/.FAILED
      exit(1)
   else # not failure ...

     if( $ENSPARALLEL ) then

         @ fpoe++

         if ( ! $AENS_ADDINFLATION ) set infloc = "NONE"

         if ( $AENS_DONORECENTER ) then
             set cmdline = "recenter.csh $FVROOT $expid $memtag $nymd $hhmn $ftype1 NONE    $blend $addinf_factor NONE $infloc $rec_rcfile"
         else
             set cmdline = "recenter.csh $FVROOT $expid $memtag $nymd $hhmn $ftype1 $ftype2 $blend $addinf_factor $cenloc $infloc $rec_rcfile"
         endif


         if ( $AENS_RECENTER_DSTJOB != 0 ) then # case of multiple jobs within few larger ones
            set cmdline = "serial_run $cmdline"

            # collect multiple recenter calls into jumbo file
            if ( $ipoe < $AENS_RECENTER_DSTJOB ) then  # nmem better devide by AENS_PERTS_DSTJOB
               @ ipoe++
               set this_script_name = `pwd`/recenter_mem${memtag}.j
               echo $this_script_name >> $ENSWORK/recenter_poe.$npoe
               chmod +x $ENSWORK/recenter_poe.$npoe
            endif
            set machfile = "-machfile $ENSWORK/recenter_machfile$npoe.$ipoe"
         else
            set machfile = ""
         endif

         jobgen.pl \
              -egress DYNRECENTER_EGRESS \
              -q $RECENTER_QNAME         \
              recenter_mem${memtag}      \
              $GID                       \
              $RECENTER_WALLCLOCK        \
              "$cmdline"                 \
              $ensloc/mem$memtag         \
              $MYNAME                    \
              $ENSWORK/.DONE_MEM${memtag}_${MYNAME}_${ftype1}_${ftype2}.$yyyymmddhh \
               "Recenter ANA Failed"


          if ( $AENS_RECENTER_DSTJOB != 0 ) then
             if ( -e recenter_mem${memtag}.j ) then
                chmod +x recenter_mem${memtag}.j
             else
                echo " ${MYNAME}: Recenter Failed to generate PBS jobs for Member ${memtag}, Aborting ... "
                touch $ENSWORK/.FAILED
                exit(1)
             endif


             if ( ($ipoe == $AENS_RECENTER_DSTJOB) || (($fpoe == $ntodo ) && ($ipoe < $AENS_RECENTER_DSTJOB) ) ) then
                @ myncpus = $ipoe
                setenv JOBGEN_NCPUS $myncpus
                jobgen.pl \
                     -q $RECENTER_QNAME \
                     recenter_dst${npoe}       \
                     $GID                      \
                     $RECENTER_WALLCLOCK       \
                     "job_distributor.csh -machfile $ENSWORK/recenter_machfile$npoe -usrcmd $ENSWORK/recenter_poe.$npoe -usrntask $ENSRECENTER_NCPUS -njobs $ipoe " \
                     $ENSWORK  \
                     $MYNAME             \
                     $ENSWORK/.DONE_POE${npoe}_${MYNAME}.$yyyymmddhh \
                     "Recenter Failed for Member ${npoe}"
                if (! -e recenter_dst${npoe}.j ) then
                   echo " ${MYNAME}: Recenter Failed to generate DST PBS jobs for Member ${memtag}, Aborting ... "
                   touch $ENSWORK/.FAILED
                   exit(1)
                endif
                /bin/mv recenter_dst${npoe}.j $ENSWORK/
                $ATMENS_BATCHSUB $ENSWORK/recenter_dst${npoe}.j
                touch .SUBMITTED
                @ ipoe = 0 # reset counter
                @ npoe++
             endif 

          else

             if ( -e recenter_mem${memtag}.j ) then
                $ATMENS_BATCHSUB recenter_mem${memtag}.j
             else
                echo " ${MYNAME}: Failed to generate PBS jobs for Recentering ANA, Aborting ... "
                touch $ensloc/.FAILED
                exit(1)
             endif

          endif # <DST>

      else

          if ( $AENS_ADDINFLATION ) then
             if ( !($?ADDINF_FACTOR) ) then
                set addinflation = "-inflate $infloc/$expid.nmcpert.eta.${nymd}_${hh}z.$memtag.$NCSUFFIX"
             else
                set addinflation = "-a $ADDINF_FACTOR -inflate $infloc/$expid.nmcpert.eta.${nymd}_${hh}z.$memtag.$NCSUFFIX"
             endif
          endif

          set damp = "-damp"
          if ( $blend == "NONE" ) set damp = ""

          if ( $AENS_DONORECENTER ) then
             dyn_recenter.x -g5 -rc $rec_rcfile $damp $addinflation \
                              $expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX \
                              NONE \
                              NONE
                              
          else
             dyn_recenter.x -g5 -rc $rec_rcfile $damp $addinflation \
                              $expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX \
                              ../ensmean/$expid.${ftype2}.${nymd}_${hhmn}z.$NCSUFFIX \
                              $cenloc/$expid.${ftype2}.${nymd}_${hhmn}z.$NCSUFFIX
          endif
          if (! -e DYNRECENTER_EGRESS ) then
              echo " ${MYNAME}: dyn_recenter failed status($status), aborting ..."
              touch $ensloc/.FAILED
              exit(1)
          else
              touch $ENSWORK/.DONE_MEM${memtag}_${MYNAME}_${ftype1}_${ftype2}.$yyyymmddhh
          endif

      endif

   endif # <failure check>
   cd -
end

# Monitor status of ongoing jobs
# ------------------------------
if ( $ENSPARALLEL ) then
     jobmonitor.csh $nmem ${MYNAME}_${ftype1}_${ftype2} $ENSWORK $yyyymmddhh
     if ($status) then
         echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
         exit(1)
     endif
endif

# Clean up
# --------
@ ic = 1
@ failed = 0
while ( $ic < $nmem + 1 )
   set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
   if ( -e mem$memtag/DYNRECENTER_EGRESS ) then
      /bin/rm mem$memtag/recenter_mem*.j
      /bin/rm mem$memtag/recenter_mem*.e*
   else
      echo "${MYNAME}: looks like member $memtag has not been recentered ..."
      @ failed = 1
   endif
   @ ic = $ic + 1
end
/bin/rm $ENSWORK/recenter_poe.*
#/bin/rm $ENSWORK/recenter_poe*.j

if ($failed) then
    echo "${MYNAME}: not all members recentered, aborting"
    exit(1)
endif
touch $ENSWORK/.DONE_${MYNAME}_${ftype1}_${ftype2}.$yyyymmddhh
exit(0)
