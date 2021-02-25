#!/bin/csh

# setperts.csh - preparing the perturbations to use for the 
#                additive inflation
#
# !REVISION HISTORY:
#
#   19Jun2012 El Akkraoui  - initial code
#   26Aug2012 Todling      - some revisions
#   14Oct2012 Todling      - rid of reset_time; fix GFIO frq writeout
#   20Oct2012 Todling      - Update API: frequency now in arg list
#                            also proper error check for env vars
#   09Mar2013 Todling      - Rename inflating perturbation files
#   20Jun2016 Todling      - Differentiate egress when using dyndiff
#                            since all run in same directory
#  31Mar2020  Todling   Jobmonitor to protect against faulty batch-block
#  04Jun2020  Todling   revise parallelization strategy for dyndiff
#  08Jun2020  Todling   revise use of mp_stats in present context
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME setperts.csh

# need usage here
# ---------------
if ( $#argv < 6 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - set perturbations for additive inflation"
   echo " " 
   echo " SYNOPSIS" 
   echo " " 
   echo "  $MYNAME  expid member nymd nhms freq INFLOC"
   echo " " 
   echo " where" 
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   member -  number of member to operate on"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   freq   -  frequency of perturbations, in minutes"
   echo "   INFLOC -  location of inflating perturbations"
   echo "             (set to NONE when not applicable)"
   echo " " 
   echo " DESCRIPTION" 
   echo "    This procedure fetches perturbations from the database"
   echo "  making them available to the present ensemble cycle. This also"
   echo "  invokes the necessary procedures to calculate and remove the mean"
   echo "  from the perturbations"
   echo " " 
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo "  "
   echo "   ATMENSETC - location of ensemble resource files"
   echo "   FVHOME    - location of experiment            "
   echo "   FVROOT    - location of DAS build             "
   echo "   FVWORK    - work directory"
   echo " " 
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "   AENS_PERTS_DSTJOB- distribute multiple works within smaller jobs"
   echo "   ENSPARALLEL - performs calculation in parallel (default: 0)"
   echo "   NCSUFFIX    - suffix for netcdf files (default: nc4)"
   echo " " 
   echo " SEE ALSO "
   echo "   acquire_ensperts.csh - acquire (link) perturbation residing in database"
   echo "   mp_stats.x           - provides one possible way to remove mean of perts"
   echo " " 
   echo " AUTHOR"
   echo "   Amal El Akkraoui (Amal.ElAkkraoui@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0

if ( !($?AENS_PERTS_DSTJOB) ) setenv AENS_PERTS_DSTJOB 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?ENSPARALLEL)   ) setenv ENSPARALLEL 0
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?PERTS_WALLCLOCK) ) setenv FAILED 1
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

#source $FVROOT/bin/g5_modules
set path = ( .  $FVHOME/run $FVROOT/bin $path )

set expid   = $1
set nmem    = $2
set nymd    = $3
set nhms    = $4
set freq    = $5
set ploc    = $6

set hh = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}

setenv ENSWORK $FVWORK
setenv DO_DMGET 1   # force dmget to be used when acquire retrieve files
setenv dry_run

if ( $ENSPARALLEL ) then
   setenv JOBGEN_NCPUS_PER_NODE 2
   if ( !($?ENSRECENTER_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $ENSRECENTER_NCPUS
   endif
endif

set dyndiffx = `which dyndiff.x`
if ( $AENS_PERTS_DSTJOB != 0 ) then
  set dyndiffx = "serial_run $dyndiffx"
endif

if ( -e $ENSWORK/.DONE_MEM001_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

if(! -d $ENSWORK/$ploc ) mkdir -p $ENSWORK/$ploc
cd $ENSWORK/$ploc

@ pfreq_hrs = $freq / 60
set freq_hrs = `echo $pfreq_hrs | cut -c1-2`
set freq_hms = ${freq_hrs}0000

# Get the perturbations from the achive first
# -------------------------------------------
if ( ! -e $ENSWORK/.DONE_MEM001_ACQUIRE_ENSPERTS.$yyyymmddhh ) then 
    jobgen.pl \
       acqperts            \
       $GID                \
       $PERTS_WALLCLOCK    \
       "acquire_ensperts.csh  ${expid} ${nmem} $nymd $nhms $ploc |& tee -a $ENSWORK/acquire_ensperts.log" \
       $ENSWORK/$ploc      \
       $MYNAME             \
       $ENSWORK/.DONE_MEM001_ACQUIRE_ENSPERTS.$yyyymmddhh \
       "Acquire Perturbations Failed"

       if ( -e acqperts.j ) then
          if ( $ATMENS_BATCHSUB == "sbatch" ) then
             $ATMENS_BATCHSUB -W acqperts.j
          else
             $ATMENS_BATCHSUB -W block=true acqperts.j
          endif
       else
          echo " ${MYNAME}: Acquire Perturbations Failed, Aborting ... "
          exit(1)
       endif

       # Monitor job in case block fails
       # -------------------------------
       jobmonitor.csh 1 ACQUIRE_ENSPERTS $ENSWORK $yyyymmddhh
       if ($status) then
           echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
           exit(1)
       endif

else 
   echo " ${MYNAME}: ACQUIRE_ENSPERTS already done."
endif 

# Create temp dir and move original perts here since these are not
# quite the ones we need; we must remove their mean for them to be good
# ---------------------------------------------------------------------
if ( ! -d tmperts ) mkdir -p tmperts
cd tmperts

# Use mp_stats to calculate mean of perturbations and
# generate de-biased perturbations for inflation ...
# ---------------------------------------------------
if ( -e $ATMENSETC/mp_stats_perts.rc ) then

   if (!($?PERTS_ENSTAT_MPIRUN)) then
      echo "${MYNAME}: error, need PERTS_ENSTAT_MPIRUN env to run mp_stats, aborting"
      exit(1)
   endif
   if (!($?PERTS_NCPUS)) then
      echo "${MYNAME}: error, need PERTS_NCPUS env to run mp_stats, aborting"
      exit(1)
   endif

   if (! -e .MP_STATS_EGRESS_perts_${yyyymmddhh} ) then

      foreach fn ( `ls ../$expid.nmcpert.eta.${nymd}_${hh}z.*.$NCSUFFIX` )
         /bin/mv $fn .
      end

      $dry_run $PERTS_ENSTAT_MPIRUN -tmpl ../${expid}.nmcpert.eta.%y4%m2%d2_%h2z -alpha -1.0 \
                          -rc $ATMENSETC/mp_stats_perts.rc \
                          -date $nymd ${hh}0000 -egress .MP_STATS_EGRESS_perts_${yyyymmddhh} \
                          $expid.nmcpert.eta.${nymd}_${hh}z.*.$NCSUFFIX
      if (! -e .MP_STATS_EGRESS_perts_${yyyymmddhh} ) then
         echo " ${MYNAME}: Failed to remove mean of perturbationss (mp_stats.x), Aborting ... "
         touch $ENSWORK/.FAILED
         exit(1)
      endif
   endif

else

  # Calculate perturbations mean
  # ----------------------------
  if ( ! -e $ENSWORK/.DONE_PERTMEAN.$yyyymmddhh ) then

     foreach fn ( `ls ../$expid.nmcpert.eta.${nymd}_${hh}z.*.$NCSUFFIX` )
        /bin/mv $fn .
     end
     set pertmean = $expid.gsimean.eta.${nymd}_${hh}z.$NCSUFFIX
     if ( -e ${pertmean} ) /bin/rm ${pertmean}
  
     $dry_run GFIO_mean_r4.x -o ${pertmean} -date ${nymd} -time ${hh}0000 -inc $freq_hms \
                                 $expid.nmcpert.eta.${nymd}_${hh}z.*.$NCSUFFIX
     if ( $status ) then
        echo "${MYNAME}: cannot calculate perturbation mean"
        exit 1
     else
        touch $ENSWORK/.DONE_PERTMEAN.$yyyymmddhh 
     endif
  else
     echo " ${MYNAME}: PERTURBATION MEAN already done."
  endif

  # Still inside tmperts, check that mean file is indeed present
  # ------------------------------------------------------------
  set pertmean = ${expid}.gsimean.eta.${nymd}_${hh}z.${NCSUFFIX}
  if ( ! -e ${pertmean} ) then
     echo " ${MYNAME}: somehow can no longer find ${expid}.gsimean.eta.${nymd}_${hh}z.${NCSUFFIX}, aborting ..."
     exit(1)
  endif

#  If so, check on what is left to do in terms of removing mean
#  ------------------------------------------------------------
   if ( $ENSPARALLEL ) then
      set nfiles = `/bin/ls .DONE_MEM*_PERTDIFF.$yyyymmddhh | grep -v ENSMEAN | wc -l`
      echo "${MYNAME}: number of already available files  ${nfiles}"
      @ ntodo = $nmem - $nfiles
   endif


  # Still inside tmperts, remove mean from perturbations
  # but now place resulting mean-free member under $ploc
  # ----------------------------------------------------
  /bin/rm $ENSWORK/perts_poe.*
  /bin/rm $ENSWORK/perts_machfile*
  @ fpoe = 0
  @ ipoe = 0
  @ npoe = 0
  @ ic = 1
  while ( $ic < $nmem + 1 )
        set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
        set pertmem  = ${expid}.nmcpert.eta.${nymd}_${hh}z.${memtag}.${NCSUFFIX}
        if ( ! -e ${pertmem} ) then
             echo " ${MYNAME}: somehow can no longer find ${pertmem}, aborting ..."
  	   exit(1)
        endif

        if ( $ENSPARALLEL ) then

             @ fpoe++

             if ( $AENS_PERTS_DSTJOB != 0 ) then # case of multiple jobs within few larger ones
                # collect multiple pertdiff calls into jumbo file
                if ( $ipoe < $AENS_PERTS_DSTJOB ) then # nmem better devide by AENS_PERTS_DSTJOB
                   @ ipoe++
                   set this_script_name = `pwd`/pertdiff_${memtag}.j
                   echo $this_script_name >> $ENSWORK/perts_poe.$npoe
                   chmod +x $ENSWORK/perts_poe.$npoe
                endif
                set machfile = "-machfile $ENSWORK/perts_machfile$npoe.$ipoe"
             else
                set machfile = ""
             endif

             jobgen.pl \
                 -egress DYNDIFF_EGRESS_${memtag} \
                 -q $PERTS_QNAME        \
                 pertdiff_${memtag}     \
                 $GID                   \
                 $PERTS_WALLCLOCK       \
                 "$dry_run $dyndiffx -g5 $pertmem $pertmean -o ../$pertmem -egress DYNDIFF_EGRESS_${memtag}" \
                 $ENSWORK/$ploc/tmperts \
                 $MYNAME                \
                 $ENSWORK/.DONE_MEM${memtag}_PERTDIFF.$yyyymmddhh \
                 "Failed to remove mean from perturbation"
   
             if ( $AENS_PERTS_DSTJOB != 0 ) then
                if ( -e pertdiff_${memtag}.j ) then
                   chmod +x pertdiff_${memtag}.j
                else
                   echo " ${MYNAME}: PERTDIFF Failed to generate PBS jobs for Member ${memtag}, Aborting ... "
                   touch $ENSWORK/.FAILED
                   exit(1)
                endif

                if ( ($ipoe == $AENS_PERTS_DSTJOB) || (($fpoe == $ntodo ) && ($ipoe < $AENS_PERTS_DSTJOB) ) ) then
                   @ myncpus = $ipoe
                   setenv JOBGEN_NCPUS $myncpus
                   setenv PERTS_NCPUS 1  # dyndiff is a serial program
                   jobgen.pl \
                        -q $PERTS_QNAME     \
                        perts_dst${npoe}    \
                        $GID                \
                        $PERTS_WALLCLOCK    \
                        "job_distributor.csh -machfile $ENSWORK/perts_machfile$npoe -usrcmd $ENSWORK/perts_poe.$npoe -usrntask $PERTS_NCPUS -njobs $ipoe " \
                        $ENSWORK            \
                        $MYNAME             \
                        $ENSWORK/.DONE_POE${npoe}_${MYNAME}.$yyyymmddhh \
                        "PERTS Failed for Member ${npoe}"
                   /bin/mv perts_dst${npoe}.j $ENSWORK/
                   # this job is really not monitored; the real work done by pertdiff_mem${memtag}.j is monitored
                   $ATMENS_BATCHSUB $ENSWORK/perts_dst${npoe}.j
                   touch .SUBMITTED
                   @ ipoe = 0 # reset counter
                   @ npoe++
                endif

             else

                if ( -e pertdiff_${memtag}.j ) then
                   $ATMENS_BATCHSUB pertdiff_${memtag}.j
                   touch .SUBMITTED
                else
                   echo " ${MYNAME}: Failed to remove mean from perturbation, Aborting ... "
                   touch $ENSWORK/.FAILED
                   exit(1)
                endif

             endif # <DSTJOB>


       else # sequential calculations

          cd $ENSWORK/$ploc/tmperts
          $dry_run $dyndiffx -g5 $pertmem $pertmean -o ../$pertmem
          if ( $status ) then
               echo " ${MYNAME}: Failed to remove mean from perturbation, Aborting ... "
               exit(1)
          else
             touch $ENSWORK/.DONE_MEM${memtag}_PERTDIFF.$yyyymmddhh
          endif
          cd -
  
       endif
       @ ic = $ic + 1
  end
  cd $ENSWORK

  # Monitor status of ongoing jobs
  # ------------------------------
  if ( $ENSPARALLEL ) then
       jobmonitor.csh $nmem PERTDIFF $ENSWORK $yyyymmddhh
       if ($status) then
           echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
           exit(1)
       endif
  endif

endif

# Clean up
# --------
#@ ic = 1
#while ( $ic < $nmem + 1 )
#   set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
#   /bin/rm $ENSWORK/$ploc/tmperts/pertdiff_${memtag}.j 
#   /bin/rm $ENSWORK/$ploc/tmperts/pertdiff_${memtag}.e*
#   @ ic = $ic + 1
#end
cd $ENSWORK

if ( ! -d updated_ens/perts ) mkdir -p updated_ens/perts
cd updated_ens/perts
set n = 0
while ( $n < $nmem )
    @ n = $n + 1
    set nnn = `echo $n | awk '{printf "%03d", $1}'`
    set fname = "$ENSWORK/$ploc/$expid.nmcpert.eta.${nymd}_${hh}z.$nnn.$NCSUFFIX"
    if( -e $fname ) then
       ln -sf $fname .
    else
       echo " ${MYNAME}: somehow cannot find $fname, aborting ..."
       exit(1)
    endif
end
cd -

# If here, ended successfully
touch $ENSWORK/.DONE_MEM001_${MYNAME}.$yyyymmddhh
exit 0
