#!/bin/csh

#atmos_ens2gcm.csh - convert ensemble to what GCM needs (IAU increment) 
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  03Apr2013  Todling   Implement options for distribute multi-work jobs
#                       (no longer use makeiau.csh script)
#  07May2017  Todling   Allow own config of mkiau when running GEPS
#  03May2020  Todling   Logic not to over-subscribe node
#  21Jun2020  Todling   Add ability to handle a control case
#------------------------------------------------------------------
#
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmos_ens2gcm.csh
setenv ENSMEAN 0

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - construct IAU increment for each member "
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  current analysis date "
   echo "   time   -  current analysis time"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "  This procedure constructs IAU increment for each member "
   echo "  of the ensemble from corresponding analysis and background"
   echo "  fields."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "   ATMENSETC       - location of pertinent resource files  "
   echo "   FVHOME          - location of experiment            "
   echo "   FVROOT          - location of DAS build             "
   echo "   FVWORK          - location of work directory        "
   echo "   GID             - group id to run job under         "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "   ATMGEPS        - set when running atmopheric GEOS EPS"
   echo "   AENS_IAU_DSTJOB- distribute multiple works within smaller jobs"
   echo "   NCSUFFIX       - suffix of hdf/netcdf files (default: nc4)"
   echo "   ENSPARALLEL    - when set, runs all ensemble components in parallel "
   echo "                    (default: off)"
   echo "   ENSIAU_NCPUS   - when parallel ens on, this sets NCPUS for IAU calculation"
   echo "   IAU_WALLCLOCK  - wall clock time to run makeiau, default 0:10:00 "
   echo "   IAU_QNAME      - name of queue (default: NULL, that is, let pbs pick) "
   echo "   MPIRUN_ENSIAU  - specifies mprun command line, needed when ENSPARALLEL is on"
   echo " "
   echo " SEE ALSO"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 07May2017      by: R. Todling"
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


if ( !($?ATMGEPS)       ) setenv ATMGEPS 0
if ( !($?AENS_IAU_DSTJOB) ) setenv AENS_IAU_DSTJOB 0
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)   ) setenv ENSPARALLEL 0
if ( !($?IAU_WALLCLOCK) ) setenv IAU_WALLCLOCK 0:10:00
if ( !($?IAU_QNAME)     ) setenv IAU_QNAME NULL

if ( $ENSPARALLEL ) then
   if ( !($?MPIRUN_ENSIAU) ) setenv FAILED 1
   if ( !($?ENSIAU_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $ENSIAU_NCPUS
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
set yyyymmddhh = ${nymd}${hh}

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: all done"
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

if ( $ENSPARALLEL ) then
   set nfiles = `/bin/ls $ENSWORK/.DONE_MEM*_${MYNAME}.$yyyymmddhh | grep -v ENSMEAN | wc -l`
   echo "${MYNAME}: number of already available files  ${nfiles}"
   @ ntodo = $nmem - $nfiles
endif

# Handle a control run when applicable
# ------------------------------------
if ( -d $ENSWORK/ensctrl ) then
   if (! -e  $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh ) then
      cd $ENSWORK/ensctrl

         if(-e agcm_import_ctrl_rst ) /bin/rm agcm_import_ctrl_rst
         touch input.nml

         # prepare proper rc file if necessary ...
         set mkiaurc = $ATMENSETC/mkiau.rc.tmpl
         if ( $ATMGEPS ) then
            if ( -e $FVHOME/run/ageps/mkiau.rc.tmpl ) set mkiaurc = $FVHOME/run/ageps/mkiau.rc.tmpl
         endif
         if ( -e $mkiaurc ) then # this means: running cubed
              /bin/rm -f sed_file
              echo "s/>>>EXPID<<</${expid}/1"         > sed_file
              echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> sed_file
              echo "s/>>>ANADATE<<</${nymd}/1"       >> sed_file
              echo "s/>>>ANATIME<<</${nhms}/1"       >> sed_file
              echo "s/>>>NMEMTAG<<</ensctrl/1"       >> sed_file
              /bin/rm -f ./mkiau.rc
              sed -f sed_file  $mkiaurc  > ./mkiau.rc
              set xcmd = ""
         else
              echo " ${MYNAME}: Cannot find mkiau.rc.tmpl file, Aborting ... "
              exit(1)
         endif

         if( $ENSPARALLEL ) then

          jobgen.pl \
               -egress IAU_EGRESS    \
               -q $IAU_QNAME         \
               iau_ensctrl           \
               $GID                  \
               $IAU_WALLCLOCK        \
               "$MPIRUN_ENSIAU $xcmd |& tee -a $ENSWORK/iau_ensctrl.log " \
               $ENSWORK/ensctrl      \
               $MYNAME               \
               $ENSWORK/.DONE_MEM001_ENSCTRL_${MYNAME}.$yyyymmddhh \
               "IAU Failed"

               if ( -e iau_ensctrl.j ) then
                  $ATMENS_BATCHSUB iau_ensctrl.j
               else
                  echo " ${MYNAME}: Failed to generate PBS jobs for makeiau (ctrl), Aborting ... "
                  touch $ENSWORK/.FAILED
                  exit(1)
               endif

             # Monitor job in case block fails
             # -------------------------------
             jobmonitor.csh 1 ENSCTRL_${MYNAME} $ENSWORK $yyyymmddhh
             if ($status) then
                 echo "${MYNAME}: cannot complete (ctrl) due to failed jobmonitor, aborting"
                 exit(1)
             endif

      else # do serial work
   
          $MPIRUN_ENSIAU $xcmd |& tee -a $ENSWORK/iau_ensctrl.log
          if ($status) then
              echo " ${MYNAME}: Failed to run makeiau.csh (ctrl), Aborting ... "
              exit(1)
          else
              touch IAU_EGRESS
          endif
    
      endif # check for parallel work

      cd -
   endif
endif # ENSCTRL

# Create IAU increments (for now, at same resolution as ensemble)
# ---------------------
# members ...
set fpoe = 0
set ipoe = 0
set npoe = 0
/bin/rm $ENSWORK/iau_poe.*
/bin/rm $ENSWORK/iau_machfile*
@ ic = 0
while ( $ic < $nmem )
   @ ic = $ic + 1
   set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
   if (! -e $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh ) then

      cd mem$memtag
      if(-e agcm_import_mem${memtag}_rst ) /bin/rm agcm_import_mem${memtag}_rst
      touch input.nml

      # prepare proper rc file if necessary ...
      set mkiaurc = $ATMENSETC/mkiau.rc.tmpl
      if ( $ATMGEPS ) then
         if ( -e $FVHOME/run/ageps/mkiau.rc.tmpl ) set mkiaurc = $FVHOME/run/ageps/mkiau.rc.tmpl
      endif
      if ( -e $mkiaurc ) then # this means: running cubed
           /bin/rm -f sed_file
           echo "s/>>>EXPID<<</${expid}/1"         > sed_file
           echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> sed_file
           echo "s/>>>ANADATE<<</${nymd}/1"       >> sed_file
           echo "s/>>>ANATIME<<</${nhms}/1"       >> sed_file
           echo "s/>>>NMEMTAG<<</mem${memtag}/1"  >> sed_file
           /bin/rm -f ./mkiau.rc
           sed -f sed_file  $mkiaurc  > ./mkiau.rc
           set xcmd = ""
      else
           echo " ${MYNAME}: Cannot find mkiau.rc.tmpl file, Aborting ... "
           exit(1)
      endif

      if( $ENSPARALLEL ) then

          @ fpoe++

          if ( $AENS_IAU_DSTJOB != 0 ) then # case of multiple jobs within few larger ones
             # collect multiple iau calls into jumbo file
             if ( $ipoe < $AENS_IAU_DSTJOB ) then  # nmem better devide by AENS_IAU_DSTJOB
                @ ipoe++
                set this_script_name = `pwd`/iau_mem${memtag}.j
                echo $this_script_name >> $ENSWORK/iau_poe.$npoe
                chmod +x $ENSWORK/iau_poe.$npoe
             endif
             set machfile = "-machfile $ENSWORK/iau_machfile$npoe.$ipoe"
          else
             set machfile = ""
          endif

          jobgen.pl \
               -egress IAU_EGRESS $machfile \
               -q $IAU_QNAME         \
               iau_mem${memtag}      \
               $GID                  \
               $IAU_WALLCLOCK        \
               "$MPIRUN_ENSIAU $xcmd |& tee -a $ENSWORK/iau_mem${memtag}.log " \
               $ENSWORK/mem$memtag   \
               $MYNAME               \
               $ENSWORK/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh \
               "IAU Failed"

               if ( $AENS_IAU_DSTJOB != 0 ) then
                  if ( -e iau_mem${memtag}.j ) then
                     chmod +x iau_mem${memtag}.j
                  else
                     echo " ${MYNAME}: Failed to generate IAU PBS jobs for Member ${memtag}, Aborting ... "
                     touch $ENSWORK/.FAILED
                     exit(1)
                  endif
 
                  if ( ($ipoe == $AENS_IAU_DSTJOB) || (($fpoe == $ntodo ) && ($ipoe < $AENS_IAU_DSTJOB) ) ) then
                     set this_ntasks_per_node = `facter processorcount`
                     @ ncores_needed = $ENSIAU_NCPUS / $this_ntasks_per_node
                     if ( $ncores_needed == 0 ) then
                       @ myncpus = $this_ntasks_per_node
                     else
                       if ( $ENSIAU_NCPUS == $ncores_needed * $this_ntasks_per_node ) then
                          @ myncpus = $ENSIAU_NCPUS
                       else
                          @ myncpus = $ENSIAU_NCPUS / $this_ntasks_per_node
                          @ module = $myncpus * $this_ntasks_per_node - $ENSIAU_NCPUS
                          if ( $module != 0 ) @ myncpus = $myncpus + 1
                          @ myncpus = $myncpus * $this_ntasks_per_node
                       endif
                     endif
                     @ myncpus = $ipoe * $myncpus
                     #_ @ myncpus = $AENS_IAU_DSTJOB * $ENSIAU_NCPUS
                     setenv JOBGEN_NCPUS $myncpus
                     jobgen.pl \
                          -egress AIAU_EGRESS -q $IAU_QNAME \
                          iau_dst${npoe}       \
                          $GID                \
                          $IAU_WALLCLOCK    \
                          "job_distributor.csh -machfile $ENSWORK/iau_machfile$npoe -usrcmd $ENSWORK/iau_poe.$npoe -usrntask $ENSIAU_NCPUS -njobs $ipoe" \
                          $ENSWORK  \
                          $MYNAME   \
                          $ENSWORK/.DONE_POE${npoe}_${MYNAME}.$yyyymmddhh \
                          "IAU Failed for Member ${npoe}"
                     /bin/mv iau_dst${npoe}.j $ENSWORK/
                     $ATMENS_BATCHSUB $ENSWORK/iau_dst${npoe}.j
                     touch .SUBMITTED
                     @ ipoe = 0 # reset counter
                     @ npoe++
                  endif 

               else

                  if ( -e iau_mem${memtag}.j ) then
                     $ATMENS_BATCHSUB iau_mem${memtag}.j
                  else
                     echo " ${MYNAME}: Failed to generate PBS jobs for makeiau, Aborting ... "
                     touch $ENSWORK/.FAILED
                     exit(1)
                  endif

              endif # <poe>

      else # do serial work
   
          $MPIRUN_ENSIAU $xcmd |& tee -a $ENSWORK/iau_mem${memtag}.log
          if ($status) then
              echo " ${MYNAME}: Failed to run makeiau.csh, Aborting ... "
              exit(1)
          else
              touch IAU_EGRESS
          endif
    
      endif # check for parallel work

      cd ../

   endif # check for termination of individual case
end

# Monitor status of ongoing jobs
# ------------------------------
 if ( $ENSPARALLEL ) then
      jobmonitor.csh $nmem $MYNAME $ENSWORK $yyyymmddhh
      if ($status) then
          echo " ${MYNAME}: cannot complete due to failed jobmonitor, aborting"
          exit(1)
      endif
 endif

# Double check that all finished properly
# ---------------------------------------
@ ic = 0
while ( $ic < $nmem )
   @ ic = $ic + 1
   set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
   if (! -e mem$memtag/IAU_EGRESS ) then 
      echo " ${MYNAME}: make IAU seem to have failed for member: $memtag"
      touch $ENSWORK/.FAILED
   endif
end
if( -e $ENSWORK/.FAILED ) then
  exit(1)
else
  /bin/rm iau_dst*
  /bin/rm iau_poe*
endif

touch .DONE_${MYNAME}.$yyyymmddhh
exit(0)
