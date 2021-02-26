#!/bin/csh

# atmens_vtrack.csh - calculate TC track from ensemble of forecasts
#
# !REVISION HISTORY:
#
#  01Sep2015  Todling   Initial script
#  23Jun2020  Todling   Redef meaning of ATMENSLOC (is it needed here?)
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 1
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars
setenv MYNAME atmens_vtrack.csh

# The following env variable allows generation of a fake ensemble
# ---------------------------------------------------------------
if ( !($?SIMULATE_ENSEMBLE) ) setenv SIMULATE_ENSEMBLE 0

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - calculate TC track for each member of ensemble"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms vtrkfrq "
   echo " "
   echo " where"
   echo "   expid     -  usual experiment name, e.g., b541iau"
   echo "   nymd      -  starting date of forecast (as in YYYYMMDD)"
   echo "   nhms      -  starting time of forecast (as in HHMMSS)"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED RESOURCE FILES "
   echo " "
   echo "   vtrack.rc       - controls track program"
   echo "   vtrack.ctl.tmpl - grads template for tracking program"
   echo "   vtx.ctl.tmpl    - grads template for relocator program"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES "
   echo " "
   echo "    ATMENSETC       - location of resource files        "
   echo "    FVHOME          - location of experiment            "
   echo "    FVROOT          - location of DAS build             "
   echo "    FVWORK          - location of work directory        "
   echo "    GID             - group id for PBS jobs             "
   echo "    VAROFFSET       - offset min from first synoptic time"
   echo "    VTRKFRQF        - frequency of tracking calculation "
   echo "    VTXLEVS         - pressure levels track works with  "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES "
   echo " "
   echo "    ATMENSLOC        - location of ensemble (default: FVHOME)"
   echo "    NCSUFFIX         - suffix of hdf/netcdf files (default: nc4)"
   echo "    ENSPARALLEL      - when set, runs all ensemble components in parallel "
   echo "                       (default: off)"
   echo "    AENS_VTRACK_DSTJOB- distribute multiple works within smaller jobs"
   echo "    ENSVTRK_NCPUS     - number of cpus used to run tracker"
   echo "    VTRACK_WALLCLOCK  - wall clock time to run vtrack, default 1:00:00 "
   echo "    VTRACK_QNAME      - name of queue (default: NULL, that is, let pbs pick) "
   echo "    STRICT            - sets whether or not tcvitial must be present"
   echo " "
   echo " SEE ALSO"
   echo " "
   echo "   vtrack  - main vortex track driver"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 01Sep2015      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif
 
setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?ATMENSLOC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1
if ( !($?VAROFFSET)     ) setenv FAILED 1
if ( !($?VTRKFRQF)      ) setenv FAILED 1
if ( !($?VTXLEVS)       ) setenv FAILED 1

if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)   ) setenv ENSPARALLEL 0
if ( !($?STRICT)        ) setenv STRICT 1
if ( !($?LOCAL_ACQUIRE) ) setenv LOCAL_ACQUIRE 0
if ( !($?VTRACK_WALLCLOCK))setenv VTRACK_WALLCLOCK 1:00:00
if ( !($?VTRACK_QNAME))    setenv VTRACK_QNAME NULL

if ( $ENSPARALLEL ) then
   if ( !($?ENSVTRK_NCPUS) ) then
     setenv FAILED 1 
   else
     setenv JOBGEN_NCPUS $ENSVTRK_NCPUS
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
set hh       = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}
set timetag    = ${nymd}_${hh}z

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

if ( ! -e $ATMENSETC/vtrack.rc ) then 
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
   echo "${MYNAME}: nothing to do"
   exit(0)
endif

if ( $?FVSPOOL ) then
   set spool = "-s $FVSPOOL "
else
   set diren = `dirname $FVHOME`
   set spool = "-s $diren/spool "
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $SHARE/dasilva/opengrads/Contents $path )

if ( -d $ENSWORK/vtrack ) /bin/rm -r $ENSWORK/vtrack
mkdir $ENSWORK/vtrack
cd $ENSWORK/vtrack

# Acquire tcvitals
# ----------------
if ( -e $FVHOME/run/obsys.rc ) then
   /bin/cp $FVHOME/run/obsys.rc .
else
   echo " \e[0;31m ${MYNAME}: Failed to find obsys.rc, aborting ... \e[0m"
   exit(1)
endif

if( -e $ENSWORK/mem001/CAP.rc ) then
    set jobsgmt = `echorc.x -rc $ENSWORK/mem001/CAP.rc JOB_SGMT`
    @ fcsthrs = ( $jobsgmt[1] * 24 ) + ( $jobsgmt[2] / 10000 )
    @ fcstsec = $fcsthrs * 3600
else
   echo " \e[0;31m ${MYNAME}: cannot determine fcst length, aborting ... \e[0m"
   exit(1)
endif
echo "\e[0;31m ${MYNAME}: forecast length $fcsthrs hrs \e[0m"

# first synoptic time
@ offset_sec = $VAROFFSET * 60
@ offset_hrs = $VAROFFSET / 60
set firstana = `tick $nymd $nhms $offset_sec`
set nymda = $firstana[1]
set nhmsa = $firstana[2]
set hha   = `echo $nhmsa | cut -c1-2`

# end time of forecast
set fcstend  = `tick $nymd $nhms $fcstsec`
set nymde = $fcstend[1]
set nhmse = $fcstend[2]
set hhe   = `echo $nhmse | cut -c1-2`

set strict = ""
if($STRICT) set strict = "-strict"
set tcvitals_class = `grep tcvitals $FVHOME/run/obsys.rc | grep BEGIN | cut -d" " -f2`

@ adjust_fcsthrs = $fcsthrs - $offset_hrs
@ fcsthrs_by6 = $adjust_fcsthrs / 6

 if ( (`uname -n` !~ borg[a-z]???) || ( $LOCAL_ACQUIRE ) ) then

     acquire_obsys -v -d $FVWORK $spool $strict -ssh  \
                         $nymda $nhmsa 060000 1 $tcvitals_class  # acquire tcvitals (6-hrs back); 
     if ( $status ) then
        touch .FAILED_ACQ_TCV
     endif
    
 else

     if( -e .FAILED_ACQ_TCV ) /bin/rm .FAILED_ACQ_TCV
     set fname = "acquire_tcv.pbs"
     alias fname1 "echo \!* >! $fname"
     alias fname2 "echo \!* >> $fname"
     set qsub_acquire = 1
     
     fname1 "#\!/bin/csh -xvf"
     fname2 "#SBATCH --account=$GID"
     fname2 "#SBATCH --partition=datamove"
     fname2 "#SBATCH --time=2:00:00"
     fname2 "#SBATCH --job-name=acquire"
     fname2 "#SBATCH --ntasks=1"
     fname2 "#PBS -N acquire"
     fname2 "#PBS -l nodes=1:ppn=1"
     fname2 "#PBS -S /bin/csh"
     fname2 "#PBS -V"
     fname2 "#PBS -j eo"
     fname2 ""
     fname2 "cd $FVWORK"
     fname2 "acquire_obsys -v -drc $ENSWORK/vtrack/obsys.rc -d $ENSWORK/vtrack $strict $nymda $nhmsa 060000 $fcsthrs_by6 $tcvitals_class"
     fname2 "if ( $status ) then"
     fname2 "   touch .FAILED_ACQ_TCV"
     fname2 "endif"
     fname2 "exit"
     
     if ( $ATMENS_BATCHSUB == "sbatch" ) then
        $ATMENS_BATCHSUB -W $fname
     else
        $ATMENS_BATCHSUB -W block=true $fname
     endif
     sleep 2

 endif

 if( -e .FAILED_ACQ_TCV ) then
    echo " \e[0;31m ${MYNAME}: Failed to acquire tcvitals, aborting ... \e[0m"
    exit(1)
 endif

# Check tcvitals for valid entries
# (assumes tcvitals.yyyymmddhh naming convention)
# ------------------------------------------------
if ( ! -e $ENSWORK/.DONE_gettcvitals.${nymda}${hha}+${nymde}${hhe} ) then
   set notfound = 1
   touch tcvitals.${nymda}${hha}+${nymde}${hhe}
   foreach vitals  ( `/bin/ls *tcvitals*` )
      echo "Checking $vitals for valid entries."
      cat $vitals
      set tcvdate = `echo $vitals | cut -d . -f2 | cut -c 1-8`
      set tcvtime = `echo $vitals | cut -d . -f2 | cut -c 9-10`00
      \rm -f ${vitals}.tmp
      mv $vitals ${vitals}.tmp
      cat ${vitals}.tmp | grep "$tcvdate $tcvtime" > $vitals
      echo "Corrected ${vitals}:"
      cat  $vitals >> tcvitals.${nymda}${hha}+${nymde}${hhe}
      \rm -f ${vitals}.tmp
      set notfound = 0
   end
   if ($notfound) then
      touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
      echo "${MYNAME}: no tcvitals found, nothing to do ..."
      exit(0)
   else
      touch $ENSWORK/.DONE_gettcvitals.${nymda}${hha}+${nymde}${hhe}
   endif
endif
if ( -z tcvitals.${nymda}${hha}+${nymde}${hhe} ) then
   /bin/rm tcvitals.${nymda}${hha}+${nymde}${hhe}
   echo " \e[0;31m ${MYNAME}: Empty tcvitals found, aborting ... \e[0m"
   echo " ${MYNAME}: Complete "
   exit(0)
endif
/bin/cp tcvitals.${nymda}${hha}+${nymde}${hhe} \
        $ENSWORK/updated_ens/$expid.tcvitals.${nymda}${hha}+${nymde}${hhe}.txt

# Back to ens-work directory ...
# ------------------------------
cd $ENSWORK
set members = `/bin/ls -d mem* | wc`
set nmem = $members[1]

# Loop over each member and derive track information for them
# -----------------------------------------------------------
set fpoe = 0
set n = 0
setenv STORE_FVWORK $FVWORK
while ( $n < $nmem )

  @ n = $n + 1
  set nnn = `echo $n | awk '{printf "%03d", $1}'`
  if (! -e $ENSWORK/.DONE_MEM${nnn}_${MYNAME}.$yyyymmddhh ) then

     # need to fake work directory for vtrack script; redefine FVWORK here
     setenv FVWORK $ENSWORK/mem$nnn
     cd $FVWORK
     /bin/cp $ENSWORK/vtrack/tcvitals.${nymda}${hha}+${nymde}${hhe} .
     /bin/cp $ATMENSETC/vtx.ctl.tmpl    .
     /bin/cp $ATMENSETC/vtrack.ctl.tmpl .
     echo "vtrack -trktyp FV5 -strict -fcst -freq $VTRKFRQF -fcsthrs $adjust_fcsthrs -rc $ATMENSETC/vtrack.rc $nymda $nhmsa $expid"
           vtrack -trktyp FV5 -strict -fcst -freq $VTRKFRQF -fcsthrs $adjust_fcsthrs -rc $ATMENSETC/vtrack.rc $nymda $nhmsa $expid
           if ( $status ) then
              echo " \e[0;31m ${MYNAME}: VTRACK failed for member $nnn, aborting ... \e[0m"
              exit(1)
           endif
           /bin/cp *trak.FV5* $ENSWORK/updated_ens/mem$nnn
     cd -
     touch $ENSWORK/.DONE_MEM${nnn}_${MYNAME}.$yyyymmddhh
  endif

end #while
setenv FVWORK $STORE_FVWORK

# if made it here, then exit nicely
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
if ( $idone == $nmem ) then
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
   echo " ${MYNAME}: Complete "
   exit(0)
else
   echo " ${MYNAME}: Failed "
   exit(1)
endif
