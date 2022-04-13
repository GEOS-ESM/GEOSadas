#!/bin/csh

# acquire_atmens.csh - acquire members of Atmos-Ens experiment to allow
#                      GSI to run.
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  09Apr2013  Todling   Update prologue
#  06Apr2016  Todling   Allow to acquire stats tar ball
#  24Feb2020  Todling   Adjust to allow replay from older ebkg tar balls
#  23Jun2020  Todling   Redef meaning of ATMENSLOC
#  06Oct2021  Todling   Use parallel untar
########################################################################

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME "acquire_atmens.csh"

if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - acquire pre-existing Atmos-Ensemble "
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms acqfile "
   echo " "
   echo "  where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  analysis date, as in YYYYMMDD "
   echo "   nhms   -  analysis time, as HHMMSS"
   echo "   acqfile - full path name of acquire rc file"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "   Acquire pre-existing Atmos-Ensemble and make it available"
   echo "   to ongoing experiment. This is to allow running a hybrid-"
   echo "   GSI experiment without the need for regenerating the ensemble."
   echo " "
   echo "   This script is also used to retrieve the pre-existing ensemble "
   echo "   when running observation impact."
   echo " "
   echo "   Example of valid command line:"
   echo "     $MYNAME b541iau 20091018 00 atmens_replay.acq"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES:"
   echo " "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    FVWORK        - work directory where ensemble will fall "
   echo "    GID           - group ID to run job under         "
   echo "    TIMEINC       - analysis frequency (minutes)      "
   echo "    VAROFFSET     - off-time of forecast wrt to 1st synoptic time"
   echo " "
   echo " RESOURCE FILES"
   echo " "
   echo "    atmens_replay.acq  - use this in ensemble replay mode"
   echo "    atmens_asens.acq   - use this when running hybrid adjoint GSI"
   echo " "
   echo " OPTIONAL EVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENS_NCPUSTAR - number of CPUS used for untar (default: 32) "
   echo "    ATMENSLOC       - place where to put acquired ensemble "
   echo "                      (default: FVWORK)                    "
   echo " SEE ALSO"
   echo "    analyzer  - driver for central ADAS analysis"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 06Oct2021      by: R. Todling"
   echo " "
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) then
   if ( ($?BATCH_SUBCMD) ) then
      setenv ATMENS_BATCHSUB $BATCH_SUBCMD
   else
      setenv FAILED 1
   endif
endif
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1
if ( !($?TIMEINC)       ) setenv FAILED 1
if ( !($?VAROFFSET)     ) setenv FAILED 1

if ( !($?ENSACQ_WALLCLOCK)) setenv ENSACQ_WALLCLOCK 2:00:00
if ( !($?ATMENSLOC)       ) setenv ATMENSLOC $FVWORK/atmens
if ( !($?ATMENS_NCPUSTAR) ) setenv ATMENS_NCPUSTAR 32

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid  = $1
set nymda  = $2
set nhmsa  = $3
set acqfile = $4

if (! -e $acqfile ) then
   echo " ${MYNAME}: $acqfile not found, cannot acquire ensemble. Aborting ..."
   touch $FVWORK/.FAILED
   exit(1)
endif

setenv ENSWORK $FVWORK
cd $ENSWORK

if ( $?FVSPOOL ) then
   set spool = "-s $FVSPOOL "
else
   set diren = `dirname $FVHOME`
   set spool = "-s $diren/spool "
endif

@ anafreq_hr   = $TIMEINC / 60
@ varoffset_sc = $VAROFFSET * 60

# time when forecast begining
set beg_fcs = `tick $nymda $nhmsa -$varoffset_sc`
set nymdb = $beg_fcs[1]
set nhmsb = $beg_fcs[2]
set hhb   = `echo $nhmsb | cut -c1-2`
set yyyymmddhh = ${nymdb}${hhb}

# Launch acquire job to retrieve pre-existing ensemble
# ----------------------------------------------------
  setenv JOBGEN_NCPUS          1
  setenv JOBGEN_NCPUS_PER_NODE 1
  jobgen.pl \
       -expid $expid         \
       -q datamove           \
       acq_atmens            \
       $GID                  \
       $ENSACQ_WALLCLOCK     \
       "acquire -v -rc $acqfile -d $ENSWORK $spool -ssh $nymdb $nhmsb ${anafreq_hr}0000 1" \
       $ENSWORK              \
       $MYNAME               \
       $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh \
       "Acquire existing AtmEns Failed"

       if ( -e acq_atmens.j ) then
          if ( $ATMENS_BATCHSUB == "sbatch" ) then
             $ATMENS_BATCHSUB -W acq_atmens.j
          else
             $ATMENS_BATCHSUB -W block=true acq_atmens.j
          endif
       else
          echo " ${MYNAME}: Failed to generate PBS jobs to acquire existing Atmos Ensemble. Aborting ... "
          touch $FVWORK/.FAILED
          exit(1)
       endif

# unravel existing ensemble
# -------------------------
if (! -d $ATMENSLOC ) then
   mkdir $ATMENSLOC
endif

# check that tar ball was indeed retrieved
# ----------------------------------------
set tarballtyps = 
foreach typ ( stat eprg ebkg ) # do not reorder the first two
   grep atmens_${typ} $acqfile
   if (! $status ) then
      set tarballtyps = ( $tarballtyps $typ )
   endif
end

foreach ball ( $tarballtyps ) # do not reorder the first two
   if ( -e $expid.atmens_${ball}.${nymdb}_${hhb}z.tar.gz ) then
      set zipped = 1
   else
      if ( -e $expid.atmens_${ball}.${nymdb}_${hhb}z.tar ) then  # check for already un-zipped file
        set zipped = 0
      else
         if ( $ball == "ebkg" ) then
            echo "${MYNAME}: failed to retrieve tarball $ball w/ ensemble. Aborting ..."
            exit(1)
         else
            echo "${MYNAME}: warning: failed to retrieve tarball $ball w/ ensemble."
            set zipped = -1
         endif
      endif
   endif

   if ( $zipped == 1 ) then
      gunzip $expid.atmens_${ball}.${nymdb}_${hhb}z.tar.gz
   endif
   if ( -e $expid.atmens_${ball}.${nymdb}_${hhb}z.tar ) then
      if ( $ATMENS_NCPUSTAR > 1 ) then
         parallel-untar.py $expid.atmens_${ball}.${nymdb}_${hhb}z.tar $ATMENS_NCPUSTAR
      else
         tar -xvf $expid.atmens_${ball}.${nymdb}_${hhb}z.tar
      endif
      set dummy = (`/bin/ls -1d *.atmens_${ball}.${nymdb}_${hhb}z`)
      set oldexpid = `echo $dummy[1] | cut -d. -f1`
      if ( "$oldexpid" != "$expid" ) then
         if ( ! -d $expid.atmens_${ball}.${nymdb}_${hhb}z ) then
            /bin/mv -T $oldexpid.atmens_${ball}.${nymdb}_${hhb}z $expid.atmens_${ball}.${nymdb}_${hhb}z
         endif
      endif
      if ( $ball == "stat" ) then
         /bin/mv  $expid.atmens_${ball}.${nymdb}_${hhb}z/* $ATMENSLOC
      else  # members already exist 
         cd $expid.atmens_${ball}.${nymdb}_${hhb}z
         foreach dir ( `/bin/ls -d mem*` ) 
            if (! -d $ATMENSLOC/$dir/ ) mkdir -p $ATMENSLOC/$dir/ # just make sure dir exists
            /bin/mv $dir/* $ATMENSLOC/$dir/
         end
         cd - 
      endif
   endif

end # <ball>

# count number of members
set members = `/bin/ls -d $ATMENSLOC/mem* | wc`
set nmem = $members[1]

# Get positioned
cd $ATMENSLOC
touch .no_archiving

# set member count to pre-existing count when available
if ( -d $ATMENSLOC ) then
   set members = `/bin/ls -d mem* | wc`
   set pre_exist_nmem = $members[1]
else
   set pre_exist_nmem = $nmem
endif
echo "Found $pre_exist_nmem ensemble members"

# TBD: need to find a decent way to do this check ...
#     for now, simply don't check - code will crash if
#     less members are available than required.

# rename files to match existing experiment name
@ n = 0
while ( $n < $pre_exist_nmem )

  @ n = $n + 1
  set nnn = `echo $n | awk '{printf "%03d", $1}'`
  cd mem$nnn
  foreach fn (`/bin/ls *bkg* *prog*`)
     set prefix = `echo $fn | cut -d. -f1`
     if ( "$prefix" == "$expid" ) then
        # no need to rename files, exit while
     else
         set suffix = `echo $fn | cut -d. -f2-`
         /bin/mv $fn $expid.$suffix
         echo "renamed member $fn to $expid.$suffix"
     endif
  end 
  # make sure files have 4-digit time
  set lstfn = (`/bin/ls *.bkg.eta.????????_??z.*`)
  if ( $#lstfn > 0 ) then
     foreach fn ( $lstfn )
        set mypfx  = `echo $fn | cut -d. -f1-3`
        set mysfx  = `echo $fn | cut -d. -f5-`
        set mynymd = `echo $fn | cut -d. -f4 | cut -c1-8`
        set myhh   = `echo $fn | cut -d. -f4 | cut -c10-11`
        /bin/mv $fn $mypfx.${mynymd}_${myhh}00z.$mysfx
     end
  endif
  cd -

end
# If ens mean has been brought in, make sure to rename files accordingly
if ( -d ensmean ) then
   cd ensmean
   foreach fn (`/bin/ls *bkg* *prog*`)
      set prefix = `echo $fn | cut -d. -f1`
      if ( "$prefix" == "$expid" ) then
         # no need to rename files, exit while
      else
          set suffix = `echo $fn | cut -d. -f2-`
          /bin/mv $fn $expid.$suffix
          echo "renamed member $fn to $expid.$suffix"
      endif
   end 
   # make sure files have 4-digit time
   set lstfn = (`/bin/ls *.bkg.eta.????????_??z.*`)
   if ( $#lstfn > 0 ) then
      foreach fn ( $lstfn )
         set mypfx  = `echo $fn | cut -d. -f1-3`
         set mysfx  = `echo $fn | cut -d. -f5-`
         set mynymd = `echo $fn | cut -d. -f4 | cut -c1-8`
         set myhh   = `echo $fn | cut -d. -f4 | cut -c10-11`
         /bin/mv $fn $mypfx.${mynymd}_${myhh}00z.$mysfx
      end
   endif
   cd -
endif
# all done
exit(0)
