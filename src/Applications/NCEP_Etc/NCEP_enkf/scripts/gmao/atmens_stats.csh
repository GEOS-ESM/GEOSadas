#!/bin/csh

# atmens_stats - calculate mean and rms of ensemble
#
# !REVISION HISTORY:
#
#  16Nov2011  Todling   Initial script
#  15Apr2012  Todling   Calculate rms in energy for both bkg and ana
#  01Oct2012  Todling   Some parallelization at the level of background jobs
#  14Oct2012  Todling   Rid of reset_time; fixed GFIO_mean to write frq right
#  20Oct2012  Todling   Update API: add nymd nhms to do only desired date/time
#                       Major revamp: parallelized
#  08Feb2013  RT/AElA   Add new calculation of stats (mp_stats.x), but for now
#                       still left old capability in place, just in case.
#  25Mar2013  Todling   Allow mp_stats to run under mpi
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#  02May2020  Todling   Allow for user-spec freq of bkg stat calc
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME atmens_stats.csh

# need usage here
# ---------------
if ( $#argv < 5 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - calculates statistics from ensemble members"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME nmem ftype ensloc nymd nhms"
   echo " " 
   echo " where" 
   echo "   nmem   -  number of members to be created"
   echo "   ftype  -  file type (e.g., bkg.eta, bkg.sfc, ana.eta)"
   echo "   ensloc -  location to place generated ensemble"
   echo "   nymd   -  date of members to calc stats for (as YYYYMMDD)"
   echo "   nhms   -  time of members to calc stats for (as HHMMSS)"
   echo " " 
   echo " DESCRIPTION" 
   echo " " 
   echo "  This script calculates statistics such as mean, RMS and energy-based"
   echo "  spread from ensemble members. Two ways of performing these calculations"
   echo "  are available. These are driven by the present (or absence) of the resource"
   echo "  file mp_stats.rc, which triggers the most efficient way of performing all "
   echo "  required calculations using a single pass through the input data. Alternatively,"
   echo "  a series of calculations is performed where: (i) the mean of the members is "
   echo "  computed, followed by removal of mean from members, followed by (iii) calculation "
   echo "  of RMS; and finally performing (iv) calculation of energy-based spread."
   echo " " 
   echo "  Example of valid command line:"
   echo "   $MYNAME 10 /archive/u/$user/u000_c72/atmens 20111201 210000"
   echo " " 
   echo " REQUIRED RESOURCE FILES"
   echo " " 
   echo "  mp_stats.rc              - needed when calculations use mp_stats.x"
   echo "  atmens_incenergy.rc.tmpl - needed when energy-spread uses pertenergy.x"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    AENSTAT_MPIRUN - command line for exec of mp_stats.x"
   echo "    ASYNBKG    - frequency of background (minutes) "
   echo "    EXPID      - experiment name                   "
   echo "    FVROOT     - location of DAS build             "
   echo "    FVHOME     - location of experiment            "
   echo " " 
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " " 
   echo "    ATMENSETC     - specify to provide location of pert-energy RC file"
   echo "    ENSPARALLEL   - when set, runs all ensemble components in parallel (default: off)"
   echo "    AENSTAT_NCPUS - number of cpus to use for this procedure"
   echo "                    (NOTE: required when ENSPARALLEL is on)"
   echo "    ATMENS_BKGSTATFRQ - specific bkg freq for stats calculation"
   echo " SEE ALSO"
   echo "  mp_stats.x   - program to calculate statistics from fields in SDF files"
   echo "  dyn_diff.x   - program to calculate difference between dyn-vector files"
   echo "  GFIO_mean.x  - program to calculate averages from fields in SDF files"
   echo "  pertenergy.x - calculates energy-based error (squared-difference)"
   echo "  ut_atmens_stats.csh - unity-tester for this script (also helps produce"
   echo "                        statistics when running offline).               "
   echo " " 
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 02May2020      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
setenv DOENERGY 0
if ( !($?ASYNBKG)     )  setenv FAILED   1
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENS_DOMEAN)  ) setenv ATMENS_DOMEAN 1
if ( !($?ENSPARALLEL) )  setenv ENSPARALLEL 0
if ( !($?FVROOT)      )  setenv FAILED   1
if ( !($?FVHOME)      )  setenv FAILED   1
if ( !($?EXPID)       )  setenv FAILED   1
if ( !($?NCSUFFIX)    )  setenv NCSUFFIX nc4

if ( $ENSPARALLEL ) then
   setenv JOBGEN_NCPUS_PER_NODE 2
   if ( !($?AENSTAT_NCPUS) ) then
     setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $AENSTAT_NCPUS
   endif
   if ( !($?GID)               )  setenv FAILED  1
   if ( !($?AENSTAT_QNAME)     )  setenv FAILED  1
   if ( !($?AENSTAT_WALLCLOCK) )  setenv FAILED  1
endif


# The following env variable allows generation of a fake ensemble
# ---------------------------------------------------------------
if ( !($?SIMULATE_ENSEMBLE) ) setenv SIMULATE_ENSEMBLE 0

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set path = ( . $FVHOME/run $FVROOT/bin $path )

if ( $SIMULATE_ENSEMBLE ) then
  set dry_run = echo
else
  set dry_run =
endif

# command line parameters
# -----------------------
set nmem   = $1  # number of ensemble members
set ftype  = $2  # file type (e.g, bkg.eta)
set ensloc = $3  # root location for members and mean
set nymd   = $4  # date of members to calc stats for (YYYYMMDD)
set nhms   = $5  # time of members to calc stats for (HHMMSS)

setenv BKGFREQ $ASYNBKG
if ($?ATMENS_BKGSTATFRQ) then
   setenv BKGFREQ $ATMENS_BKGSTATFRQ
endif

set hh   = `echo $nhms | cut -c1-2`
set hhmn = `echo $nhms | cut -c1-4`
set yyyymmddhhmn =  ${nymd}${hhmn}
set timetagz   =  ${nymd}_${hhmn}z
@ bkgfreq_hr  =  $BKGFREQ / 60
@ bkgfreq_mn  =  $BKGFREQ - $bkgfreq_hr * 60
set bkgfreq_hh = `echo $bkgfreq_hr |awk '{printf "%02d", $1}'`
set bkgfreq_mm = `echo $bkgfreq_mn |awk '{printf "%02d", $1}'`
set bkgfreq_hhmn = ${bkgfreq_hh}${bkgfreq_mm}

if ( ("$ftype" == "bkg.eta" || "$ftype" == "ana.eta") ) then
   if( ($?ATMENSETC) ) then
     if( -e $ATMENSETC/atmens_incenergy.rc.tmpl ) setenv DOENERGY 1
   endif
endif
setenv FAKEMEAN 0
if ( ("$ftype" == "ana.eta" ) ) then
   if( ($?ATMENSETC) ) then
     if( -e $ATMENSETC/easyeana.rc ) setenv FAKEMEAN 1
   endif
endif
set etag  = "NULL"

# get positioned ...
# ------------------
cd $ensloc/
if( !($?ENSWORK) ) then
    setenv ENSWORK $ensloc
endif

# if new stat calculation ...
# ---------------------------
if( ($?ATMENSETC) ) then
  if ( -e $ATMENSETC/mp_stats.rc ) then
     if ( !($?AENSTAT_MPIRUN) ) then
        echo " ${MYNAME}: env(AENSTAT_MPIRUN) not defined, aborting ..."
        exit 1
     endif
     if(! -d ensmean ) mkdir -p $ensloc/ensmean
     if(! -d ensrms  ) mkdir -p $ensloc/ensrms
     cd mem001
     set alltype = `ls *.${ftype}.*${timetagz}.$NCSUFFIX`
     foreach fn ( $alltype )
        set my_date = `echo $fn | cut -d. -f4 | cut -c1-8`
        set my_hhmn = `echo $fn | cut -d. -f4 | cut -c10-13`
        set mopt = "-o   ../ensmean/$fn"
        set sopt = "-stdv ../ensrms/$fn"
        set eopt = ""
        if ("$ftype" == "bkg.eta" || "$ftype" == "ana.eta" || "$ftype" == "prog.eta" ) then
            if("$ftype" == "bkg.eta" ) set etype = "bene.err"
            if("$ftype" == "ana.eta" ) set etype = "aene.err"
            if("$ftype" == "prog.eta") set etype = "pene.err"
            set eopt = "-ene ../ensrms/$EXPID.${etype}.${my_date}_${my_hhmn}z.$NCSUFFIX"
        endif
        if(! -e .MP_STATS_EGRESS_${ftype}_${my_date}${my_hhmn} ) then
           $dry_run $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc $mopt $sopt $eopt -inc ${bkgfreq_hhmn}00 \
                                    -egress .MP_STATS_EGRESS_${ftype}_${my_date}${my_hhmn} ../mem*/$fn
        endif
     end
     # make sure all is successfully done
     foreach fn ( $alltype )
        set my_date = `echo $fn | cut -d. -f4 | cut -c1-8`
        set my_hhmn = `echo $fn | cut -d. -f4 | cut -c10-13`
        if (! -e .MP_STATS_EGRESS_${ftype}_${my_date}${my_hhmn} ) then
           echo " ${MYNAME}: Failed to calculate stats (mp_stats.x) for ${ftype}_${my_date}${my_hhmn}, Aborting ... "
           touch $ENSWORK/.FAILED
           exit(1)
        endif
     end
     exit (0)
  endif
endif

setenv NCPUS 1 # NOTE: for now since there is a memory issue

# Calculate the ensemble mean for each file type and date/time
# ----------------------------------------------------------------
if ( $ATMENS_DOMEAN ) then
  foreach this ( mean )
     set opt = ""
     if( "$this" == "rms" ) set opt = "-rms"
     if(! -d ens$this ) mkdir -p $ensloc/ens$this
     cd mem001
     set alltype = `ls *.${ftype}.*${timetagz}.$NCSUFFIX`
     foreach fn ( $alltype )
        set my_date = `echo $fn | cut -d. -f4 | cut -c1-8`
        set my_hhmn = `echo $fn | cut -d. -f4 | cut -c10-13`
        if ( "$this" == "mean" && $FAKEMEAN ) then 
           $dry_run /bin/cp $fn ../ens$this/$fn
        else
           $dry_run GFIO_mean_r4.x -o ../ens$this/$fn $opt -date $my_date -time ${my_hhmn}00 -inc ${bkgfreq_hhmn}00 ../mem*/$fn &
        endif
     end
     wait
     cd ../
  end
endif # <ATMENS_DOMEAN>

# calculate anomalies
# note: the anomaly directories are removed to make sure the mean is not removed 
#       from the members more than once, this prevents redundancy and error in 
#       cases when the job is submitted over and again due to some small failures.
foreach this ( anom )
     @ n = 0
     @ ic = 0
     while ( $n < $nmem )
        @ n = $n + 1
        set memtag  = `echo $n |awk '{printf "%03d", $1}'`
        if( -e $ensloc/${this}_${ftype}_${memtag} ) /bin/rm -r $ensloc/${this}_${ftype}_${memtag}
        mkdir -p $ensloc/${this}_${ftype}_${memtag}
        cd mem$memtag
        foreach fn ( `ls *.${ftype}.*${timetagz}.$NCSUFFIX` )
           @ ic++
           if ( $ic > $NCPUS ) then
               wait
               @ ic = 1
           endif
           set my_date = `echo $fn | cut -d. -f4 | cut -c1-8`
           set my_hhmn = `echo $fn | cut -d. -f4 | cut -c10-13`
           if ( $ENSPARALLEL ) then

              if ( $FAKEMEAN ) then
                 set cmd = "dyndiff.x -g5 $fn ../ensmean/$fn -o ../${this}_${ftype}_${memtag}/$fn"
              else
                 set cmd = "GFIO_mean_r4.x -o ../${this}_${ftype}_${memtag}/$fn -date $my_date -time ${my_hhmn}00 -inc ${bkgfreq_hhmn}00 1.0,$fn -1.0,../ensmean/$fn"
              endif
              jobgen.pl \
                  -q $AENSTAT_QNAME      \
                  statano_${memtag}      \
                  $GID                   \
                  $AENSTAT_WALLCLOCK     \
                  "$dry_run $cmd"        \
                  $ensloc/mem$memtag     \
                  $MYNAME                \
                  $ensloc/.DONE_MEM${memtag}_${ftype}_STATANO.$yyyymmddhh \
                  "Failed to calculate anomaly"

                  if ( -e statano_${memtag}.j ) then
                     $ATMENS_BATCHSUB statano_${memtag}.j
                     touch .SUBMITTED
                  else
                     echo " ${MYNAME}: Failed to calculate anomaly, Aborting ... "
                     touch $ENSWORK/.FAILED
                     exit(1)
                  endif

           else # <ENSPARALLEL>

              if ( $FAKEMEAN ) then
                 $dry_run dyndiff.x -g5 $fn ../ensmean/$fn -o ../${this}_${ftype}_${memtag}/$fn
              else
                  $dry_run GFIO_mean_r4.x -o ../${this}_${ftype}_${memtag}/$fn -date $my_date \
                                                                               -time ${my_hhmn}00 -inc ${bkgfreq_hhmn}00 \
                                                                                1.0,$fn -1.0,../ensmean/$fn 
             endif
                  if ($status) then
                     echo " ${MYNAME}: Failed to calculate anomaly, Aborting ... "
                     touch $ENSWORK/.FAILED
                     exit(1)
                  endif
           endif # <ENSPARALLEL>
        end
        cd ../
     end # <nmem>
     # Monitor status of ongoing jobs
     # ------------------------------
     if ( $ENSPARALLEL ) then
          jobmonitor.csh $nmem ${ftype}_STATANO $ensloc $yyyymmddhh
          if ($status) then
              echo "${MYNAME}: cannot complete due to failed STATANO jobmonitor, aborting"
              exit(1)
          endif
     endif
     if ( $DOENERGY ) then
        if ("$ftype" == "bkg.eta") then
           set ecase = "benergy"
           set etag  = "BENERGY"
        endif
        if ("$ftype" == "ana.eta") then
           set ecase = "aenergy"
           set etag  = "AENERGY"
        endif
        if (! -e $ensloc/.DONE_MEMALL_${etag}_${yyyymmddhhmn} ) then
           @ n = 0
           while ( $n < $nmem )
              @ n = $n + 1
              set memtag  = `echo $n |awk '{printf "%03d", $1}'`
              cd mem$memtag
              foreach fn ( `ls *.${ftype}.*${timetagz}.$NCSUFFIX` )
                 set my_date = `echo $fn | cut -d. -f4 | cut -c1-8`
                 set my_hhmn = `echo $fn | cut -d. -f4 | cut -c10-13`
                 # edit tmpl and create rc
                 if (! -e atmens_incenergy_${my_date}_${my_hhmn}.rc ) then
                    /bin/rm -f sed_file
                    echo "s/>>>PERT_NAME<<</..\/${this}_${ftype}_${memtag}\/$fn/1"   > sed_file
                    echo "s/>>>AREF_NAME<<</..\/ensmean\/$fn/1"                     >> sed_file
                    /bin/rm -f ./atmens_incenergy_${my_date}_${my_hhmn}.rc
                    sed -f sed_file  $ATMENSETC/atmens_incenergy.rc.tmpl > ./atmens_incenergy_${my_date}_${my_hhmn}.rc
                 endif
    
                 # calculate total energy-based diagnostic
                 if ( $ENSPARALLEL ) then

                    jobgen.pl \
                        -q $AENSTAT_QNAME      \
                        pertene_${memtag}      \
                        $GID                   \
                        $AENSTAT_WALLCLOCK     \
                        "$dry_run pertenergy.x -g5 -o $ecase -expid dummy -pick $my_date ${my_hhmn}00 -epick $my_date ${my_hhmn}00 -rc atmens_incenergy_${my_date}_${my_hhmn}.rc " \
                        $ensloc/mem$memtag     \
                        $MYNAME                \
                        $ensloc/.DONE_MEM${memtag}_${etag}.$yyyymmddhhmn \
                        "Failed to calculate member $memtag energy"

                        if ( -e pertene_${memtag}.j ) then
                           $ATMENS_BATCHSUB pertene_${memtag}.j
                           touch .SUBMITTED
                        else
                           echo " ${MYNAME}: Failed to calculate member $memtag energy, Aborting ... "
                           touch $ENSWORK/.FAILED
                           exit(1)
                        endif

                   else # <ENSPARALLEL>

                        $dry_run pertenergy.x -g5 -o $ecase \
                                              -expid dummy -pick $my_date ${my_hhmn}00 \
                                              -epick $my_date ${my_hhmn}00 \
                                              -rc atmens_incenergy_${my_date}_${my_hhmn}.rc
                        if ($status) then
                           echo " ${MYNAME}: Failed to calculate member $memtag energy, Aborting ... "
                           touch $ENSWORK/.FAILED
                           exit(1)
                        endif


                   endif # <ENSPARALLEL>
              end # <FILETYPE>
              cd ../
           end # <nmem>
           wait
        endif # <DONE_MEMALL>

        # Monitor status of ongoing jobs
        # ------------------------------
        if ( $ENSPARALLEL ) then
             jobmonitor.csh $nmem $etag $ensloc $yyyymmddhhmn
             if ($status) then
                 echo "${MYNAME}: cannot complete due to failed $etag jobmonitor, aborting"
                 exit(1)
             endif
        endif

     endif # <DOENERGY>
end
if ($DOENERGY) then
#   check that calculation of energy went well
    @ n = 0
    @ m = 0
    while ( $n < $nmem )
      @ n = $n + 1
      set memtag  = `echo $n |awk '{printf "%03d", $1}'`
      if (-e $ensloc/mem$memtag/$ecase.${timetagz}.$NCSUFFIX ) then
         @ m = $m + 1
         # clean up ...
         /bin/rm $ensloc/mem$memtag/sed_file $ensloc/mem$memtag/atmens_incenergy_*.rc
      endif
    end
    if( $m == $nmem ) then
       $dry_run touch   $ensloc/.DONE_MEMALL_${etag}_${yyyymmddhhmn}
    endif
endif
# calculate rms
foreach this ( rms )
     set opt = ""
     if( "$this" == "rms" ) set opt = "-rms"
     if(! -e $ensloc/ens$this ) mkdir -p $ensloc/ens$this
     cd anom_${ftype}_001
     set alltype = `ls *.${ftype}.*${timetagz}.$NCSUFFIX`
     foreach fn ( $alltype )
        set my_date = `echo $fn | cut -d. -f4 | cut -c1-8`
        set my_hhmn = `echo $fn | cut -d. -f4 | cut -c10-13`
        $dry_run GFIO_mean_r4.x -o ../ens$this/$fn $opt -date $my_date -time ${my_hhmn}00 -inc ${bkgfreq_hhmn}00 ../anom_${ftype}_*/$fn &
     end
     wait
     foreach fn ( $alltype )
        set my_date = `echo $fn | cut -d. -f4 | cut -c1-8`
        set my_hhmn = `echo $fn | cut -d. -f4 | cut -c10-13`
        if (-e $ensloc/.DONE_MEMALL_${etag}_${yyyymmddhhmn}) then
            if ( $etag == "BENERGY" ) set etype = "bene.err"
            if ( $etag == "AENERGY" ) set etype = "aene.err"
            $dry_run GFIO_mean_r4.x -o ../ens$this/$EXPID.${etype}.${my_date}_${my_hhmn}z.$NCSUFFIX \
                                    -date $my_date -time ${my_hhmn}00 -inc ${bkgfreq_hhmn}00 \
                                   ../mem*/$ecase.${my_date}_${my_hhmn}z.$NCSUFFIX
            if(! $status) then
               $dry_run /bin/rm ../mem*/$ecase.${my_date}_${my_hhmn}z.$NCSUFFIX
            endif
        endif
     end
     cd ../
end
# clean up anomalies
@ n = 0
while ( $n < $nmem )
  @ n = $n + 1
  set memtag  = `echo $n |awk '{printf "%03d", $1}'`
  $dry_run /bin/rm -r $ensloc/anom_${ftype}_$memtag
  $dry_run /bin/rm    $ensloc/mem$memtag/statano_${memtag}.[oej]*
  if ($DOENERGY) then
      $dry_run /bin/rm $ensloc/mem$memtag/pertene_${memtag}.[oej]*
#_RT      $dry_run /bin/rm $ensloc/mem*/.DONE_MEM*_${etag}
  endif
end
#_RT if($DOENERGY) $dry_run /bin/rm $ensloc/.DONE_MEMALL_${etag}_${yyyymmddhhmn}
