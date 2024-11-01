#!/bin/csh

# post_egcm - post-processing after ensemble of atmospheric GCMs
#
# !REVISION HISTORY:
#
#  20Oct2012  Todling   Initial script (split from gcm_ensemble)
#  19Apr2013  Todling   Linked w/ HISTORY to automatically select
#                       streams to work on and do stats for
#  21Apr2014  Todling   Implement parallelization of stats calculation
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#  30Oct2024  Todling   - Bug fix: job monitor was waiting for last
#                         of date/time set of jobs to complete, but
#                         there is a chance some of those complete
#                         sooner than some from early date/time; now
#                         job-monitor works at date/time level.
#                       - Add option to pack jobs with slurm arrays 
#                         and possibly packable.
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME post_egcm.csh

if ( $#argv < 5 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - post processing after ensemble of GCMs"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid nymd nhms toffset ensloc"
   echo " "
   echo " where"
   echo "   expid   -  usual experiment name, e.g., b541iau"
   echo "   nymd    -  initial date of forecast, as in YYYYMMDD "
   echo "   nhms    -  initial time of forecast, as HHMMSS"
   echo "   toffset -  time offset to start calculating stats (min)"
   echo "   ensloc  -  location of ensemble members"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedure will calculate ensemble statistics for"
   echo "  each of the output streams present in the COLLECTIONS"
   echo "  table of the HISTAENS.rc.tmpl file. When initial-time"
   echo "  specific history exists, this will take precedence over"
   echo "  HISTAENS.rc.tmpl, i.e., if a file HISTAENS_21.rc.tmpl"
   echo "  is present in the ATMENSETC directory, this will be "
   echo "  used to determined the output stream to work from instead. "
   echo " "
   echo "  Alternatively, still, if not all output streams are to be"
   echo "  worked on, the user may specify it's own subset of streams"
   echo "  to calculate statistics for (you must have at least bkg.sfc"
   echo "  and bkg.eta in this list). This can be done by placing a file"
   echo "  named post_egcm.rc under ATMENSETC with a trimmed version of"
   echo "  the COLLECTIONS table in the history RC. The same idea applies"
   echo "  to this file for choices for different initial times, that is,"
   echo "  it is also possible to have a files like post_egcm_21.rc."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091018 210000 360 FVWORK/updated_ens"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ASYNBKG       - frequency of background (minutes) "
   echo "    ATMENSETC     - location of ensemble RC files     "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    TIMEINC       - analysis frequency (minutes)      "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    NCSUFFIX       - suffix of hdf/netcdf files (default: nc4)"
   echo "    PEGCM_ARRAY    - let slurm control distribution of jobs   "
   echo "    PEGCM_PACKL    - let arrays combined with packable jobs   "
   echo "    PEGCM_ALLPARALLEL - parallelize all streams (a little aggressive)"
   echo "                        (default: parallize by date/time)"
   echo " "
   echo " OPTIONAL RESOURCE FILES"
   echo " "
   echo "  post_egcm.rc - user specific collection subset"
   echo " "
   echo " SEE ALSO "
   echo " "
   echo "    atmens_stats.csh - calculates required/desired statistics from ensemble"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 31Oct2024      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ASYNBKG)       ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?TIMEINC)       ) setenv FAILED 1

if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4

if ( !($?PEGCM_ALLPARALLEL) ) setenv PEGCM_ALLPARALLEL  0
if ( !($?PEGCM_ARRAY)    ) setenv PEGCM_ARRAY    0
if ( !($?PEGCM_PACKL)    ) setenv PEGCM_PACKL    0
if ( !($?PEGCM_QNAME)    ) setenv PEGCM_QNAME    NULL
if ( !($?PEGCM_WALLCLOCK)) setenv PEGCM_WALLCLOCK NULL


setenv JOBGEN_NCPUS_PER_NODE -1

if ( $PEGCM_QNAME == "NULL" || $PEGCM_WALLCLOCK == "NULL" ) then
   setenv PEGCM_SERIAL 1
   setenv PEGCM_ALLPARALLEL 0  # override user specs
else
   if ( !($?AENSTAT_NCPUS)   ) then
      echo "${MYNAME}: must define AENSTAT_NCPUS"
      setenv FAILED 1
   else
     setenv JOBGEN_NCPUS $AENSTAT_NCPUS
     setenv JOBGEN_NCPUS_PER_NODE -1
   endif
   setenv PEGCM_SERIAL 0
endif

if ( !($?JOBGEN_PFXNAME) ) then
  set pfxname = ""
else
  set pfxname = ${JOBGEN_PFXNAME}_
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid   = $1
set nymdb   = $2
set nhmsb   = $3
set toffset = $4
set ensloc  = $5

set hhb     = `echo $nhmsb | cut -c1-2`
set hhmnb   = `echo $nhmsb | cut -c1-4`
set yyyymmddhhmn = ${nymdb}${hhmnb}

setenv ENSWORK $ensloc
if (-e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhhmn ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

if ( -e $ENSWORK/.FAILED ) then
   echo " ${MYNAME}: Found $ENSWORK/.FAILED"
   echo " ${MYNAME}: If resubmitting, please remove this file before doing so "
   echo " ${MYNAME}: Cannot proceed until then, aborting ... "
   exit(1)
endif

set packable = ""
if ( $PEGCM_PACKL ) then
  set packable = "-packable"
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]

# Get positioned inside ENSWORK
# -----------------------------
cd  $ENSWORK
touch .no_archiving

# Calculate mean/rms of newly generated ensemble
# ----------------------------------------------
if (! -e $ENSWORK/.DONE_redone_allstat_$MYNAME.$yyyymmddhhmn ) then

  cd $ENSWORK

  # determine history selections to handle
  # --------------------------------------
  if ( -e $ATMENSETC/post_egcm.rc || -e $ATMENSETC/post_egcm_${hhb}.rc ) then
     if ( -e $ATMENSETC/post_egcm_${hhb}.rc ) then
        set this_histrc = $ATMENSETC/post_egcm_${hhb}.rc
     else
        set this_histrc = $ATMENSETC/post_egcm.rc
     endif
     set alltyps = (`echorc.x -rc $this_histrc COLLECTIONS`)
  else
     if ( -e $ATMENSETC/HISTAENS_${hhb}.rc.tmpl ) then
        set this_histrc = $ATMENSETC/HISTAENS_${hhb}.rc.tmpl
     else
        set this_histrc = $ATMENSETC/HISTAENS.rc.tmpl
     endif
     set alltyps = (`edhist.pl -q 3 -list inc -i $this_histrc`)
  endif
  set nn = `echo $alltyps | wc`
  set ntyps = $nn[2] # number of types to handle
  
  # loop over times within this forecast period ...
  # -----------------------------------------------
  @ bkgfreq_sec = $ASYNBKG   * 60
  @ anafreq_sec = $TIMEINC   * 60
  @ toffset_sec = $toffset   * 60
  @ nt = $anafreq_sec / $bkgfreq_sec + 1
  @ ntotal = $nt * $ntyps
  set adate = ( `tick $nymdb $nhmsb $toffset_sec` )
  @ n = 0; @ idx = 0
  while ( $n < $nt )
     @ n++
     set this_nymd = $adate[1]
     set this_nhms = $adate[2]
     set this_hhmn = `echo $this_nhms | cut -c1-4`
     set this_mm   = `echo $this_nymd | cut -c5-6`
     set this_dd   = `echo $this_nymd | cut -c7-8`
     set this_hh   = `echo $this_nhms | cut -c1-2`
     set this_hhzddmm = ${this_hh}Z${this_dd}${this_mm}
     set this_yyyymmddhhmn = ${this_nymd}${this_hhmn}
     if (! -e $ENSWORK/.DONE_redone_stat_$MYNAME.$this_yyyymmddhhmn ) then
       @ m = 0
       foreach outkind ( $alltyps )
          @ m++
          set mmm = `echo $m | awk '{printf "%03d", $1}'`
          if ( $PEGCM_SERIAL ) then

             atmens_stats.csh $nmem $outkind $ENSWORK $this_nymd $this_nhms
             if ($status) then
                echo " ${MYNAME}: trouble calculating stats for $this_nymd $this_nhms, aborting ..."
                exit(1)
             else
                touch $ENSWORK/.DONE_redone_${outkind}stat_$MYNAME.$this_yyyymmddhhmn
             endif

          else # submit stat calls as independent jobs

             if ( $PEGCM_ALLPARALLEL ) then
                @   idx  = $idx + 1
                set idx = `echo $idx | awk '{printf "%03d", $1}'`
                set tagA = $yyyymmddhhmn
                set tagB = $yyyymmddhhmn
             else
                set idx  = $mmm
                set tagA = $this_hhzddmm
                set tagB = $this_yyyymmddhhmn
             endif
             setenv JOBGEN_NCPUS $AENSTAT_NCPUS
             jobgen.pl \
                 -q $PEGCM_QNAME       \
                 pegcm_${idx}.${tagA} \
                 $GID                  \
                 $PEGCM_WALLCLOCK      \
                 "atmens_stats.csh $nmem $outkind $ENSWORK $this_nymd $this_nhms |& tee -a $ENSWORK/pegcm_${outkind}.$this_yyyymmddhhmn.log"\
                 $ENSWORK              \
                 $MYNAME               \
                 $ENSWORK/.DONE_MEM${idx}_${MYNAME}.$tagB \
                 "PEGCM Failed"

                 if ( -e pegcm_${idx}.${tagA}.j ) then
                    chmod +x pegcm_${idx}.${tagA}.j
                    if ( ! $PEGCM_ARRAY ) then
                       $ATMENS_BATCHSUB pegcm_${idx}.${tagA}.j
                       touch .SUBMITTED
                    endif
                 else
                    echo " ${MYNAME}: PostEGCM Failed to generate job for ${mmm}_${this_hhzddmm}, Aborting ... "
                    touch $ENSWORK/.FAILED
                    exit(1)
                 endif

          endif # parallel jobs

       end # <outkind>
       touch $ENSWORK/.DONE_redone_stat_$MYNAME.$this_yyyymmddhhmn

        # In case of parallel jobs ...
        # ----------------------------
        if( ! $PEGCM_SERIAL ) then
           if ( ! $PEGCM_ALLPARALLEL ) then

             # If slurm arrays, launch before monitoring ...
             # ---------------------------------------------
             if ( $PEGCM_ARRAY ) then
                # Note: the parameter called "memtag" in the job-name line below is
                #       is a parameter if jobgen.pl - not of the present program;
                #       the name of the var in jobgen is "memtag", and is properly
                #       set internally in jobgen.
                jobgen.pl \
                     -q $PEGCM_QNAME  $packable \
                     ${pfxname}pegcm_array.$this_hhzddmm \
                     $GID                      \
                     -array "1-${ntyps}" -ncc  \
                     $PEGCM_WALLCLOCK          \
                     pegcm_\${memtag}.${this_hhzddmm}.j \
                     $ENSWORK                  \
                     $MYNAME                   \
                     $ENSWORK/.DONE_ARRAY_${MYNAME}_\${memtag}.$this_yyyymmddhhmn \
                      "PEGCM Array Job Failed"

                if ( -e $ensloc/${pfxname}pegcm_array.$this_hhzddmm.j ) then
                   $ATMENS_BATCHSUB $ensloc/${pfxname}pegcm_array.$this_hhzddmm.j
                else
                   echo " ${MYNAME}: Failed to generate array batch PEGCM jobs, Aborting ... "
                   touch $ensloc/.FAILED
                   exit(1)
                endif
             endif # <ARRAY>

             # Monitor batch jobs
             # ------------------
             jobmonitor.csh $ntyps ${MYNAME} $ENSWORK $this_yyyymmddhhmn
             if ($status) then
                 echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
                 exit(1)
             endif

             # clean up
             # --------
             /bin/rm $ENSWORK/pegcm_*.j
             /bin/rm $ENSWORK/pegcm_*.j.*
             /bin/rm $ENSWORK/*pegcm_*.log

           endif # <.not.PEGCM_ALLPARALLEL>
        endif # <.not.SERIAL>

     endif # <given-date>
     touch $ENSWORK/.DONE_redone_allstat_$MYNAME.$yyyymmddhhmn

     # Increment date/time
     # -------------------
     set adate = (`tick $this_nymd $this_nhms $bkgfreq_sec`)
  end # <date/time>

  # In case of doing separated jobs, monitor their completion
  # ---------------------------------------------------------
  if( $PEGCM_ALLPARALLEL ) then

     # If slurm arrays, launch before monitoring ...
     # ---------------------------------------------
     if ( $PEGCM_ARRAY ) then
        # Note: the parameter called "memtag" in the job-name line below is
        #       is a parameter if jobgen.pl - not of the present program;
        #       the name of the var in jobgen is "memtag", and is properly
        #       set internally in jobgen.
        jobgen.pl \
             -q $PEGCM_QNAME  $packable \
             ${pfxname}pegcm_array.$yyyymmddhhmn \
             $GID                      \
             -array "1-${ntotal}" -ncc \
             $PEGCM_WALLCLOCK          \
             pegcm_\${memtag}.${yyyymmddhhmn}.j \
             $ENSWORK                  \
             $MYNAME                   \
             $ENSWORK/.DONE_ARRAY_${MYNAME}_\${memtag}.$yyyymmddhhmn \
              "PEGCM Array Job Failed"

        if ( -e $ensloc/${pfxname}pegcm_array.$yyyymmddhhmn.j ) then
           $ATMENS_BATCHSUB $ensloc/${pfxname}pegcm_array.$yyyymmddhhmn.j
        else
           echo " ${MYNAME}: Failed to generate array batch PEGCM jobs, Aborting ... "
           touch $ensloc/.FAILED
           exit(1)
        endif
     endif # <ARRAY>

     # Monitor batch jobs
     # ------------------
     jobmonitor.csh $ntotal ${MYNAME} $ENSWORK $yyyymmddhhmn
     if ($status) then
         echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
         exit(1)
     endif

     # clean up
     # --------
     /bin/rm $ENSWORK/pegcm_*.j
     /bin/rm $ENSWORK/pegcm_*.j.*
     /bin/rm $ENSWORK/*pegcm_*.log
  endif # <PEGCM_ALLPARALLEL>

endif

# made it down here, all done
# ---------------------------
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhhmn
echo " ${MYNAME}: Complete "
exit(0)
