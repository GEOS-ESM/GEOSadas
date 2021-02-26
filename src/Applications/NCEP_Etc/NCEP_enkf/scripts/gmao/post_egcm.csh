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
   echo "     Last modified: 08Apr2013      by: R. Todling"
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

if ( !($?PEGCM_QNAME)    ) setenv PEGCM_QNAME    NULL
if ( !($?PEGCM_WALLCLOCK)) setenv PEGCM_WALLCLOCK NULL

if ( $PEGCM_QNAME == "NULL" || $PEGCM_WALLCLOCK == "NULL" ) then
   setenv PEGCM_SERIAL 1
else
   if ( !($?PEGCM_NCPUS)   ) then
      echo "${MYNAME}: must define PEGCM_NCPUS"
   endif
   setenv PEGCM_SERIAL 0
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
if (! -e $ENSWORK/.DONE_redone_allbkgstat_$MYNAME.$yyyymmddhhmn ) then

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
# @ nall = $nt * $ntyps
  @ nall = 0
  set adate = ( `tick $nymdb $nhmsb $toffset_sec` )
  @ n = 0
  @ m = 0
  while ( $n < $nt )
     @ n++
     set this_nymd = $adate[1]
     set this_nhms = $adate[2]
     set this_hhmn = `echo $this_nhms | cut -c1-4`
     set this_yyyymmddhhmn = ${this_nymd}${this_hhmn}
     if (! -e $ENSWORK/.DONE_redone_bkgstat_$MYNAME.$this_yyyymmddhhmn ) then
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

             setenv JOBGEN_NCPUS_PER_NODE 1
             setenv JOBGEN_NCPUS $PEGCM_NCPUS
             jobgen.pl \
                 -q $PEGCM_QNAME       \
                 pegcm_$mmm            \
                 $GID                  \
                 $PEGCM_WALLCLOCK      \
                 "atmens_stats.csh $nmem $outkind $ENSWORK $this_nymd $this_nhms |& tee -a $ENSWORK/pegcm_${outkind}.$this_yyyymmddhhmn.log"\
                 $ENSWORK              \
                 $MYNAME               \
                 $ENSWORK/.DONE_MEM${mmm}_${MYNAME}.$yyyymmddhhmn \
                 "PEGCM Failed"

                 if ( -e pegcm_${mmm}.j ) then
                    $ATMENS_BATCHSUB pegcm_${mmm}.j
                    touch .SUBMITTED
                 else
                    echo " ${MYNAME}: PostEGCM Failed to generate PBS jobs for Member ${mmm}, Aborting ... "
                    touch $ENSWORK/.FAILED
                    exit(1)
                 endif

          endif # parallel jobs
          @ nall = $nall + 1
       end # <outkind>
     endif
     set adate = (`tick $this_nymd $this_nhms $bkgfreq_sec`)
  end
endif

# In case of doing separated jobs, monitor their completion
# ---------------------------------------------------------
if( ! $PEGCM_SERIAL ) then
   jobmonitor.csh $nall $MYNAME $ENSWORK $yyyymmddhhmn
   if ($status) then
       echo "${MYNAME}: cannot complete due to failed jobmonitor, aborting"
       exit(1)
   endif
   # clean up
   # --------
   /bin/rm pegcm_*.j pegcm_*.j.*
   /bin/rm pegcm_*.log
endif
touch $ENSWORK/.DONE_redone_allbkgstat_$MYNAME.$yyyymmddhhmn

# made it down here, all done
# ---------------------------
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhhmn
echo " ${MYNAME}: Complete "
exit(0)
