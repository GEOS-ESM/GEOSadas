#!/bin/csh -f

# setobvr - setup observer (including required observation files)
#
# !REVISION HISTORY:
#
#  02Oct2011  Todling   Stripped off from fvssi
#  07Nov2011  Todling   For the ensemble component the bias/bang files
#                       are those from the central analysis, therefore
#                       acquired at the analysis date/time, YMD,HMS
#  04Feb2012  Todling   allow for DAS-concurrent level of parallelism
#                       when running ensemble (this needs satbias from
#                       previous analysis time, ie, start time of window)
#                       (ie, relax assumption of 07Nov2011)
#  20Mar2014  El Akkraoui  Added append_gsigcrc to handle GSI_GridComp
#  18Jan2015  Todling   Adjustment to allow scheduler to run
#  26Oct2017  MJ Kim    Add references to Y. Zhu sat-bias-correction files
#  16Apr2018  Todling   Revise access to Y. Zhu sat-bias-correction files
#  15Feb2020  Todling   Allow acquire to work as non-batch call
#  20Jun2020  Todling   Minor changes for flexible location of RC files
#------------------------------------------------------------------

  if ( !($?ATMENS_VERBOSE) ) then
      setenv ATMENS_VERBOSE 0
  else
      if ( $ATMENS_VERBOSE )  set echo
  endif

  if ( !($?ATMENS_BATCHSUB) ) then
     echo "setobvr.csh: missing batch command"
     exit(1)
  endif

# This scripts acquires data from storage and runs the analyzer

  set myname = `basename $0`

  if ( $#argv < 1 ) then
      goto usage
      exit(0)
  endif

  set strict =    "" # by default do not be strict about input files
  set log    =     0 # by default do not log warnings or errors

  setenv FAILED 0
  setenv SATBNOW 0
  if ( !($?ASYNBKG)        )  setenv ASYNBKG  360
  if ( !($?ATMENSETC)      )  setenv FAILED   1
  if ( !($?ENSOBSVR)       )  setenv ENSOBSVR 1
  if ( !($?ENSPARALLEL)    )  then
        setenv SATBNOW  $ENSOBSVR
  else
        setenv SATBNOW  $ENSPARALLEL
  endif
  if ( $ENSOBSVR < 0       )  setenv SATBNOW  $ENSOBSVR
  if ( !($?GID)            )  setenv FAILED   1
  if ( !($?LOCAL_ACQUIRE)  )  setenv LOCAL_ACQUIRE 0
  if ( !($?ACFTBIAS)       )  setenv FAILED 1
  if ( !($?NCSUFFIX)       )  setenv NCSUFFIX nc4
  if ( !($?SPECRES)        )  setenv FAILED   1
  if ( !($?STRICT)         )  setenv STRICT   0
  if ( !($?TIMEINC)        )  setenv FAILED   1
  if ( !($?VAROFFSET)      )  setenv FAILED   1

  if ( $FAILED ) then 
       env
       echo " ${myname}: not all required env vars defined"
       exit 1
  endif

# Determine frequency of background availability
# ----------------------------------------------
  @ bkgfq = $ASYNBKG / 60
  @ bkgfq = $bkgfq * 10000 + ( $ASYNBKG - $bkgfq * 60 ) * 100
  set asyn =
  if ( $ASYNBKG != 360 ) set asyn = "-no_asyn"

  # initialize flags
  # skip transforms
  set STF = ""
  # skip solver
  set SKA = ""
  # skip satbias
  set SSB = ""

# Parse command line
# ------------------
  if ( $#argv > 0 ) then
  foreach token ( $argv )
     if ( "$token" == "-h" ) then
          goto usage
     endif
     if ( "$token" == "-strict" ) then
          set strict = "-strict"  # bomb if not all input files are there
          setenv STRICT 1
          shift
     endif
     if ( "$token" == "-obsclass" ) then
          shift
          set obsclass = $1
          shift
     endif
     if ( "$token" == "-log" ) then
          set log = 1  # write warning and error messages to log
          shift
     endif
     if ( "$token" == "-skipSOLVER" ) then
          set SKA = "-skipSOLVER"
          shift
     endif
     if ( "$token" == "-skipTRANSF" ) then
          set STF = "-skipTRANSF"
          shift
     endif
     if ( "$token" == "-skipSATBIAS" ) then
          set SSB = "-skipSATBIAS"
          shift
     endif
  end
  endif
  if ( $#argv < 5 ) then
       echo $myname": insufficient command line arguments"
       goto usage
  else
       setenv FVHOME  $1
       setenv FVWORK  $2
       setenv YMD     $3
       setenv HMS     $4
       setenv IDENT   $5
       setenv EXPID   `basename $FVHOME`

       set specres   = $SPECRES
       set expid     = $EXPID
       set hh        = `echo $HMS | cut -c1-2`
  endif

# Check relevant env variables
# ----------------------------
  if ( $?FVSPOOL ) then
     set spool = "-s $FVSPOOL "
  else
     set diren = `dirname $FVHOME`
     set spool = "-s $diren/spool "
  endif


# Check whether relevant directories actually exist
# -------------------------------------------------
  if ( ! (-d $FVHOME) ) then
    echo $myname": cannot find FVHOME directory $FVHOME"
    exit 1
  endif
  if ( ! (-d $FVWORK) ) then
    echo $myname": cannot find FVWORK directory $FVWORK"
    exit 1
  endif

#                 -------------------------------------
#                  PART I - Prepare Working Directory
#                 -------------------------------------

# Set up working directory and copy all the restart files along
# with analysis resource files, model namelists and diagnostic table
# file to this directory. The simulation is carried out here in
# the working directory.
# ------------------------------------------------------------------
  cd $FVWORK

# Copy resource and restart files to here
# ---------------------------------------
  /bin/cp $FVHOME/run/*.arc  .      # archiving rules
  /bin/cp $FVHOME/run/*.acq  .      # acquiring rules
  /bin/cp $FVHOME/run/*.rc   .      # acquire resource file
  /bin/cp $FVHOME/run/*.tmpl .      # acquire namelist template files
  /bin/cp $FVHOME/run/*table .      # buffer tables (only needed for testing)
  /bin/cp $FVHOME/run/*.tbl  .      # another suffix for tables!!
  /bin/cp $FVHOME/run/prepob* .     # oiqc-related files

# Overwrite rc-files from run-dir with application specific files
# ---------------------------------------------------------------
  /bin/cp $ATMENSETC/GSI_GridComp.rc.tmpl .
  /bin/cp $ATMENSETC/satbias.acq          .


  # acquire initial conditions
  
  @ aoffset_hrs   = $VAROFFSET  / 60;   @ aoffset_sec    = $VAROFFSET  * 60
  @ varwindow_hrs = $TIMEINC    / 60;   @ varwindow_sec  = $TIMEINC    * 60
  @ nbtimes       = $TIMEINC / ( $ASYNBKG ) + 1
  if ( $ASYNBKG == 360 ) set nbtimes = 1

  set buf   = `tick $YMD $HMS -$aoffset_sec`       # tick clock to the start of time window
  set nymdm = $buf[1]
  set nhmsm = $buf[2]
  @ thishrs = ${nhmsm} / 10000
  set hhm   = `echo $thishrs |awk '{printf "%02d", $1}'` # two-digit hour this seg ends

  set buf  = ( `tick $YMD $HMS -$varwindow_sec` )   # previous analysis time
  set nymd0 = $buf[1]
  set nhms0 = $buf[2]
  set hh0   = `echo $buf[2] | cut -c1-2`
  set qsub_acquire = 0  

# Run acquire_obsys (note only 1 synoptic time)
# -----------------

  @ notimes = $TIMEINC / 360                 # number of 6-hr times to retrieve obs
  set buf = `tick $YMD $HMS -$varwindow_sec` # tick clock 6-hr back
  set nymd1 = $buf[1]
  set nhms1 = $buf[2]
  set hh1   = `echo $nhms1 | cut -c1-2`

  if ( $obsclass != "NONE" ) then

     if ( (`uname -n` !~ borg[a-z]???) || ( $LOCAL_ACQUIRE ) ) then
   
          acquire_obsys -v -d $FVWORK $spool $strict -ssh  \
                              $nymd1 $nhms1 060000 1 ncep_tcvitals  # acquire tcvitals (6-hrs back); 

          acquire_obsys -v -d $FVWORK $spool $strict -ssh $YMD $HMS 060000 $notimes $obsclass

     else # if on discover, acquire via PBS job
          # -----------------------------------
           set fname = "acquire_obs.pbs" 
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
           fname2 "#PBS -l walltime=1:00:00"
           fname2 "#PBS -q datamove"
           fname2 "#PBS -S /bin/csh"
           fname2 "#PBS -V"
           fname2 "#PBS -j eo"
           fname2 ""
           fname2 "cd $FVWORK"
           fname2 "acquire_obsys -v -d $FVWORK $spool $strict -ssh $nymd1 $nhms1 060000 1 ncep_tcvitals" 
           fname2 "acquire_obsys -v -d $FVWORK $spool $strict -ssh $YMD $HMS 060000 $notimes $obsclass"
           fname2 "exit"
   
           if ( $ATMENS_BATCHSUB == "sbatch" ) then
              $ATMENS_BATCHSUB -W $fname
           else
              $ATMENS_BATCHSUB -W block=true $fname
           endif
           sleep 2

     endif # <acquire block>

# Get info about pre-qc files to be combined for QC
# ------------------------------------------------------
  if ( -e obsys.acq ) then

     /bin/cp obsys.acq  ${FVHOME}/run/obsys.acq.latest
     grep pre-qc obsys.acq | grep -v ^\# | cut -d\> -f2 > pre-qc.acq
     cat pre-qc.acq
     ls *.prepbufr.*
     if ( $status && ! -z pre-qc.acq && -e pre-qc.acq ) then
        /bin/mv obsys.acq obsys.acq.orig
        grep -v pre-qc obsys.acq.orig > obsys.acq
        echo "/this/is/a/dummy/path =>  $EXPID.prepbufr.%y4%m2%d2.t%h2z.blk" >> obsys.acq
        /bin/mv ${FVHOME}/run/obsys.acq.latest ${FVHOME}/run/obsys.acq.raw
        /bin/cp obsys.acq  ${FVHOME}/run/obsys.acq.latest
     endif

  endif

  # Fix unblocked files
  # -------------------
    zeit_ci.x FixUnblocked
    foreach ublk ( `ls *.ublk` )
        FixUnblocked.csh $ublk &
    end
    wait
    ls *FAILED_BLOCK
    if ( ( ! $status ) && ( $STRICT ) ) exit 96
    zeit_co.x FixUnblocked


  # Fix beg_endian/little_endian in the upa-buffer files on OSF1 systems
  # --------------------------------------------------------------------
    zeit_ci.x FixEndian
    if ( ( `uname -s` == "OSF1" ) || (`uname -s` == "Linux" && (`uname -m` == "ia64" || `uname -m` == "x86_64")) ) then
       foreach upa ( `ls *upabufr*bfr` )
           Reblock.csh $upa &
       end
       foreach blk ( `ls *.blk` )
           Reblock.csh $blk &
       end
    endif
    wait
    ls *FAILED_BLOCK
    if ( ( ! $status ) && ( $STRICT ) ) exit 96
    zeit_co.x FixEndian

  endif  # <obsclass>

# Acquire satbias files
# ---------------------
  if ( (`uname -n` !~ borg[a-z]???) || ( $LOCAL_ACQUIRE ) ) then

       if ($ENSOBSVR) then
           if ($ENSOBSVR < 0) then
              echo "Bias correction files expected to be local"
           else
             if ( $SATBNOW == 2 ) then
                 acquire -v -rc satbias.acq -d $FVWORK $spool -ssh $nymdm $nhmsm 060000 1
             else 
                 acquire -v -rc satbias.acq -d $FVWORK $spool -ssh $YMD $HMS     060000 1
             endif
          endif
       else
           acquire -v -rc satbias.acq -d $FVWORK $spool -ssh $nymd0 $nhms0 060000 1
       endif

  else # if on discover, acquire via PBS job
       # -----------------------------------
        set fname = "acquire_satbias.pbs" 
        alias fname1 "echo \!* >! $fname"
        alias fname2 "echo \!* >> $fname" 

        set qsub_acquire = 1

        fname1 "#\!/bin/csh -xvf"
        fname2 "#SBATCH --account=$GID"
        fname2 "#SBATCH --job-name=acquire"
        fname2 "#SBATCH --ntasks=1"
        fname2 "#SBATCH --time=1:00:00"
        fname2 "#SBATCH --partition=datamove"
        fname2 "#PBS -N acquire"
        fname2 "#PBS -l nodes=1:ppn=1"
        fname2 "#PBS -l walltime=1:00:00"
        fname2 "#PBS -q datamove"
        fname2 "#PBS -S /bin/csh"
        fname2 "#PBS -V"
        fname2 "#PBS -j eo"
        fname2 ""
        fname2 "cd $FVWORK"
        if ($ENSOBSVR) then
           if ($ENSOBSVR < 0) then
              echo "Bias correction files expected to be local"
           else
              if ( $SATBNOW == 2 ) then
                  fname2 "acquire -v -rc satbias.acq -d $FVWORK $spool -ssh $nymdm $nhmsm 060000 1"
              else 
                  fname2 "acquire -v -rc satbias.acq -d $FVWORK $spool -ssh $YMD $HMS     060000 1"
              endif 
           endif
        else
           fname2 "acquire -v -rc satbias.acq -d $FVWORK $spool -ssh $nymd0 $nhms0 060000 1"
        endif
        fname2 "exit"

        if ( $ATMENS_BATCHSUB == "sbatch" ) then
           $ATMENS_BATCHSUB -W $fname
        else
           $ATMENS_BATCHSUB -W block=true $fname
        endif
        sleep 2

  endif # <acquire block>

# Satellite biases need to be from previous time
# ----------------------------------------------
  if ( $ENSOBSVR ) then
     if ( $SATBNOW < 0 ) then
        if(-e $expid.ana.satbias.${nymd1}_${hh1}z.txt) /bin/mv $expid.ana.satbias.${nymd1}_${hh1}z.txt satbias
        if(-e $expid.ana.satbang.${nymd1}_${hh1}z.txt) /bin/mv $expid.ana.satbang.${nymd1}_${hh1}z.txt satbang
        if(-e $expid.ana.satbiaspc.${nymd1}_${hh1}z.txt) /bin/mv $expid.ana.satbiaspc.${nymd1}_${hh1}z.txt satbiaspc
        if ($ACFTBIAS) then
           if(-e $expid.ana.acftbias.${nymd1}_${hh1}z.txt) /bin/mv $expid.ana.acftbias.${nymd1}_${hh1}z.txt acftbias
        endif
     else
        if ( $SATBNOW == 1 ) then
             if(-e $expid.ana.satbias.${YMD}_${hh}z.txt) /bin/mv $expid.ana.satbias.${YMD}_${hh}z.txt satbias
             if(-e $expid.ana.satbang.${YMD}_${hh}z.txt) /bin/mv $expid.ana.satbang.${YMD}_${hh}z.txt satbang
             if(-e $expid.ana.satbiaspc.${YMD}_${hh}z.txt) /bin/mv $expid.ana.satbiaspc.${YMD}_${hh}z.txt satbiaspc
             if ($ACFTBIAS) then
               if(-e $expid.ana.cftbias.${YMD}_${hh}z.txt) /bin/mv $expid.ana.acftbias.${YMD}_${hh}z.txt acftbias
             endif
        else if ( $SATBNOW == 2 ) then
             if(-e $expid.ana.satbias.${nymdm}_${hhm}z.txt) /bin/mv $expid.ana.satbias.${nymdm}_${hhm}z.txt satbias
             if(-e $expid.ana.satbang.${nymdm}_${hhm}z.txt) /bin/mv $expid.ana.satbang.${nymdm}_${hhm}z.txt satbang
             if(-e $expid.ana.satbiaspc.${nymdm}_${hhm}z.txt) /bin/mv $expid.ana.satbiaspc.${nymdm}_${hhm}z.txt satbiaspc
             if ($ACFTBIAS) then
                if(-e $expid.ana.acftbias.${nymdm}_${hhm}z.txt) /bin/mv $expid.ana.acftbias.${nymdm}_${hhm}z.txt acftbias
             endif
        else
             echo " ${myname}: trouble, cannot obtain satbias/bang files, aborting ..."
             exit(1)
        endif
     endif
  else
     if(-e $expid.ana.satbias.${nymd1}_${hh1}z.txt) /bin/mv $expid.ana.satbias.${nymd1}_${hh1}z.txt satbias
     if(-e $expid.ana.satbang.${nymd1}_${hh1}z.txt) /bin/mv $expid.ana.satbang.${nymd1}_${hh1}z.txt satbang
     if(-e $expid.ana.satbiaspc.${nymd1}_${hh1}z.txt) /bin/mv $expid.ana.satbiaspc.${nymd1}_${hh1}z.txt satbiaspc
     if ($ACFTBIAS) then
        if(-e $expid.ana.acftbias.${nymd1}_${hh1}z.txt) /bin/mv $expid.ana.acftbias.${nymd1}_${hh1}z.txt acftbias
     endif
  endif

# Append observation table to the grid-comp
#  -----------------------------------------
  if ( $obsclass != "NONE" ) then
       append_gsigcrc.pl $FVWORK/obsys.rc GSI_GridComp.rc
       if ( $status ) then
            echo "${myname}: trouble appending obs table to GSI_GridComp.rc "
            exit(1)
       endif
  endif

# Run ANALYZER in observer setting mode
# -------------------------------------
  set gsinlat = `echorc.x -rc GSI_GridComp.rc.tmpl "GSI JM"`
  set gsinlon = `echorc.x -rc GSI_GridComp.rc.tmpl "GSI IM"`
  set gsinlev = `echorc.x -rc GSI_GridComp.rc.tmpl "GSI LM"`
  analyzer $nymd1 $nhms1 $STF $SKA $SSB -expid $EXPID -t $SPECRES -levs $gsinlev -x $gsinlon -y $gsinlat -observer -lnobs -log obs.log
  if( $status) then
     echo " ${myname}: trouble use analyzer/observer"
     exit(1)
  endif
  

# All done: echo files available at working diretory
# --------------------------------------------------
  if(-e $expid.ana.eta.${nymd1}_${hh1}z.$NCSUFFIX ) /bin/rm $expid.ana.eta.${nymd1}_${hh1}z.$NCSUFFIX
  echo ""
  /bin/ls -la
  echo ""


exit(0)

#...........................................................................

usage:

cat <<EOF

\begin{verbatim}

NAME
     setobsvr - Set up observer (usually for running analysis)

SYNOPSIS

     setobsvr [-h] [-log] [-cprs]  fvhome  fvwork

DESCRIPTION

     This script prepares observation files so observer can run.
     On input:

     fvhome     experiment home directory, e.g., /scratch1/\$user/v000_b55
     fvwork     working directory, e.g., \$TMPDIR

     Restarts, namelists and resource files are  expected to be available
     in \$FVHOME/recycle and \$FVHOME/run.  The run is performed in \$FVWORK,
     and the output files are left there for archival by the calling script.

OPTIONS

     -h           prints this page
     -strict      returns with non-zero error if acquire fails to
                  resolve all input files
     -obsclass    cls1,cls2...
                  observation data classes, such as conv_tovs,ssmi_wentz_tpw
     -log         log warning and errors to file

ENVIRONMENT VARIABLES

  FVHOME      experiment home diretory
  ATMENSETC   location of ensemble RC files (e.g., FVHOME/run/atmens)
  SPECRES     resolution of spectral backgrounds (254 for T254, 62 for T62, etc)
  VAROFFSET   time offset (abs value) between analysis time and initial time of ana window
  TIMEINC     analysis time window (6-hr for 3dvar; 6,12,18,24,etc for 4dvar)


ENVIRONMENT VARIABLES (optional)

  STRICT      when set, will crash if obs files missing

SEE ALSO

    fvssisetup    Experiment setup utility
    fvssi/gsi.j   Main experiment script created by fvssisetup.

AUTHOR
    Based on fvssi initially coded by Carlos Cruz
    Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO 
    Last modified: 20Jun2020      by: R. Todling

\end{verbatim}
\clearpage
EOF

exit 1

