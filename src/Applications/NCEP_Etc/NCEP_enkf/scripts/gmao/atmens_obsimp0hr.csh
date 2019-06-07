#!/bin/csh

# atmens_obsimp0hr.csh - calculates obs impact on ensemble ana
#
# !REVISION HISTORY:
#
#  15Apr2013  Todling   Initial script
#  18Jun2018  Todling   Summarize 0hr impact
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 1
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars
setenv MYNAME atmens_obsimp0hr.csh

# The following env variable allows generation of a fake ensemble
# ---------------------------------------------------------------
if ( !($?SIMULATE_ENSEMBLE) ) setenv SIMULATE_ENSEMBLE 0

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - calculate obs impact on analysis"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms anadir "
   echo " "
   echo " where"
   echo "   expid     -  usual experiment name, e.g., b541iau"
   echo "   nymd      -  date of analysis, as in YYYYMMDD"
   echo "   nhms      -  time of analysis, as in HHMMSS"
   echo " "
   echo " DESCRIPTION"
   echo "    This procedures calculates observation impacts on the"
   echo "  analysis (i.e, impact on the 0-hour forecast). First the"
   echo "  OMBs are converted from diag to ODS; then the OMAs are "
   echo "  are converted to ODS; and finally the impacts are produced"
   echo "  and saved in a set of ODS files that are then available for"
   echo "  archiving."
   echo "    The presence of the file odsmatch.rc under ATMENSETC "
   echo "  serves as a trigger of this procedure."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 "
   echo " "
   echo " REQUIRED RESOURCE FILES "
   echo " "
   echo "   odsmatch.rc  - required by odsselect (diag2ods)"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES "
   echo " "
   echo "    ATMENSETC       - location of resource files        "
   echo "    FVHOME          - location of experiment            "
   echo "    FVROOT          - location of DAS build             "
   echo "    FVWORK          - location of work directory        "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES "
   echo " "
   echo "    NCSUFFIX         - suffix of hdf/netcdf files (default: nc4)"
   echo "    ENSPARALLEL      - when set, runs all ensemble components in parallel "
   echo "                       (default: off)"
   echo "    AENS_OBSVR_DSTJOB- distribute multiple works within smaller jobs"
   echo "    OBSVR_WALLCLOCK  - wall clock time to run observer, default 1:00:00 "
   echo "    OBSVR_QNAME      - name of queue (default: NULL, that is, let pbs pick) "
   echo " "
   echo " SEE ALSO"
   echo " "
   echo "   diag2ods  - converts GSI diag files to ODS"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 15Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif
 
setenv FAILED 0
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1

if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4
if ( !($?ENSPARALLEL)   ) setenv ENSPARALLEL 0
if ( !($?OBSVR_WALLCLOCK))setenv OBSVR_WALLCLOCK 1:00:00
if ( !($?OBSVR_QNAME))    setenv OBSVR_QNAME NULL

if ( !($?ATMENS_0HRIMP_NCPUS) ) then
else
   setenv NCPUS $ATMENS_0HRIMP_NCPUS
endif

if ( !($?DO_0HR_IMP))     setenv DO_0HR_IMP 1

if ( $ENSPARALLEL ) then
   if ( !($?ENSGSI_NCPUS) ) then
     setenv FAILED 1 
   else
     setenv JOBGEN_NCPUS $ENSGSI_NCPUS
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
set hha      = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hha}
set timetag    = ${nymd}_${hha}z

setenv ENSWORK $FVWORK
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

if ( ! -e $ATMENSETC/odsmatch.rc ) then 
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
   echo "${MYNAME}: nothing to do"
   exit(0)
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

# Check to see if this need/can be called 
set do_oimp = 0
if((-e $ATMENSETC/obs1gsi_member.rc) && -e $ATMENSETC/GSI_GridComp_ensfinal.rc.tmpl ) set do_oimp = 1
if ( $do_oimp == 0 ) then
   touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
   echo "${MYNAME}: nothing to do"
   exit(0)
endif

# Create internal work directory
# ------------------------------
mkdir -p $ENSWORK/obsimp0hr
cd $ENSWORK/obsimp0hr

# Check to make sure observer is really needed
# --------------------------------------------
if ( -d $ENSWORK/ensmean ) then
    ln -sf $ENSWORK/ensmean/diag*ges*_ensmean .
    foreach fn ( `ls diag*ges*ensmean` )
       set prefix = `echo $fn | cut -d. -f1`
       ln -sf $fn $expid.$prefix.$timetag.bin # proper link for diag2ods to work
    end
else
   echo "${MYNAME}: trouble, cannot find diag-guess ens-mean files"
   exit(1)
endif
if ( -d $ENSWORK/ensfinal ) then
    ln -s $ENSWORK/ensfinal/diag*anl*_ensfinal .
    foreach fn ( `ls diag*anl*ensfinal` )
       set prefix = `echo $fn | cut -d. -f1`
       ln -sf $fn $expid.$prefix.$timetag.bin # proper link for diag2ods to work
    end
else
   echo "${MYNAME}: trouble, cannot find diag-guess ens-mean files"
   exit(1)
endif

# Convert OMB/OMA from diag to ODS files, and calculate impacts
# -------------------------------------------------------------
/bin/rm *.ods # not so efficient, but must prevent trying to overwrite ODS files in case of re-submission
if ( ($?NCPUS) ) then
  @ d2o_nproc = $NCPUS / 4
  if ($d2o_nproc < 1 ) set d2o_nproc = 1
else
 set d2o_nproc = 8
endif
diag2ods -rc $ATMENSETC/odsmatch.rc -reduce_diag -ncpus $d2o_nproc -log $expid.ensods.log.${nymd}_${hha}z.txt $nymd $nhms $expid
if ( $status ) then
   echo "${MYNAME}: trouble converting diag files to ODS, aborting ..."
   exit(1)
endif

# If all successfully completed, transfer ods files of interest to updated_ens dir
# --------------------------------------------------------------------------------
mkdir -p $ENSWORK/updated_ens/obsimp0hr
/bin/mv *imp0hr*ods $ENSWORK/updated_ens/obsimp0hr

# If so, summarize observation impact at 0-hr
# -------------------------------------------
if( -e $ATMENSETC/odsstats_ktonly.rc ) then
  cd $ENSWORK/updated_ens/obsimp0hr
  obimp_summary.pl -o obimp_summary.txt -rc $ATMENSETC/odsstats_ktonly.rc ${nymd} ${hha}0000
  cat obimp_summary.txt
  cd -
endif

# if made it here, then exit nicely
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
