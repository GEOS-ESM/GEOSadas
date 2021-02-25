#!/bin/csh 

# edas_scheduler - controls ensemble DAS processes
#
# !REMARKS: 
#    1. jobmonitor calls are like barriers in MPI: they appear
#       whenever syncronization is required.
#
# !REVISION HISTORY:
#
#  05Feb2012  Todling   Initial script
#  18Jan2015  Todling   Revised and tested
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME edas_scheduler.csh

setenv FAILED 0
if ( !($?DDASJNAME)     ) setenv FAILED 1
if ( !($?EXPID)         ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVWDIR)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?VAROFFSET)     ) setenv FAILED 1

# need usage here
# ---------------
if ( $FAILED ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - scheduler for running EnADAS in parallel "
   echo "             with central ADAS"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME "
   echo " "
   echo " DESCRIPTION " 
   echo " "
   echo "  This allows launching the ensemble analysis as soon as the "
   echo "  central analysis is available. The aim of the scheduler is "
   echo "  to expedite the cycle and maximize efficiency when running "
   echo "  the hybrid-variational DAS."
   echo " "
   echo "  The trigger for this mode of execution is the presence of " 
   echo "  the file edas_scheduler.j in the run directory of the "
   echo "  experiment and the environment setting"
   echo "      setenv ENSPARALLEL 2 "
   echo "  in the AtmEnsConfig.csh configuration script."
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC - location of ensemble resource files"
   echo "    DDASJNAME - name of script controling central DAS (e.g., g5das.j)" 
   echo "    EXPID     - experiment name"
   echo "    FVHOME    - location of experiment            "
   echo "    FVROOT    - location of DAS build             "
   echo "    FVWDIR    - root location of fvwork/enswork   "
   echo "    VAROFFSET - offset time from first synoptic time (min)"
   echo " " 
   echo " REMARKS"
   echo " " 
   echo "  1. Only very limited testing has been done with this mode of" 
   echo "     execution. There are still known issues related to the "
   echo "     scheduling, thus this is not the presently recommended mode"
   echo "     of running."
   echo " "
   echo " SEE ALSO "
   echo " "
   echo "  atm_ens.j        - job script, driver of the Ensemble Analysis" 
   echo "  edas_scheduler.j - job script, driver of the Scheduler" 
   echo " " 
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 18Jan2015      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "

   exit 1
endif

if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if (! -e $FVHOME/run/$DDASJNAME ) setenv FAILED 1
if (! -e $FVHOME/run/atm_ens.j  ) setenv FAILED 1
if ( $FAILED ) then
   echo "{$MYNAME}: failed, aborting ..."
   exit 2
endif

# Get current date/time
# ---------------------
cd $FVHOME/recycle
set nymd = `ls $EXPID.rst.lcv.????????_??z.bin | cut -d. -f4 | cut -c1-8`
set hh   = `ls $EXPID.rst.lcv.????????_??z.bin | cut -d. -f4 | cut -c10-11`
set yyyymmddhh = ${nymd}${hh}

# calculate analysis date
@ offset_sec = $VAROFFSET * 60
set bdate = ( $nymd ${hh}0000 )
set adate = ( `tick $bdate $offset_sec` )
set anymd = $adate[1]
set ahh   = `echo $adate[2] | cut -c1-2`

# The follow will start all processes from scratch
# ------------------------------------------------
if (-e $FVHOME/.REFRESH.$yyyymmddhh ) then
   /bin/rm $FVHOME/.DONE_MEM001_ddas.${yyyymmddhh}
   /bin/rm $FVHOME/.DONE_MEM001_atm_ens.${yyyymmddhh}
   /bin/rm $FVHOME/.DONE_MEM001_atm_ens_eana.${yyyymmddhh}
   /bin/rm $FVHOME/.DONE_MEM001_analyzer.${anymd}${ahh}
   /bin/rm $FVHOME/.DONE_MEM001_rstcp.${yyyymmddhh}
   /bin/rm $FVHOME/.REFRESH.$yyyymmddhh
endif

set ddas_work = $FVWDIR/fvwork_${EXPID}_${yyyymmddhh}
set aens_work = $FVWDIR/enswork_${EXPID}_${yyyymmddhh}

cd $FVHOME/run

# Launch one segment of deterministic (hybrid) central DAS
# --------------------------------------------------------
if (! -e $FVHOME/.DONE_MEM001_ddas.${yyyymmddhh} ) then
  if ( $ATMENS_BATCHSUB == "sbatch") then
     $ATMENS_BATCHSUB --export=kidwork=$ddas_work $DDASJNAME
  else
     $ATMENS_BATCHSUB -v kidwork=$ddas_work $DDASJNAME
  endif

# wait until RSTs have been copied to proper location
# ---------------------------------------------------
  jobmonitor.csh 1 rstcp $FVHOME ${yyyymmddhh}
  if ($status) then
     echo "${MYNAME}: failed in jobmonitor rstcp, aborting"
     exit(1)
  endif
endif

# Launch ensemble observer and ensemble analysis (EnKF, EnGSI, etc)
# ----------------------------------------------
if (! -e $FVHOME/.DONE_MEM001_atm_ens_eana.${yyyymmddhh} ) then
  if ( $ATMENS_BATCHSUB == "sbatch") then
     $ATMENS_BATCHSUB --export=kidwork=$aens_work,eanaonly=1 atm_ens.j
  else
     $ATMENS_BATCHSUB -v kidwork=$aens_work,eanaonly=1 atm_ens.j
  endif
endif

# Syncronize: monitor completion of central and ensemble analyses
# ---------------------------------------------------------------
if (! -e $FVHOME/.DONE_MEM001_analyzer.${anymd}${ahh} ) then
  jobmonitor.csh 1 analyzer     $FVHOME ${anymd}${ahh}
endif
if (! -e $FVHOME/.DONE_MEM001_atm_ens_eana.${yyyymmddhh} ) then
  jobmonitor.csh 1 atm_ens_eana $FVHOME ${yyyymmddhh}
endif
if ($status) then
   echo "${MYNAME}: failed in jobmonitor analyzer/atm_ens_eana, aborting"
   exit(1)
endif

# When analysis is complete, launch ensemble of backgrounds
# ---------------------------------------------------------
if ( -e $FVHOME/.DONE_MEM001_atm_ens_eana.${yyyymmddhh} &&  -e $FVHOME/.DONE_MEM001_analyzer.${anymd}${ahh} ) then
  if ( $ATMENS_BATCHSUB == "sbatch") then
     $ATMENS_BATCHSUB --export=kidwork=$aens_work atm_ens.j
  else
     $ATMENS_BATCHSUB -v kidwork=$aens_work atm_ens.j
  endif
#  ... and monitor its completion
#  ------------------------------
   jobmonitor.csh 1 atm_ens $FVHOME $yyyymmddhh
endif

# ... and completion of central forecast
# --------------------------------------
jobmonitor.csh 1 ddas $FVHOME $yyyymmddhh

# when getting here, all processes are complete ...
# clean up - this under test: cannot really clean up
#            otherwise cannot bypass completed processes
#            in case of re-run due to partial crashes.
set clean_up = 0
if ( $clean_up ) then
  if ( -e $FVHOME/.DONE_MEM001_rstcp.${yyyymmddhh} ) then
     /bin/rm $FVHOME/.DONE_MEM001_rstcp.${yyyymmddhh}
  else
     echo "${MYNAME}: it seems rst were not copied successfully, aborting ..."
     exit 1 
  endif
  if ( -e $FVHOME/.DONE_MEM001_analyzer.${anymd}${ahh} ) then
     /bin/rm $FVHOME/.DONE_MEM001_analyzer.${anymd}${ahh}
  else
     echo "${MYNAME}: it seems central analysis did not complete successfully, aborting ..."
     exit 1 
  endif
  if ( -e $FVHOME/.DONE_MEM001_atm_ens_eana.${yyyymmddhh} ) then
     /bin/rm $FVHOME/.DONE_MEM001_atm_ens_eana.${yyyymmddhh}
  else
     echo "${MYNAME}: it seems ensemble analysis did not complete successfully, aborting ..."
     exit 1 
  endif
  if ( -e $FVHOME/.DONE_MEM001_atm_ens.${yyyymmddhh} ) then
     /bin/rm $FVHOME/.DONE_MEM001_atm_ens.${yyyymmddhh}
  else
     echo "${MYNAME}: it seems ensemble bkg/fcst did not complete successfully, aborting ..."
     exit 1 
  endif
  if ( -e $FVHOME/.DONE_MEM001_ddas.${yyyymmddhh} ) then
     /bin/rm $FVHOME/.DONE_MEM001_ddas.${yyyymmddhh}
  else
     echo "${MYNAME}: it seems DAS did not complete successfully, aborting ..."
     exit 1 
  endif
endif # total clean-up

# from here on ... main job can submit next cycle
exit(0)
