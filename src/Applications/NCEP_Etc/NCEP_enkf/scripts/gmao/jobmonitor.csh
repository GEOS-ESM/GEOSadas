#!/bin/csh 

# jobmonitory.csh - simple script to monitor jobs running during 
#                   ensemble application
#
# !REVISION HISTORY:
#
#  04Nov2011  Todling   Initial script
#  05Feb2012  Todling   More specific about process name
#  14Sep2012  Todling   Add failure check to prevent job running
#                       end of requested time
#------------------------------------------------------------------

setenv MYNAME jobmonitor.csh

if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - monitor events of a parallel section"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  nevents calledby workdir yyyymmddhh"
   echo " "
   echo " where"
   echo "   nevents    -  number of parallel events (i.e., no. of ensemble members)"
   echo "   calledby   -  name of calling program"
   echo "   workdir    -  location where work takes place"
   echo "   yyyymmddhh -  date/hour of events"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "    This procedure monitors the completion of parallel procedures running "
   echo "  the within atmopheric ensemble DAS."
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "   JOBMONITOR_DELSLEEP_SEC - specify time interval to check job termination"
   echo "                             (seconds); default: 20 sec"
   echo "   JOBMONITOR_MAXSLEEP_MIN - specify max allowed time per procedure (minutes);"
   echo "                             procedure means e.g. all of observer calls"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

 set nmem         = $1
 set calledby     = $2
 set workdir      = $3
 set yyyymmddhh   = $4

 if ( !($?JOBMONITOR_MAXSLEEP_MIN) ) setenv JOBMONITOR_MAXSLEEP_MIN 0
 if ( !($?JOBMONITOR_DELSLEEP_SEC) ) setenv JOBMONITOR_DELSLEEP_SEC 20

 set sleep_step_sec = $JOBMONITOR_DELSLEEP_SEC
   @ sleep_max_sec  = 60 * $JOBMONITOR_MAXSLEEP_MIN

   /bin/rm .FOUND_${calledby}*
   @ ns = $nmem    # total number of jobs submitted (same as members)
   @ delsleep = 0
   echo " ${calledby}: Start monitoring job completion ... "
   while ( $ns > 0 ) # do check until all jobs are finished
     sleep $sleep_step_sec
     @ delsleep = $delsleep + $sleep_step_sec
     set n = 0
     while ( $n < $nmem )
       @ n = $n + 1
       set nnn = `echo $n | awk '{printf "%03d", $1}'`
       if( -e $workdir/.FAILED ) then
         echo " ${MYNAME}: found FAILED process (${nnn}_${calledby}.$yyyymmddhh), aborting"
         exit(1)
       endif
       if( -e $workdir/.DONE_MEM${nnn}_${calledby}.$yyyymmddhh && (! -e .FOUND_${calledby}${nnn}) ) then
         @ ns--  # at least one job completed
         touch .FOUND_${calledby}${nnn}
       endif
     end
     echo " ${calledby}: $ns jobs still waiting for DONE_MEM${nnn}_${calledby}.$yyyymmddhh after $delsleep sec "
     if ( $JOBMONITOR_MAXSLEEP_MIN > 0 ) then
        if ( $delsleep > $sleep_max_sec ) then
           echo " ${calledby}: max allowed time per procedure violated "
           echo " ${calledby}: looks like this is a run away job, abort monitoring ..."
           exit(1)
        endif
     endif
   end
   echo " ${calledby}: Done monitoring job completion. "
   /bin/rm .FOUND_${calledby}*
   # sleep a little to allow for disc syncronization (go figure!)
   sleep $sleep_step_sec
   exit(0)
