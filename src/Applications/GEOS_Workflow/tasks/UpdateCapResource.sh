#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="UpdateCapResource"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
TIMEFILE=$FVWORK/simtimes.yaml
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Final_=1

gcm_nymd0=$(grep "^gcm_nymd0:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
gcm_nhr0=$(grep "^gcm_nhms0:" $TIMEFILE | cut -d":" -f2 | tr -d " " | cut -c1-2)

# Which cap to use
# if [ $Final_ -ne 0 ]; then
#     mycap=CAP.rc.tmpl
#     if [ -e CAP_${gcm_nhr0}.rc.tmpl ]; then
# 	mycap=CAP_${gcm_nhr0}.rc.tmpl
#     fi
# else
#     mycap=CAP.rc.tmpl
# fi
mycap=CAP.rc.tmpl
if [ -e CAP_${gcm_nhr0}.rc.tmpl ]; then
    mycap=CAP_${gcm_nhr0}.rc.tmpl
fi

# Define CAP to run GCM for proper length of time
# if [ $SHORTFCST -ne 0 ] && [ $Final_ -eq 0 ]; then
#     job_sgmt=$(grep -i JOB_SGMT $mycap)
#     /bin/rm -f sed_file CAP.rc
#     echo "s/$job_sgmt/JOB_SGMT: 0 030000/1" > sed_file
#     sed -f sed_file  ./$mycap               > ./CAP.rc

#     # Don't need any model output during short-time forecast
#     /bin/rm -f sed_file
#     echo "EXPID:  $EXPID "   > sed_file
#     echo "EXPDSC: $EXPID "  >> sed_file
#     /bin/mv sed_file ./HISTORY.rc
# else
#     /bin/cp $mycap CAP.rc
# fi
/bin/cp $mycap CAP.rc

# initial time
itag=${gcm_nymd0}_${gcm_nhr0}z

# QUESTION: Should this go in another script??
/bin/rm -f ExtData.rc
cd $FVHOME/run/gocart
extdata_files=$(/bin/ls -1 *_ExtData.rc)
cat $extdata_files > $FVWORK/ExtData.rc
cd -
# /bin/cp ExtData.rc $EXPID.ExtData.$itag.rc

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
