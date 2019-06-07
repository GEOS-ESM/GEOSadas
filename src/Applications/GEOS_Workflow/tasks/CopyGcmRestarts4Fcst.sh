#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CopyGcmRestarts4Fcst"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
FCSTSTG=$FVHOME/fcst/stage
TIMEFILE=$FVWORK/simtimes.yaml
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# initial time
# TIME0 is an ecflow variable that is exported by the ecf script

# restarts
source $ThisScriptDir/CopyGcmRestarts.sh
CopyGcmRestarts_ $FVHOME/fcst/stage $TIME0

# TODO: Check if this code block is needed anymore
TIME0_hhmm=$(echo $TIME0 | sed 's/z/00z/')
if [ -e $FCSTSTG/$EXPID.traj_lcv_rst.$TIME0_hhmm.$NCSUFFIX ]; then
    /bin/ln -s $FCSTSTG/$EXPID.traj_lcv_rst.$TIME0_hhmm.$NCSUFFIX $FVWORK/$EXPID.traj.lcv.$TIME0_hhmm.$NCSUFFIX
fi
# if ( -e $fcstage/stage/$EXPID.ptrj_prs_rst.$TIME0_hhmm.$NCSUFFIX ) then
#     /bin/ln -s $fcstage/stage/$EXPID.ptrj_prs_rst.$TIME0_hhmm.$NCSUFFIX ./$EXPID.ptrj.prs.$TIME0_hhmm.$NCSUFFIX
# endif

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
