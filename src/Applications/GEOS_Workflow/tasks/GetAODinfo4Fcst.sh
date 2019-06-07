#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="GetAODinfo4Fcst"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
FVSPOOL=$FVHOME/spool

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ ! -e aod4fcst.acq ]; then
    echo "aod4fcst.acq does not exist! Exiting..."
    exit 0
fi

rstnow=($(rst_date d_rst))
initref=($(tick ${dtg[@]} 10800)) # tick 3 hrs ahead of rst time

acquire -v -rc aod4fcst.acq -s $FVSPOOL -d . -strict ${initref[0]} ${initref[1]} 030000 2

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
