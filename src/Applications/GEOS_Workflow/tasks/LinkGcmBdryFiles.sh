#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="LinkBdryCondns"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
FVRUN="$FVHOME/run"
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

mydate=($(rst_date ./d_rst))
GcmBegDate=${mydate[0]}

$FVRUN/lnbcs $GcmBegDate
if [ $? -ne 0 ]; then
    exit 1
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
