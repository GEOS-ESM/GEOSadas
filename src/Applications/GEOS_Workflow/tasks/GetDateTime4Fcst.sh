#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="GetDateTime4Fcst"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common gcm iau"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
DateTimeDir=$ThisScriptDir/utils/datetime
TIMEFILE=$FVWORK/simtimes.yaml
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Initial time (TIME0 is an ecflow variable)
source $DateTimeDir/InitialDateTime.sh
GetInitialDateTime_ $FVHOME/fcst/stage $FVWORK $TIME0 # returns itime

# Write timefile simtimes.yaml
source $DateTimeDir/WriteTimefile.sh
WriteTimefile4Fcst_ $TIMEFILE $FVWORK $FVROOT $FVHOME $ifcst

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
