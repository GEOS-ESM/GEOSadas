#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="GetDateTime4DAS"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana obs"
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

# Initial time
# -------
# TIME0 is an ecflow suite variable that has
# been exported by GetDateTime4Fcst.ecf
# This should be the only place where ecflow
# and GEOS workflow scripts are tighly coupled
# -------
source $DateTimeDir/InitialDateTime.sh
GetInitialDateTime_ $FVHOME/recycle $FVWORK $TIME0 # copies d_rst

# Write timefile simtimes.yaml
source $DateTimeDir/WriteTimefile.sh
WriteTimefile4DAS_ $TIMEFILE $FVWORK $FVROOT $TIMEINC $VAROFFSET $OBSWINDOW

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
