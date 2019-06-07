#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CopyGcmRestarts4DAS"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
FVREC=$FVHOME/recycle
TIMEFILE=$FVWORK/simtimes.yaml
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# initial time
TIMEFILE=$FVWORK/simtimes.yaml
gcm_nymd0=$(grep "^gcm_nymd0:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
gcm_nhr0=$(grep "^gcm_nhms0:" $TIMEFILE | cut -d":" -f2 | tr -d " " | cut -c1-2)
itime=${gcm_nymd0}_${gcm_nhr0}z

# restarts
source $ThisScriptDir/CopyGcmRestarts.sh
CopyGcmRestarts_ $FVREC $itime

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
