#!/usr/bin/env bash

# Run vortex track on ensemble members

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensVortexTrack"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global atmens_vtrack atmens_ana" # atmens_ana for VAROFFSET
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
TIMEFILE=$FVWORK/simtimes_atmens.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# date/time
nymdb=$(grep "^nymdb:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep "^nhmsb:" $TIMEFILE | cut -d":" -f2 | tr -d " ")

if [ $nhmsb == "210000" ] || [ $nhmsb == "090000" ]; then # temporarily wired
    atmens_vtrack.csh $EXPID $nymdb $nhmsb >> ${ATMENSLOG} 2>&1
    if [ $? -ne 0 ]; then
        echo "vtrack ensemble (atmens_vtrack.csh) failed"
        exit 1
    fi
fi

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
