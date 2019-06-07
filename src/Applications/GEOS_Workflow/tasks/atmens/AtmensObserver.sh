#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensObserver"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global from_central atmens_ana atmens_recenter atmens_obsvr"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# date/time
anymd=$(grep "^anymd:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
anhms=$(grep "^anhms:" $TIMEFILE | cut -d":" -f2 | tr -d " ")

# Run obsvr_ensemble.csh
obsvr_ensemble.csh $OBSCLASS $EXPID $anymd $anhms >> $ATMENSLOG 2>&1
if [ $? -ne 0 ]; then
    echo "obsvr_ensemble.csh failed"
    exit 1
fi

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
