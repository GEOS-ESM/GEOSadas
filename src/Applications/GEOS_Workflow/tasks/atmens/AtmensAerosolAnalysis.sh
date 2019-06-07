#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensAerosolAnalysis"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global from_central atmens_ana atmens_aod"
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

# Run atmos_eaod.csh
atmos_eaod.csh $EXPID $anymd $anhms 030000 2 >> $ATMENSLOG 2>&1
if [ $? -ne 0 ]; then
    echo "atmos_eaod.csh failed"
    exit 1
fi

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
