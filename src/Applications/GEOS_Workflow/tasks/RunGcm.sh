#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="RunGcm"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common gcm gcm_mpi mpi"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Save desired restart files to use for forecasting purposes
# TODO

# Running intermittent analysis update
# TODO

# Launch zipping job
# TODO

# TODO: CENTRAL_AGCM_PARALLEL

# Now, run GCM
use_shmem=$(echorc.x -rc CAP.rc USE_SHMEM)
if [ $use_shmem -ne 0 ]; then
    RmShmKeys_sshmpi.csh
fi

/bin/rm -f ./EGRESS
$MPIRUN_GCM >> fvpsas.log

if [ $use_shmem -ne 0 ]; then
    RmShmKeys_sshmpi.csh
fi

if [ ! -e ./EGRESS ]; then
    echo "GCM failed!"
    exit 1
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
