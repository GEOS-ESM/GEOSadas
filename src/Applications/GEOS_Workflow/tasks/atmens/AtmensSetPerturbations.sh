#!/usr/bin/env bash

# In case of additive inflation, prepare the perturbations

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensSetPerturbations"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global atmens_ana atmens_setperts atmens_recenter"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
LOGFILE=$FVWORK/setperts.log

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ $AENS_ADDINFLATION ] && [ -f $ATMENSETC/nmcperts.rc ]; then

    # date/time
    anymd=$(grep "^anymd:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
    anhms=$(grep "^anhms:" $TIMEFILE | cut -d":" -f2 | tr -d " ")

    # number of ens members
    nmems=$(ls -1d $HYBRIDGSI/mem* | wc -l)

    # run setperts.csh
    setperts.csh ${EXPID} $nmems $anymd $anhms $TIMEINC $AENSADDINFLOC >> $LOGFILE 2>&1
    if [ $? -ne 0 ]; then
        echo "Failed in setperts.csh, aborting."
        exit 1
    fi

else

    echo "setperts.csh was not run"

fi

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
