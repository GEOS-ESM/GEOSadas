#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensArchive"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global atmens_archive atmens_ana"
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
hhb=$(echo $nhmsb | cut -c1-2)

atmens_arch.csh \
    $EXPID $nymdb $nhmsb $FVHOME/run/atmens/atmens_storage.arc \
    eadas atmens4arch.${nymdb}_${hhb} >> atm_ens_arch.${nymdb}_${hhb}z.log 2>&1
if [ $? -ne 0 ]; then
    echo "atmens archival failed"
    exit 1
fi

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
