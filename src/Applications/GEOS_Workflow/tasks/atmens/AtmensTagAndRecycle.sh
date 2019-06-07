#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensTagAndRecycle"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global"
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

# Store updated ensemble for archiving
StoreLocation=$ATMENSLOC/atmens4arch.${nymdb}_${hhb}
\mv $ATMENSLOC/atmens $StoreLocation
\cp .zeit $StoreLocation/$EXPID.atmens_zeit.log.${nymdb}_${hhb}z.txt

# Prepare for next cycle
\mv $FVWORK/updated_ens $ATMENSLOC/atmens

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
