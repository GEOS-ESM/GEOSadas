#!/usr/bin/env bash

# Run ensemble of atmospheric GCMs

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensGcm"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global from_central atmens_ana atmens_gcm"
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
tfcst_hh=$((2*$TIMEINC/60))

# nlon/nlat
ens_mres=$(getgfiodim.x $FVHOME/atmens/ensmean/$EXPID.bkg.eta.${nymdb}_${hhb}z.$NCSUFFIX)
read ens_nlons ens_nlats rest <<< $ens_mres

# Run gcm_ensemble.csh
gcm_ensemble.csh $EXPID $nymdb $nhmsb $tfcst_hh $ens_nlons $ens_nlats >> ${ATMENSLOG} 2>&1
if [ $? -ne 0 ]; then
    echo "gcm_ensemble failed"
    exit 1
fi

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
