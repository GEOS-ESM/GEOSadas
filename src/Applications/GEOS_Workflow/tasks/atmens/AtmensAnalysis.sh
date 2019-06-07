#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensAnalysis"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global atmens_ana atmens_enkf"
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

# Run ensemble of atmospheric analyses
atmos_eana.csh $EXPID $anymd $anhms >> $ATMENSLOG 2>&1
if [ $? -ne 0 ]; then
    echo "atmos_eana.csh failed"
    exit 1
fi

# Save EnKF analyses before recentering and inflation
nymdb=$(grep "^nymdb:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep "^nhmsb:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
hhb=$(echo $nhmsb | cut -c1-2)
cd $FVWORK/updated_ens
if [ ! -e $HYBRIDGSI/${EXPID}.atmens_eana_brec.${nymdb}_${hhb}z.tar ]; then
    tar -cvf $HYBRIDGSI/${EXPID}.atmens_eana_brec.${nymdb}_${hhb}z.tar mem0*/*.ana.eta*nc4
fi

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
