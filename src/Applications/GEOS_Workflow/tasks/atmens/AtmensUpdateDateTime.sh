#!/usr/bin/env bash

# Run ensemble of atmospheric GCMs

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensUpdateDateTime"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global atmens_ana"
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

# Update date/time
mkdir -p $RSTSTAGE4AENS
mkdir -p $STAGE4HYBGSI
tsec=$(( $TIMEINC*60 ))
old_rst_lcv=${EXPID}.rst.lcv.${nymdb}_${hhb}z.bin
if [ -e $RSTSTAGE4AENS/$old_rst_lcv ]; then
    echo "Old rst.lcv [$old_rst_lcv] still exists! Run AtmensTagAndRecycle first."
    exit 1
fi
next_seg=($(tick $nymdb $nhmsb $tsec))
mkdrstdate.x ${next_seg[@]}
new_rst_lcv=${EXPID}.rst.lcv.${next_seg[0]}_${next_seg[1]}z.bin
\mv d_rst $RSTSTAGE4AENS/$new_rst_lcv

# All done
cd $FVWORK
zeit_co.x $myname
cd $CWD
