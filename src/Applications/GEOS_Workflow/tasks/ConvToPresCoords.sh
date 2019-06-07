#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="ConvToPresCoords"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common conv2prs"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ $CONVUPA -ne 0 ]; then
    $FVROOT/bin/cnv2prs.pl -etaana -etabkg -etaasm -sfc -diag
else
    echo "CONVUPA=0 => Not running cnv2prs.pl"
fi

if [ $CONVSFC -ne 0 ]; then
    $FVROOT/bin/cnv2prs.pl -dsfc
else
    echo "CONVSFC=0 => Not running cnv2prs.pl"
fi

# This is used when extending bkg integration to mid-range forecasts
if [ $CONVPROG -ne 0 ]; then
    $FVROOT/bin/cnv2prs.pl -prog
else
    echo "CONVPROG=0 => Not running cnv2prs.pl"
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
