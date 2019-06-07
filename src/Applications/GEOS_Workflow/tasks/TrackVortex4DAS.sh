#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="RunVortexTracker"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common vtx"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# shorthands
CWD=$(pwd)
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

nymdb=$(grep ^nymdb: $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep ^nhmsb: $TIMEFILE | cut -d":" -f2 | tr -d " ")

# pc: Hardcoded value??
vtrtime=($(tick $nymdb $nhmsb 21600)) # need to test this w/o IAU
nymdt=${vtrtime[0]}
nhmst=${vtrtime[1]}

# Check tcvitals for valid entries
# (assumes tcvitals.yyyymmddhh naming convention)
for vitals in $(ls *tcvitals*); do
    echo "Checking $vitals for valid entries."
    #cat $vitals
    tcvdate=$(echo $vitals | cut -d . -f2 | cut -c 1-8)
    tcvtime=$(echo $vitals | cut -d . -f2 | cut -c 9-10)00
    /bin/rm -f ${vitals}.tmp
    /bin/mv $vitals ${vitals}.tmp
    # pc: ignore error in case of empty file
    cat ${vitals}.tmp | grep "$tcvdate $tcvtime" > $vitals || true
    echo "Corrected ${vitals}:"
    #cat  $vitals
    /bin/rm -f ${vitals}.tmp
done

# First run vortex tracking code ...
if [ $VTRACK -ne 0 ]; then
    vtrack -freq $VTRKFRQA -rc $FVWORK/vtrack.rc $nymdt $nhmst $EXPID
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
