#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="TagAndRecycle4Fcst"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Date times
nymd0=$(grep "^fcst_beg_nymd:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhms0=$(grep "^fcst_beg_nhms:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhr0=$(printf %02d $((${nhms0#0}/10000)))
nymd2=$(grep "^fcst_end_nymd:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhms2=$(grep "^fcst_end_nhms:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhr2=$(printf %02d $((${nhms2#0}/10000)))

# Timestamp for tagging
# TIME0 is an ecflow variable, exported by TagAndRecycle4Fcst.ecf
rtags=$TIME0+${nymd0}_${nhr0}z-${nymd2}_${nhr2}z

# Tag log giles
/bin/mv -f fvpsas.log $EXPID.gcm.log.$rtags.txt
/bin/mv -f trak.log $EXPID.trak.log.$rtags.txt || true # do not exit on error

# TODO: mkresrst

# All done
cd $FVWORK
zeit_co.x $myname

# Tag zeit file
/bin/mv -f .zeit $EXPID.zeit.reg.$rtags.txt

# Back to orig location
cd $CWD
