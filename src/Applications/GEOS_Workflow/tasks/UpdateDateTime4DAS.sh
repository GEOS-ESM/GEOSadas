#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="UpdateDateTime4DAS"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana obs"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location
TIMEFILE=$FVWORK/simtimes.yaml
TIMEFILE_PRV=$FVWORK/simtimes.prv.yaml
DateTimeDir=$ThisScriptDir/utils/datetime

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Update d_rst
wrt_rst_nymd=$(grep ^wrt_rst_nymd: $TIMEFILE | cut -d":" -f2 | tr -d " ")
wrt_rst_nhms=$(grep ^wrt_rst_nhms: $TIMEFILE | cut -d":" -f2 | tr -d " ")
/bin/mv -f $FVWORK/d_rst $FVWORK/d_rst_prv
$FVROOT/bin/mkdrstdate.x $wrt_rst_nymd $wrt_rst_nhms

# Update cap_restart
mycaprst=$FVWORK/cap_restart
/bin/rm -f $mycaprst
$FVROOT/bin/rst_date $FVWORK/d_rst > $mycaprst

# Save simtimes.yaml as simtimes.prv.yaml
# Write segment/restart time to simtimes.yaml
/bin/mv -f  $TIMEFILE $TIMEFILE_PRV
source $DateTimeDir/WriteTimefile.sh
WriteTimefile4DAS_ $TIMEFILE $FVWORK $FVROOT $TIMEINC $VAROFFSET $OBSWINDOW

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
