#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AtmensGetDateTime"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="atmens_global atmens_ana"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Get itime (yymmdd_hhz) from rst_lcv
if [ ! -d "$RSTSTAGE4AENS" ]; then exit 1; fi
rst_lcv=$RSTSTAGE4AENS/$EXPID.rst.lcv.????????_??z.bin
itime=$(basename ${rst_lcv} | awk -F'.' '{print $4}')

# Sanity check
# ------------
# TIME0 is an ecflow suite variable - this should be the only
# place where ecflow and GEOS workflow scripts are tightly coupled
# ------------
if [ "$itime" != "$TIME0" ]; then
    echo "TIME0 from ecflow is $TIME0, while itime from rst.lcv file is $itime"
    exit 1
fi

# Compute anymd/anhms
nymdb=$(echo $itime | cut -c1-8)
hhb=$(echo $itime | cut -c10-11)
nhmsb=${hhb}0000
varoffset_sec=$(($VAROFFSET*60))
anadate=$(tick $nymdb $nhmsb $varoffset_sec)
read anymd anhms <<< $anadate

# Write anymd/anhms to TIMEFILE
: "${TIMEFILE?is not set}"
(
    flock -x 9
    echo "# Atmens times" >> $TIMEFILE
    echo "nymdb: $nymdb" >> $TIMEFILE
    echo "nhmsb: $nhmsb" >> $TIMEFILE
    echo "anymd: $anymd" >> $TIMEFILE
    echo "anhms: $anhms" >> $TIMEFILE
) 9>/tmp/"$TIMEFILE".lock

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
