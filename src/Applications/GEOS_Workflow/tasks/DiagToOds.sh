#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="DiagToOds"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana ana_mpi"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

nproc=$((NCPUS_GSI/4))
if [ $nproc -lt 1 ]; then
    nproc=1
fi

nymdb=$(grep ^nymdb: simtimes.yaml | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep ^nhmsb: simtimes.yaml | cut -d":" -f2 | tr -d " ")
hh=$(echo $nhmsb | cut -c1-2)

diag2ods -rc $FVWORK/odsmatch.rc -ncpus $nproc \
         -log $EXPID.ods.log.${nymdb}_${hh}z.txt $nymdb $nhmsb $EXPID

if [ $DO_0HR_IMP -ne 0 ]; then
    obimp_summary.pl $nymdb $nhmsb
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
