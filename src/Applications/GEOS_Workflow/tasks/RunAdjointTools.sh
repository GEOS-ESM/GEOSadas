#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="RunAdjointTools"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common iau adj adj_mpi"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
TIMEFILE=$FVWORK/simtimes.yaml
DateTimeDir=$ThisScriptDir/utils/datetime

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ -e stage4fsens.arc ]; then
    echo "$myname: stage4fsens.arc exists, postponing adjoint run for later"
    exit 0
fi

# Date/time
tmp_nymd=$(grep "^fcst_end_nymd:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
tmp_nhms=$(grep "^fcst_end_nhms:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
fcst_end=($tmp_nymd $tmp_nhms)
nsecs=$(grep "^fcst_nsecs:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
# FcsBegEpoch
source $DateTimeDir/FcstEpochTimes.sh
GetFcstEpochTimes_

# Invoke only when one of these .rc files is present
if [ -e fvsvec.rc ] || [ -e initadj.rc ] || [ -e oseledec.rc ]; then

    iamhere=$(pwd)

    # Calculate singular vectors
    if [ -e fvsvec.rc ] && [ -e fvsvec.ccmrun.namelist.tmpl ]; then
        zeit_ci.x fvsvec
        fvsvec $iamhere $EXPID ${fcst_end[0]} ${fcst_end[1]} $nsecs
        zeit_co.x fvsvec
    fi

    # Calculate sensitivity
    if [ -e CAP_apert.rc.tmpl ]; then
	rundt=$(grep "^HEARTBEAT_DT:" CAP_apert.rc.tmpl | cut -d: -f2)
    else
        rundt=$(grep "^HEARTBEAT_DT:" CAP.rc.tmpl       | cut -d: -f2)
    fi

    thisrc=fvsens.ccmrun.namelist.tmpl
    hh=$(echo ${FcsBegEpoch[1]} | cut -c1-2)
    if [ -e fvsens.ccmrun.namelist_${hh}.tmpl ]; then
	thisrc=fvsens.ccmrun.namelist_${hh}.tmpl
    fi
    if [ -e initadj.rc ] && [ -e $thisrc ]; then
        zeit_ci.x fvsens
        fvsens $iamhere $EXPID ${fcst_end[0]} ${fcst_end[1]} $hh $rundt
        zeit_co.x fvsens
    fi

    # Calculate completementary sensitivity
    if [ -e oseledec.rc ] && [ -e fvoseledec.ccmrun.namelist.tmpl ]; then
	zeit_ci.x fvoseledec
        fvoseledec $iamhere $EXPID $fcst_end[1] $fcst_end[2] $nsecs
        zeit_co.x fvoseledec
    fi

fi # < svec sens >

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
