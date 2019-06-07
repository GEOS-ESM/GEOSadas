#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CopyResourceFiles4Fcst"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

source $ThisScriptDir/CopyResourceFiles.sh
CopyResourceFiles_

for myfile in $FVHOME/fcst/*; do
    if [ -d $myfile ]; then continue; fi
    if [[ $file == *.log.* ]]; then continue; fi
    fname=$(basename "$myfile")
    ext="${fname##*.}"
    # exclude restarts
    if [ "$ext" != "bin" ] && [ "$ext" != "$NCSUFFIX" ]; then
	/bin/cp $myfile $FVWORK
    fi
done

# Rename AGCM resource file for bootstrap
if [ -e $FVHOME/fcst/AGCM.BOOTSTRAP.rc.tmpl ]; then
    cp -f $FVWORK/AGCM.rc.tmpl $FVWORK/AGCM.rc.tmpl.HOLD
    cp -f $FVWORK/AGCM.BOOTSTRAP.rc.tmpl $FVWORK/AGCM.rc.tmpl
fi

# Disble GAAS feedback from GCM, in case a new
# GEOS_ChemGridComp.rc has been copied from $FVHOME/fcst
vED -i $FVWORK/GEOS_ChemGridComp.rc -vv ENABLE_GAAS=.FALSE.
cat $FVWORK/GEOS_ChemGridComp.rc

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
