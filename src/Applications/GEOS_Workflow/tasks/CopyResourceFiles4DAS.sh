#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CopyResourceFiles4DAS"

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

# Rename AGCM resource file for bootstrap
if [ -e $FVHOME/run/AGCM.BOOTSTRAP.rc.tmpl ]; then
    cp -f $FVWORK/AGCM.rc.tmpl $FVWORK/AGCM.rc.tmpl.HOLD
    cp -f $FVWORK/AGCM.BOOTSTRAP.rc.tmpl $FVWORK/AGCM.rc.tmpl
fi

# Copy GAAS resource files
# Substitute values for all defined env vars
for rcfile in $(ls "$FVHOME"/run/gaas/*.rc)
do
    rcfilename=$(basename $rcfile)
    target=$FVWORK/"$rcfilename"
    vED -env $rcfile -o $target
    cat $target
done
if [ -e "$FVHOME"/run/gaas/GAAS.BOOTSTRAP ]; then
    cp "$FVHOME"/run/gaas/GAAS.BOOTSTRAP $FVWORK
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
