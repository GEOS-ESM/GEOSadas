#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CleanUpDAS"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthand
FVRUN=$FVHOME/run

# # Remove FVWORK directory
# /bin/rm -rf $FVWORK

if [ -e $FVRUN/AGCM.BOOTSTRAP.rc.tmpl ]; then
    mv $FVRUN/AGCM.BOOTSTRAP.rc.tmpl $FVRUN/AGCM.BOOTSTRAP.rc.tmpl.DONE
fi

if [ $GAAS_ANA -ne 0 ]; then
    if [ -e $FVRUN/gaas/GAAS.BOOTSTRAP ]; then
	mv $FVRUN/gaas/GAAS.BOOTSTRAP $FVRUN/gaas/GAAS.BOOTSTRAP_
    fi
fi
