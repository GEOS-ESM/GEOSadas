#!/usr/bin/env bash

# NOTE: THIS SCRIPT SHOULD PROBABLY BE CALLED RenameRstCheckpoint4Fcst.sh
# THIS SCRIPT SEEMS LIKE A COMBINATION OF RenameRstCheckpoint and UpdateDateTime

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="Geos5SaveFcstRestart"

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

if [ $(ls *_checkpoint* | wc -l) == 0 ]; then
    echo "No checkpoint files - nothing to rename/save. Exiting..."
    exit 0
fi

# Move checkpoint files into restart files
recdate=$(grep RECORD_REF_DATE AGCM.rc | grep -v "#" | cut -d: -f2)
rectime=$(grep RECORD_REF_TIME AGCM.rc | grep -v "#" | cut -d: -f2)
rst_tag="${recdate[0]}_$(echo ${rectime[0]} | cut -c1-2)z.$RSTSUFFIX"
chk_tag="${recdate[0]}_$(echo ${rectime[0]} | cut -c1-4)z.$RSTSUFFIX"
grs_list=($(grs_list.pl -rc AGCM.rc))
for rs in ${grs_list[@]}; do
    /bin/mv  ${rs}_checkpoint.${chk_tag}  $EXPID.${rs}_rst.${rst_tag}
done

# d_rst
/bin/rm -f ./d_rst
if [ ${#grs_list[@]} > 0 ]; then
    mkdrstdate.x ${recdate[1]} ${rectime[1]}  $EXPID.rst.lcv.${rst_tag}
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
