#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="RenameRstCheckpoint"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common gcm ana"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
TIMEFILE=$FVWORK/simtimes.yaml
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Final_=1 # QUESTION: do we really need this?

# Timestamp
wrt_rst_nymd=$(grep ^wrt_rst_nymd: $TIMEFILE | cut -d":" -f2 | tr -d " ")
wrt_rst_nhr=$(grep ^wrt_rst_nhms: $TIMEFILE | cut -d":" -f2 | tr -d " " | cut -c1-2)
timestamp=${wrt_rst_nymd}_${wrt_rst_nhr}

# If bootstrapping...
if [ -e $FVWORK/AGCM.rc.tmpl.HOLD ]; then
    /bin/mv -f $FVWORK/AGCM.rc.tmpl.HOLD $FVWORK/AGCM.rc.tmpl
    /bin/mv -f $FVWORK/AGCM.rc.HOLD $FVWORK/AGCM.rc
fi
cat ./AGCM.rc
grs_list=($(grs_list.pl -rc $FVWORK/AGCM.rc))
echo "grs_list: ${grs_list[@]}"

# TODO: Handle AOD analysis files when doing ensemble ADAS
# possibly in a different script

# Move checkpoint files into restart files
#if [ $Final_ -ne 0 ]; then

for rsType in ${grs_list[@]}; do

    if [ "$rsType" == "agcm_import" ] || [ "$rsType" == "agcm_internal" ] || [ "$rsType" == "aiau_import" ] ; then
	continue
    fi

    GCMrst=${rsType}_rst
    GCMrst_TS=${GCMrst}.${timestamp}z.$RSTSUFFIX

    # intermittent 6-hr analysis case
    if [ $ASYNBKG -eq 360 ]; then
        checkpoint=${rsType}_checkpoint
    else
        checkpoint=${rsType}_checkpoint.${timestamp}00z.$RSTSUFFIX
    fi

    if [ -e $checkpoint ]; then
        /bin/mv $checkpoint $GCMrst
        # if [ $NUMSEGS -eq 1 ]; then TODO: Double check this block
        #     /bin/mv $GCMrst ${EXPID}.$GCMrst_TS &
        # else
        /bin/cp $GCMrst ${EXPID}.$GCMrst_TS &
        # fi
    else
	echo "ERROR: GCM checkpoint file [$checkpoint] not found! Aborting..."
	exit 1
    fi

done
wait

#fi  # < $Final_ >

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
