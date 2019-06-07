#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="TagAndRecycle4DAS"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana obs"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location
TIMEFILE=$FVWORK/simtimes.prv.yaml
aoffset_sec=$(($VAROFFSET*60))
FVREC=$FVHOME/recycle
RSTHOLD=$FVREC/hold

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Date times
# TIME0 is an ecflow suite variable that is exported
# by TagAndRecycle.ecf
read nymd0 nhr0z <<< $(echo $TIME0 | awk '{split($0,a,"_"); print a[1],a[2]}')
nymd2=$(grep ^wrt_rst_nymd: $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhms2=$(grep ^wrt_rst_nhms: $TIMEFILE | cut -d":" -f2 | tr -d " ")
ana_nymde=$(grep ^ana_nymde: $TIMEFILE | cut -d":" -f2 | tr -d " ")
ana_nhmse=$(grep ^ana_nhmse: $TIMEFILE | cut -d":" -f2 | tr -d " ")

# TODO: cases FORECAST, SKIPANA, DO4DVAR

# Timestamps for tagging
hour2=$(printf %02d $((${nhms2#0}/10000)))
rtag=${nymd2}_${hour2}z
rtags=${nymd0}_${nhr0z}-${nymd2}_${hour2}z

# TODO: FORECAST
# TODO: simtimes.yaml

/bin/rm -rf $RSTHOLD
/bin/mkdir -p $RSTHOLD

# tag+recycle updated d_rst
/bin/cp -f d_rst $EXPID.d_rst
/bin/mv -f d_rst $EXPID.rst.lcv.$rtag.bin
/bin/cp -f $EXPID.rst.lcv.$rtag.bin $RSTHOLD/
# if ( $final_fcst ) /bin/cp $EXPID.rst.lcv.$rtag.bin ${fcstage}/ &

source $ThisScriptDir/TagAndRecycleFunctions.sh
# tag+recycle GCM files
TagAndRecycleGcm_
# tag+recycle Analysis files
TagAndRecycleAna_

GDA_list=($(ls *GDA.all.*)) || true # suppress error in case of no GDA files
listLength=${#GDA_list[@]}
if [ $listLength -gt 0 ]; then
    lastndx=$(($listLength - 1))
    GDA_rst=$(echo ${GDA_list[$lastndx]} | sed -e 's/\.all\./.rst./')
    /bin/cp ${GDA_list[$lastndx]} ./${GDA_rst}
    # if ( $?trksufx ) then
    #      set tstsufx = `echo $GDA_rst | cut -d. -f5,6`
    #      if ( "$tstsufx" == "$trksufx" ) /bin/rm -f $GDA_rst
    # endif
    if [ -e $GDA_rst ]; then
	/bin/cp $GDA_rst $RSTHOLD/$GDA_rst
    fi
fi

# TODO: ctl files

# TODO: mkresrst

# Tar up restarts
cd $RSTHOLD
tar cvf $FVWORK/$EXPID.rst.${rtag}.tar $EXPID.*
cd -

# Delete restarts for the old run from FVREC and copy
# restarts from RSTHOLD to FVREC
/bin/rm -f $FVREC/*.*
/bin/mv -f $RSTHOLD/* $FVREC/

# All done
cd $FVWORK
zeit_co.x $myname

# Tag zeit file
/bin/mv -f .zeit $EXPID.zeit.reg.$rtags.txt

# Back to orig location
cd $CWD
