#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="UpdateHistoryResource4Fcst"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common gcm iau"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
DateTimeDir=$ThisScriptDir/utils/datetime
FCSTSTG=$FVHOME/fcst/stage
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# foffset_sec
foffset_sec=$(grep "^foffset_sec:" $TIMEFILE | cut -d":" -f2 | tr -d " ")

# Trajectory times (TrjBegEpoch, TrjEndEpoch)
# uses foffset_sec
source $DateTimeDir/TrajectoryTimes.sh
GetTrajectoryTimes_

TrjBegDate=${TrjBegEpoch[0]}
TrjBegTime=${TrjBegEpoch[1]}
TrjEndDate=${TrjEndEpoch[0]}
TrjEndTime=${TrjEndEpoch[1]}

dtg=($(rst_date d_rst))
if [ $ifcst -ne 0 ]; then
    rundt=$foffset_sec # offset time from synoptic hour
else
    rundt=$(grep "^HEARTBEAT_DT:" CAP.rc.tmpl | cut -d: -f2)
fi
initref=($(tick ${dtg[@]} $rundt))

hh=$((rundt/3600))
mn=$((($rundt-(3600*$hh))/60))
hh=$(echo $hh |awk '{printf "%02d", $1}')
mn=$(echo $mn |awk '{printf "%02d", $1}')
trajfrqhms=${hh}${mn}00

/bin/rm -f histrc_sed_file
echo "s/>>>REFDATE<<</${initref[0]}/g"  > histrc_sed_file
echo "s/>>>REFTIME<<</${initref[1]}/g" >> histrc_sed_file
echo "s/>>>IOBTRJD<<</$TrjBegDate/1"   >> histrc_sed_file
echo "s/>>>IOBTRJT<<</$TrjBegTime/1"   >> histrc_sed_file
echo "s/>>>IOETRJD<<</$TrjEndDate/1"   >> histrc_sed_file
echo "s/>>>IOETRJT<<</$TrjEndTime/1"   >> histrc_sed_file
echo "s/>>>TRAJFRQ<<</$trajfrqhms/1"   >> histrc_sed_file
echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> histrc_sed_file

# initial time
gcm_nymd0=$(grep "^gcm_nymd0:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
gcm_nhr0=$(grep "^gcm_nhms0:" simtimes.yaml | cut -d":" -f2 | tr -d " " | cut -c1-2)
itime=${gcm_nymd0}_${gcm_nhr0}z

/bin/rm -f HISTORY.rc
hh_beg=$(echo $itime | cut -c10-11)
if [ -e HISTORY_${hh_beg}.rc.tmpl ]; then
    sed -f histrc_sed_file ./HISTORY_${hh_beg}.rc.tmpl > ./HISTORY.rc
else
    sed -f histrc_sed_file ./HISTORY.rc.tmpl > ./HISTORY.rc
fi
cat HISTORY.rc

# TODO: Move this to some "copy" script
if [ -e $FCSTSTG/$EXPID.vtx_prs_rst.$itime.$NCSUFFIX ]; then
    vtxfile=$(echo $EXPID.vtx_prs_rst.$itime.$NCSUFFIX | sed -e 's/vtx_prs_rst/vtx.prs/')
    ln -s $FCSTSTG/$EXPID.vtx_prs_rst.$itime.bin ./$vtxfile
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
