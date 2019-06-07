#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="UpdateAgcmResource4Fcst"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common gcm iau"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

/bin/rm -f agcmrc_sed_file AGCM.rc AGCM.rc.HOLD

# date/time
fcs_nymdi=22000101
fcs_nhmsi=000000
nymdt=$(grep "^fcst_beg_nymd:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhmst=$(grep "^fcst_beg_nhms:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
hht=$(echo $nhmst | cut -c1-2)
foffset_sec=$(grep "^foffset_sec:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
anadate=($(tick $nymdt $nhmst $foffset_sec))
dtg=($(rst_date d_rst))
if [ $ifcst -ne 0 ]; then
    rundt=$foffset_sec # offset time from synoptic hour
else
    rundt=$(grep "^HEARTBEAT_DT:" CAP.rc.tmpl | cut -d: -f2)
fi
initref=($(tick ${dtg[@]} $rundt))

# Define AGCM to properly output desired restarts
echo "s/>>>REFDATE<<</${fcs_nymdi}/1"            > agcmrc_sed_file
echo "s/>>>REFTIME<<</${fcs_nhmsi}/1"           >> agcmrc_sed_file
echo "s/>>>FCSDATE<<</${fcs_nymdi}/1"           >> agcmrc_sed_file
echo "s/>>>FCSTIME<<</${fcs_nhmsi}/1"           >> agcmrc_sed_file
echo "s/>>>RECFINL<<</NO/1"                     >> agcmrc_sed_file # do not write out final state
echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"           >> agcmrc_sed_file

if [ $ifcst -ne 0 ]; then
    if [ $DO4DIAU -ne 0 ]; then
	echo "s/>>>4DIAUDAS<<<//1"              >> agcmrc_sed_file # no 4d-tendency for now
	echo "s/>>>FORCEDAS<<</#/1"             >> agcmrc_sed_file
    else
        echo "s/>>>4DIAUDAS<<</#/1"             >> agcmrc_sed_file # no 4d-tendency for now
        echo "s/>>>FORCEDAS<<<//1"              >> agcmrc_sed_file
    fi
    echo "s/>>>ANADATE<<</${initref[0]}/1"      >> agcmrc_sed_file
    echo "s/>>>ANATIME<<</${initref[1]}/1"      >> agcmrc_sed_file
else
    echo "s/>>>4DIAUDAS<<</#/1"                 >> agcmrc_sed_file # no 4d-tendency for now
    echo "s/>>>FORCEDAS<<</#/1"                 >> agcmrc_sed_file
    echo "s/>>>ANADATE<<</${anadate[0]}/1"      >> agcmrc_sed_file
    echo "s/>>>ANATIME<<</${anadate[1]}/1"      >> agcmrc_sed_file
fi
echo "s/>>>COUPLED<<</#/1"                      >> agcmrc_sed_file
echo "s/>>>DATAOCEAN<<<//1"                     >> agcmrc_sed_file
echo "s/>>>FORCEGCM<<</#/1"                     >> agcmrc_sed_file
if [ $blendrs -ne 0 ] || [ $blendec -ne 0 ]; then
    echo "s/>>>REGULAR_REPLAY<<</ /1"           >> agcmrc_sed_file
    if [ $blendrs -ne 0 ]; then
        echo "s/>>>REGULAR_REPLAY_ECMWF<<</#/1" >> agcmrc_sed_file
        echo "s/>>>REGULAR_REPLAY_NCEP<<</ /1"  >> agcmrc_sed_file
        echo "s/>>>REGULAR_REPLAY_GMAO<<</#/1"  >> agcmrc_sed_file
    fi

    if [ $blendec -ne 0 ]; then
        echo "s/>>>REGULAR_REPLAY_ECMWF<<</ /1" >> agcmrc_sed_file
        echo "s/>>>REGULAR_REPLAY_NCEP<<</#/1"  >> agcmrc_sed_file
        echo "s/>>>REGULAR_REPLAY_GMAO<<</#/1"  >> agcmrc_sed_file
    fi

else
    echo "s/>>>REGULAR_REPLAY_ECMWF<<</#/1"     >> agcmrc_sed_file
    echo "s/>>>REGULAR_REPLAY_NCEP<<</#/1"      >> agcmrc_sed_file
    echo "s/>>>REGULAR_REPLAY_GMAO<<</#/1"      >> agcmrc_sed_file
    echo "s/>>>REGULAR_REPLAY<<</#/1"           >> agcmrc_sed_file
fi
time0date=${nymdt}_${hht}
echo "s/>>>ANA0YYYYMMDDHH<<</$time0date/1"      >> agcmrc_sed_file

sed -f agcmrc_sed_file ./AGCM.rc.tmpl > ./AGCM.rc
if [ -e ./AGCM.rc.tmpl.HOLD ]; then # Bootstrapping
    sed -f agcmrc_sed_file ./AGCM.rc.tmpl.HOLD > ./AGCM.rc.HOLD
fi
cat AGCM.rc

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
