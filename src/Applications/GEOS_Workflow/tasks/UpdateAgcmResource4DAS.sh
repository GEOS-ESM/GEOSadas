#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="UpdateAgcmResource4DAS"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana gcm iau"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Segment times
nymdb=$(grep ^nymdb: $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep ^nhmsb: $TIMEFILE | cut -d":" -f2 | tr -d " ")
gcm_nymd0=$(grep ^gcm_nymd0: $TIMEFILE | cut -d":" -f2 | tr -d " ")
gcm_nhms0=$(grep ^gcm_nhms0: $TIMEFILE | cut -d":" -f2 | tr -d " ")
ana0_hr=$(echo $gcm_nhms0 |cut -c1-2)
ana0date=${gcm_nymd0}_${ana0_hr}
wrt_ana=($gcm_nymd0 $gcm_nhms0)

# Restart times
wrt_rst_nymd=$(grep ^wrt_rst_nymd: simtimes.yaml | cut -d":" -f2 | tr -d " ")
wrt_rst_nhms=$(grep ^wrt_rst_nhms: simtimes.yaml | cut -d":" -f2 | tr -d " ")

Viter_=0
# Final_=1

if [ $DO4DVAR -ne 0 ]; then
    echo "$myname: 4DVAR-specific code not implemented yet"
    exit 1
fi

# Define time to write-out extra restart (normally used for fcst)
# TODO: This should probably be removed from here
fcs_nymdi=37760704
fcs_nhmsi=010000
if [ ! $(grep -c "@" saverst.rc) ]; then
    saverst=($(cat saverst.rc))
    tot2store=${#saverst[@]}
    id0=0
    while [ $id0 -lt $tot2store ]; do
	((id0++))
	saveme="${saverst[$id0]}"
	if [ "$wrt_fcs_nhms" == "${saveme}0000" ]; then
            fcs_nymdi=$nymdb
            fcs_nhmsi=${saveme}0000
	fi
    done  # < id0 >
fi  # < -e saverst.rc >

# ---
# Define AGCM resource
# ---

/bin/rm -f sed_file AGCM.rc AGCM.rc.HOLD

echo "s/>>>REFDATE<<</${wrt_rst_nymd}/1"        > sed_file # actual date to write rst for next cycle
echo "s/>>>REFTIME<<</${wrt_rst_nhms}/1"       >> sed_file # actual time to write rst for next cycle

if [ $MKRESRST -ne 0 ]; then
    echo "s/>>>RECFINL<<</YES/1"               >> sed_file # write out checkpoint rst
else
    echo "s/>>>RECFINL<<</NO/1"                >> sed_file # do not write out checkpoint rst
fi

echo "s/>>>FCSDATE<<</${fcs_nymdi}/1"          >> sed_file # date of extra restart (normally for fcst)
echo "s/>>>FCSTIME<<</${fcs_nhmsi}/1"          >> sed_file # time of extra restart (normally for fcst)

if [ $DOIAU -ne 0 ]; then
    if [ $DO4DIAU -ne 0 ]; then
        echo "s/>>>FORCEDAS<<</#/1"            >> sed_file # force gcm w/ IAU increment
        echo "s/>>>4DIAUDAS<<<//1"             >> sed_file # no 4d-tendency
    else
        echo "s/>>>FORCEDAS<<<//1"             >> sed_file # force gcm w/ IAU increment
        echo "s/>>>4DIAUDAS<<</#/1"            >> sed_file # no 4d-tendency
    fi
else
    echo "s/>>>FORCEDAS<<</#/1"                >> sed_file # run free gcm
    echo "s/>>>4DIAUDAS<<</#/1"                >> sed_file # no 4d-tendency
fi

if [ $WCONSTRAINT -ne 0 ]; then
    # WCONSTRAINT for now only controls FGAT
    # in this case, ana writes inc in middle of time period
    # tauanl=$(($VAROFFSET*60))
    # wrt_inc=($(tick $wrt_ana[0] $wrt_ana[1] $tauanl))
    # echo "s/>>>ANADATE<<</${wrt_inc[0]}/1"     >> sed_file
    # echo "s/>>>ANATIME<<</${wrt_inc[1]}/1"     >> sed_file
    echo "$myname: Non-zero WCONSTRAINT code has not been implemented yet"
else # if not weak constraint ...
    if [ $DOIAU -ne 0 ]; then
        echo "s/>>>ANADATE<<</${nymdb}/1"      >> sed_file
        echo "s/>>>ANATIME<<</${nhmsb}/1"      >> sed_file
    else
        echo "s/>>>ANADATE<<</${wrt_ana[0]}/1" >> sed_file
        echo "s/>>>ANATIME<<</${wrt_ana[1]}/1" >> sed_file
    fi
fi # <WCONSTRAINT>

echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"          >> sed_file
echo "s/>>>COUPLED<<</#/1"                     >> sed_file
echo "s/>>>DATAOCEAN<<<//1"                    >> sed_file
echo "s/>>>FORCEGCM<<</#/1"                    >> sed_file
echo "s/>>>REGULAR_REPLAY_ECMWF<<</#/1"        >> sed_file
echo "s/>>>REGULAR_REPLAY_NCEP<<</#/1"         >> sed_file
echo "s/>>>REGULAR_REPLAY_GMAO<<</#/1"         >> sed_file
echo "s/>>>REGULAR_REPLAY<<</#/1"              >> sed_file
echo "s/>>>ITER<<</$Viter_/1"                  >> sed_file
echo "s/>>>ANA0YYYYMMDDHH<<</$ana0date/1"      >> sed_file

if [ -e AGCM_${Viter_}.rc.tmpl ]; then
    sed -f sed_file ./AGCM_${Viter_}.rc.tmpl > ./AGCM.rc
else
    sed -f sed_file ./AGCM.rc.tmpl           > ./AGCM.rc
fi
if [ -e AGCM.rc.tmpl.HOLD ]; then # BOOTSTRAPping
    sed -f sed_file ./AGCM.rc.tmpl.HOLD      > ./AGCM.rc.HOLD
fi
cat AGCM.rc

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
