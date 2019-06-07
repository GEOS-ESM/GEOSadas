#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="RunVortexTracker"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common vtx ana"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ $VTRACK -eq 0 ]; then
    echo "$myname: $VTRACK == 0. Exiting..."
fi

aoffset_sec=$(( $VAROFFSET*60 ))
read nymd1g nhms1g <<< $(rst_date d_rst)
read nymd1 nhms1 <<< $(tick $nymd1g $nhms1g $aoffset_sec)

nymdt=$(grep "^fcst_beg_nymd:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhmst=$(grep "^fcst_beg_nhms:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
foffset_sec=$(grep "^foffset_sec:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
anadate=($(tick $nymdt $nhmst $foffset_sec))

jobsgmt=($(echorc.x -rc CAP.rc JOB_SGMT))
ndays=$(( ${jobsgmt[0]}+1 ))

# '${var#0}' removes leading zero from var, otherwise var is
# interpreted as an octal
# e.g. if var=090000. then, ${var#0} is 90000

fcsthrs=$(( ${jobsgmt[0]}*24 + ${b[1]#0}/10000 )) # $var#0
fcstsec=$(( $fcsthrs*3600 ))
offset_hrs=$(( $foffset_sec/3600 ))
adjust_fcsthrs=$(( $fcsthrs-$offset_hrs ))
adjust_fcstsec=$(( $adjust_fcsthrs*3600 ))
fvtrack_end=($(tick $nymd1 $nhms1 $adjust_fcstsec))
nymde=${fvtrack_end[0]}
nhmse=${fvtrack_end[1]}
hhe=$(echo $nhmse | cut -c1-2)

tcvitals_class=$(grep tcvitals $FVHOME/run/obsys.rc | grep BEGIN | cut -d" " -f2)

acquire_obsys -v -d $FVWORK $strict ${anadate[0]} ${anadate[1]} 060000 $ndays $tcvitals_class
ls -lrt *vtx.prs* *vtx.mix*

# Check tcvitals for valid entries
# (assumes tcvitals.yyyymmddhh naming convention)
hh1g=$(echo $nhms1g | cut -c1-2)
hha=$(echo ${anadate[1]} | cut -c1-2)
for vitals in $(ls *tcvitals*); do
    echo "Checking $vitals for valid entries."
    cat $vitals
    tcvdate=$(echo $vitals | cut -d . -f2 | cut -c 1-8)
    tcvtime=$(echo $vitals | cut -d . -f2 | cut -c 9-10)00
    /bin/rm -f ${vitals}.tmp
    /bin/mv $vitals ${vitals}.tmp
    # pc: ignore error in case of empty file
    cat ${vitals}.tmp | grep "$tcvdate $tcvtime" > $vitals || true
    echo "Corrected ${vitals}:"
    cat  $vitals tcvitals.${anadate[0]}${hha}+${nymde}${hhe}
    /bin/rm -f ${vitals}.tmp
done

vtrack -trktyp FV5 -fcst -freq $VTRKFRQF -fcsthrs $adjust_fcsthrs -rc $FVWORK/vtrack.rc ${anadate[0]} ${anadate[1]} $EXPID

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
