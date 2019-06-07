#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="UpdateHistoryResource4DAS"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana gcm iau"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
DateTimeDir=$ThisScriptDir/utils/datetime
TIMEFILE="simtimes.yaml"

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Times
nymdb=$(grep ^nymdb: $TIMEFILE | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep ^nhmsb: $TIMEFILE | cut -d":" -f2 | tr -d " ")
gcm_nymdb=$(grep ^gcm_nymdb: $TIMEFILE | cut -d":" -f2 | tr -d " ")
gcm_nhmsb=$(grep ^gcm_nhmsb: $TIMEFILE | cut -d":" -f2 | tr -d " ")
wrt_rst_nymd=$(grep ^wrt_rst_nymd: $TIMEFILE | cut -d":" -f2 | tr -d " ")
wrt_rst_nhms=$(grep ^wrt_rst_nhms: $TIMEFILE | cut -d":" -f2 | tr -d " ")

# Trajectory times - Trj(Beg,End)Epoch
source $DateTimeDir/TrajectoryTimes.sh
GetTrajectoryTimes_

# Viter_=0
# Final_=1

# Don't need any model output during the short-time forecast
if [ $SHORTFCST -ne 0 ]; then
    /bin/rm -f sed_file
    echo "EXPID:  $EXPID "   > sed_file
    echo "EXPDSC: $EXPID "  >> sed_file
    /bin/mv sed_file ./HISTORY.rc

    exit 0
fi

# Define time to write-out extra restart (normally used for fcst)
fcs_nymdi=22000101
fcs_nhmsi=000000
if [ ! $(grep -c "@" saverst.rc) ]; then
    saverst=($(cat saverst.rc))
    tot2store=${#saverst[@]}
    id0=0
    while [ $id0 -lt $tot2store ]
    do
        ((id0++))
        saveme="${saverst[$id0]}"
        if [ "$wrt_fcs_nhms" == "${saveme}0000" ]; then
            fcs_nymdi=$nymdb
            fcs_nhmsi=${saveme}0000
        fi
    done  # < id0 >
fi  # < -e saverst.rc >


if [ -e $FCSTAGE/initadj.rc ]; then
    set fcs_nymdi = $nymdb
    set fcs_nhmsi = $nhmsb
fi # < -e initadj.rc >

rundt=$(grep "^HEARTBEAT_DT:" CAP.rc.tmpl | cut -d: -f2)
hh=$(($rundt/3600))
mn=$((($rundt-(3600*$hh))/60))
hh=$(echo $hh |awk '{printf "%02d", $1}')
mn=$(echo $mn |awk '{printf "%02d", $1}')
trajfrqhms=${hh}${mn}00

varwindow_sec=$(($TIMEINC*60))
qcref=($(tick $wrt_rst_nymd $wrt_rst_nhms $varwindow_sec))  # tick time to synoptic hour
ioebkgd=${qcref[0]}  # date to stop writing background files
ioebkgt=${qcref[1]}  # time to stop writing background files

/bin/rm -f sed_file
echo "s/>>>IOEDATE<<</${gcm_nymdb}/1" > sed_file
echo "s/>>>IOETIME<<</${gcm_nhmsb}/1" >> sed_file

if [ -e replay.acq ]; then   # in replay mode, only need to write bkg at synoptic times
    echo "s/>>>IOBBKGD<<</${ana_nymde}/1" >> sed_file
    echo "s/>>>IOBBKGT<<</${ana_nhmse}/1" >> sed_file
    echo "s/>>>IOEBKGD<<</${ana_nymde}/1" >> sed_file
    echo "s/>>>IOEBKGT<<</${ana_nhmse}/1" >> sed_file
    echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1" >> sed_file
else                         # in regular DAS, write bkg until the end of the cycle
    if [ $DOIAU -ne 0 ]; then
        TrjBegDate=${TrjBegEpoch[0]}
        TrjBegTime=${TrjBegEpoch[1]}
        TrjEndDate=${TrjEndEpoch[0]}
        TrjEndTime=${TrjEndEpoch[1]}

        qcref=($(tick $wrt_rst_nymd $wrt_rst_nhms 10800)) # tick time to synoptic hour
        qcref_nymd=${qcref[0]}
        qcref_nhms=${qcref[1]}
        echo "s/>>>IOBBKGD<<</${wrt_rst_nymd}/1" >> sed_file # for IAU, only write bkg's at later half of integration
        echo "s/>>>IOBBKGT<<</${wrt_rst_nhms}/1" >> sed_file
        echo "s/>>>IOBQCD<<</${qcref_nymd}/1"    >> sed_file
        echo "s/>>>IOBQCT<<</${qcref_nhms}/1"    >> sed_file
        echo "s/>>>IOEQCD<<</${qcref_nymd}/1"    >> sed_file
        echo "s/>>>IOEQCT<<</${qcref_nhms}/1"    >> sed_file
        echo "s/>>>IOBTRJD<<</$TrjBegDate/1"     >> sed_file
        echo "s/>>>IOBTRJT<<</$TrjBegTime/1"     >> sed_file
        echo "s/>>>IOETRJD<<</$TrjEndDate/1"     >> sed_file
        echo "s/>>>IOETRJT<<</$TrjEndTime/1"     >> sed_file
        echo "s/>>>TRAJFRQ<<</$trajfrqhms/1"     >> sed_file
    else
        echo "s/>>>IOBBKGD<<EOF</17760704/1"     >> sed_file # write bkg's from begining of time
        echo "s/>>>IOBBKGT<<EOF</000000/1"       >> sed_file
    fi

    echo "s/>>>IOEBKGD<<</${ioebkgd}/1"          >> sed_file
    echo "s/>>>IOEBKGT<<</${ioebkgt}/1"          >> sed_file
    echo "s/>>>FCSDATE<<</${fcs_nymdi}/1"        >> sed_file
    echo "s/>>>FCSTIME<<</${fcs_nhmsi}/1"        >> sed_file
    echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"        >> sed_file
fi

# Set date/times for gaas_bkg.sfc outputs. These gaas times
# are specific to the GCM run and do not affect AerosolAnalysis
rstdate=($(rst_date ./d_rst))
gaasDateTimeBeg=($(tick ${rstdate[@]} 0 090000))
gaasDateTimeEnd=($(tick ${rstdate[@]} 0 120000))
gaasDateBeg=${gaasDateTimeBeg[0]}
gaasTimeBeg=${gaasDateTimeBeg[1]}
gaasDateEnd=${gaasDateTimeEnd[0]}
gaasTimeEnd=${gaasDateTimeEnd[1]}

echo "s/>>>GAASDATEBEG<<</$gaasDateBeg/" >> sed_file
echo "s/>>>GAASTIMEBEG<<</$gaasTimeBeg/" >> sed_file
echo "s/>>>GAASDATEEND<<</$gaasDateEnd/" >> sed_file
echo "s/>>>GAASTIMEEND<<</$gaasTimeEnd/" >> sed_file

# set date/times for gaas inst outputs
IgaasDateTimeBeg=($(tick ${rstdate[@]} 0 030000))
IgaasDateTimeEnd=($(tick ${rstdate[@]} 0 060000))

IgaasDateBeg=${IgaasDateTimeBeg[0]}
IgaasTimeBeg=${IgaasDateTimeBeg[1]}

IgaasDateEnd=${IgaasDateTimeEnd[0]}
IgaasTimeEnd=${IgaasDateTimeEnd[1]}

echo "s/>>>IGAASDATEBEG<<</$IgaasDateBeg/" >> sed_file
echo "s/>>>IGAASTIMEBEG<<</$IgaasTimeBeg/" >> sed_file
echo "s/>>>IGAASDATEEND<<</$IgaasDateEnd/" >> sed_file
echo "s/>>>IGAASTIMEEND<<</$IgaasTimeEnd/" >> sed_file

/bin/rm -f ./HISTORY.rc

# Which HISTORY to use?
hh=$(echo $gcm_nhms0 | cut -c1-2)
myhist=HISTORY.rc.tmpl
if [ -e HISTORY_${hh}.rc.tmpl ]; then
    myhist=HISTORY_${hh}.rc.tmpl
fi
sed -f sed_file $myhist > ./HISTORY.rc # full  diagnostics all other cases

# cat ./HISTORY.rc

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
