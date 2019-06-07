#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CoupleAnaToGcm_4DIAU"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana iau iau_mpi mpi"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location
FVRUN=$FVHOME/run

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

Viter_=0
Final_=1

varwindow_sec=$(($TIMEINC*60))
adtfwd=$varwindow_sec

# Segment times
gcm_nymd0=$(grep ^gcm_nymd0: simtimes.yaml | cut -d":" -f2 | tr -d " ")
gcm_nhms0=$(grep ^gcm_nhms0: simtimes.yaml | cut -d":" -f2 | tr -d " ")
nymdb=$(grep ^nymdb: simtimes.yaml | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep ^nhmsb: simtimes.yaml | cut -d":" -f2 | tr -d " ")
hhb=$(echo $nhmsb | cut -c1-2)
beg_ana=($gcm_nymd0 $gcm_nhms0)
end_ana=($(tick ${beg_ana[@]} $adtfwd))

# Move previous IAU tendency files out of the way
# TODO: QUESTION: Isn't $Viter_ always 0? Check fvpsas.
if [ $Viter_ -gt 0 ]; then
    # TODO: viterm1 has not been used anywhere
    # viterm1=$(($Viter_ - 1))
    rndasfiles -sfx "bin" -purge $EXPID "agcm_import_rst" \
	           ${beg_ana[0]} ${beg_ana[1]} \
               ${end_ana[0]} ${end_ana[1]} $ASYNBKG $Viter_
fi

# Checkpoint, skip if complete
ddz=${beg_ana[0]}
hhz=$(echo ${beg_ana[1]} | cut -c1-2)
DONE_FILE=$FVWORK/.DONE_MEM001_CoupleAnaToGcm_${Viter_}.${ddz}${hhz}
if [ -e $DONE_FILE ]; then
    exit 0
fi

viterp1=$(($Viter_ + 1))

# TODO: QUESTION: What's the point of DOETA2BIN

$FVRUN/lnbcs $nymdb # QUESTION: again? a different date?

# ---
# Convert ana "eta" files to rst
# ---

# Update restart?
updrst0=""
if [ "$MPIRUN_UPRST" != "/dev/null" ]; then
    updrst0="-updrst0"
fi

if [ "$updrst0" == "-updrst0" ]; then
    for fn in fvcore_internal_rst moist_internal_rst pchem_internal_rst; do
        if [ $Viter_ -eq 0 ]; then
            /bin/mv $fn ${fn}_iter0
            ln -sf ${fn}_iter0 $fn
        fi
    done
fi

# Update background from 4d analysis
ana4dupd.pl -ncpus 7 -iau -iter $Viter_ -rcdir . $updrst0 $EXPID \
	    ${beg_ana[0]} ${beg_ana[1]} \
	    ${end_ana[0]} ${end_ana[1]} $ASYNBKG

# When updating initial condition ...
if [ "$updrst0" == "-updrst0" ] && [ $Final_ -eq 0 ]; then
    for fn in fvcore_internal_rst moist_internal_rst pchem_internal_rst
    do
        thishh=$(echo ${beg_ana[1]} | cut -c1-2)
        rndasfiles -sfx $RSTSUFFIX -purge $EXPID $fn \
		           ${beg_ana[0]} ${beg_ana[1]} \
		           ${beg_ana[0]} ${beg_ana[1]} 0 $viterp1
	    /bin/rm $fn
	    ln -sf $EXPID.$fn.${beg_ana[0]}_${thishh}z.iter${viterp1}.bin $fn
    done
fi

agcmfn=$(echorc.x -template $EXPID ${beg_ana[0]} ${beg_ana[1]} iau_tendency_filename)
/bin/ln -sf $agcmfn agcm_import_rst

if [ $HYBRIDGSI != "/dev/null" ] &&
       [ ! -f $FVHOME/run/atmens_replay.acq ] &&
       [ $Final_ -ne 0 ] &&
       [ $STAGE4HYBGSI != "/dev/null" ]; then
    rndasfiles -stage $STAGE4HYBGSI $EXPID ana.eta \
	           ${beg_ana[0]} ${beg_ana[1]} \
               ${end_ana[0]} ${end_ana[1]} $ASYNBKG 99
    touch $FVHOME/.DONE_MEM001_analyzer.${nymdb}${hhb}
fi

# save ana/xinc files at end time-slot before these are overwritten by cycle
ana09upa=$(echorc.x -template $EXPID ${end_ana[0]} ${end_ana[1]} upper-air_ana09_filename)
ana_upa=$(echorc.x -template $EXPID ${end_ana[0]} ${end_ana[1]} upper-air_ana_filename)
/bin/cp $ana_upa $ana09upa

xinc09upa=$(echorc.x -template $EXPID ${end_ana[0]} ${end_ana[1]} upper-air_xinc09_filename)
xinc_upa=$(echorc.x -template $EXPID ${end_ana[0]} ${end_ana[1]} upper-air_xinc_filename)
/bin/cp $xinc_upa $xinc09upa

iau09inc=$(echorc.x -template $EXPID ${end_ana[0]} ${end_ana[1]} iau09_tendency_filename)
iau_inc=$(echorc.x -template $EXPID ${end_ana[0]} ${end_ana[1]} iau_tendency_filename)
/bin/cp $iau_inc $iau09inc

touch $DONE_FILE

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
