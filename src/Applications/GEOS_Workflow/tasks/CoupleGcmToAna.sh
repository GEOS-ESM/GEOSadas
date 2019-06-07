#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CoupleGcmToAna"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana obs"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Segment times (ana_nymde, ana_nhmse)
ana_nymde=$(grep ^ana_nymde: $TIMEFILE | cut -d":" -f2 | tr -d " ")
ana_nhmse=$(grep ^ana_nhmse: $TIMEFILE | cut -d":" -f2 | tr -d " ")

# bkg_dateb and list of bkg files
source $ThisScriptDir/BkgFileList.sh
GetBkgFileList_ $ana_nymde $ana_nhmse $VAROFFSET $TIMEINC $ASYNBKG $OBSWINDOW

#
# Save the last bkgsfc/upa output within this cycle so
# it doesn't get overwritten
#

if [ $ASYNBKG -ne 360 ]; then

    bkg_nymdl=${bkg_dateb[0]}
    bkg_nhmsl=${bkg_dateb[1]}

    vtxp3=$(echorc.x -template $EXPID $bkg_nymdl $bkg_nhmsl upper-air_vtx_filename)
    vtx09=$(echorc.x -template $EXPID $bkg_nymdl $bkg_nhmsl upper-air_vtx09_filename)
    if [ -e $vtxp3 ]; then
	/bin/cp $vtxp3 $vtx09  &
    fi

    sfcp3=$(echorc.x -template $EXPID $bkg_nymdl $bkg_nhmsl surface_bkg_filename)
    lastndx=$((${#bkgsfc_lst[@]}-1))
    /bin/cp $sfcp3 ${bkgsfc_lst[$lastndx]}  &

    bkgp3=$(echorc.x -template $EXPID $bkg_nymdl $bkg_nhmsl upper-air_bkg_filename)
    lastndx=$((${#bkgupa_lst[@]}-1))
    /bin/cp $bkgp3 ${bkgupa_lst[$lastndx]} &

    cbkgp3=$(echorc.x -template $EXPID $bkg_nymdl $bkg_nhmsl chem_bkg_filename)
    lastndx=$((${#chembkg_lst[@]}-1))
    /bin/cp $cbkgp3 ${chembkg_lst[$lastndx]} &

fi
wait

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
