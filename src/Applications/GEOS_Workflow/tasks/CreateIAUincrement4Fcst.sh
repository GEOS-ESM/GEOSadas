#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CreateIAUincrement4Fcst"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
FVSPOOL="$FVHOME/spool"

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ -e agcm_import_rst ]; then
    echo "agcm_import_rst exists, nothing to do..."
    exit 0
fi

dtg=($(rst_date d_rst))
agcmfn=$(echorc.x -template $EXPID ${dtg[@]} iau_tendency_filename)
if [ -e $FVHOME/fcst/stage/$agcmfn ]; then
    ln -sf $FVHOME/fcst/stage/$agcmfn agcm_import_rst
    exit 0
fi
echo "Could not find agcm_import_rst. Trying to create one..."

if [ ! -e fcst.acq ]; then
    echo $myname": cannot find fcst.acq file, aborting..."
    exit 1
fi

initref=($(tick ${dtg[@]} 10800)) # tick clock 3 hrs ahead of rst time

# TODO: DO4DIAU
fcstacq_freq=060000
fnymdb=${initref[0]}
fnhmsb=${initref[1]}
nfcst=1

# End of IAU-period
timeinc_sec=$(( $TIMEINC*60 ))
end_ana=($(tick fnymdb fnhmsb $timeinc_sec))

acquire -v -rc fcst.acq -s $FVSPOOL -d . $fnymdb $fnhmsb $fcstacq_freq $nfcst
if [ -f fcst09.acq ]; then
    acquire -v -rc fcst09.acq -s $FVSPOOL -d . ${end_ana[0]} ${end_ana[1]} $fcstacq_freq $nfcst
fi

# TODO: DO4DIAU

bkg06fn=$(echorc.x -template $EXPID ${initref[0]} ${initref[1]} upper-air_bkg06_filename)
bkgfile=$(echorc.x -template $EXPID ${initref[0]} ${initref[1]} upper-air_bkg_filename)
bkg4ainc=$bkgfile
if [ -e $bkg06fn ]; then
    bkg4ainc=$bkg06fn
fi
if [ ! -e $bkg4ainc ]; then
    /bin/cp fvpsas.log  ${FVHOME}/run/fvpsas.abnormal.log
    echo $myname": cannot find background file, aborting..."
    exit 1
fi

ana4ainc=$(echorc.x -template $EXPID $initref[0] $initref[1] upper-air_ana_filename)
ifile=$(echorc.x -template $EXPID $initref[0] $initref[1] upper-air_inc_filename)
if [ ! -e $ana4ainc ]; then
    /bin/cp fvpsas.log  ${FVHOME}/run/fvpsas.abnormal.log
    echo $myname": cannot find background file"
    exit 1
fi

if [ -e mkiau.rc.tmpl ]; then
    /bin/rm -f sed_file
    echo "s/>>>EXPID<<</${EXPID}/1"         > sed_file
    echo "s/>>>BKGFNAME<<</${bkg4ainc}/1"  >> sed_file
    echo "s/>>>ANAFNAME<<</${ana4ainc}/1"  >> sed_file
    echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> sed_file
    echo "s/>>>ANADATE<<</${initref[0]}/1" >> sed_file
    echo "s/>>>ANATIME<<</${initref[1]}/1" >> sed_file
    /bin/rm -f ./mkiau.rc
    sed -f sed_file  ./mkiau.rc.tmpl  > ./mkiau.rc
    $MPIRUN_IAU
else
    imout=$(grep AGCM_IM: AGCM.rc.tmpl | cut -d : -f2)
    jmout=$(grep AGCM_JM: AGCM.rc.tmpl | cut -d : -f2)
    $MPIRUN_IAU -ana $ana4ainc -bkg $bkg4ainc \
		-imout $imout -jmout $jmout \
		-divr -iau agcm_import_rst \
		-nymd ${initref[0]} -nhms ${initref[1]}

fi
if [ -e IAU_EGRESS ]; then
    echo "IAU increment generated"
    /bin/rm -f IAU_EGRESS  # clean up and move on
else
    echo "Failed to generate IAU increment"
    exit 1
fi

if [ ! -e $ifile ]; then
    dyn_recenter.x -g5 $ana4ainc $bkg4ainc NONE -o $ifile
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
