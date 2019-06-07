#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AcquireObservations"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana obs aod"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location
FVSPOOL="$FVHOME/spool"
TIMEFILE=$FVWORK/simtimes.yaml
ACQ_FAILED=$FVWORK/acquire.FAILED
DMF_FAILED=$FVWORK/dmf.FAILED

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Check DMF status
# TODO

# nsteps/bnymd/bnhms/enymd
nsteps=$(grep ^acq_nsteps $TIMEFILE | cut -d":" -f2)
bnymd=$(grep ^acq_bnymd $TIMEFILE | cut -d":" -f2)
bnhms=$(grep ^acq_bnhms $TIMEFILE | cut -d":" -f2)
enymd=$(grep ^acq_enymd $TIMEFILE | cut -d":" -f2)

# Set default values if env vars are not defined
aero_obsdbrc=${AERO_OBSDBRC:-obsys-gaas.rc} # for custom rc, set this env var
obsclass=${OBSCLASS:-NONE}
req_obsclass=${REQ_OBSCLASS:-NONE}
aod_obsclass=${AOD_OBSCLASS:-NONE}

aerosol_acquire=1
if [ $LOCAL_ACQUIRE -ne 0 ] || [ -f replay.acq ]; then
    aerosol_acquire=0
fi

while [ $bnymd -le $enymd ] && [ ! -f $ACQ_FAILED ] && [ ! -f $DMF_FAILED ]; do

    touch $ACQ_FAILED
    argsf="-v -d $FVWORK -s $FVSPOOL -ssh"

    if [ "$obsclass" != "NONE" ]; then
        acquire_obsys $argsf $bnymd $bnhms 060000 $nsteps $obsclass
    fi
    if [ "$req_obsclass" != "NONE" ]; then
        acquire_obsys $argsf -strict $bnymd $bnhms 060000 $nsteps $req_obsclass
    fi
    if [ $GAAS_ANA -ne 0 ] && [ $aerosol_acquire -ne 0 ] && [ "$aod_obsclass" != "NONE" ]; then
        msteps=$(( $nsteps*2 ))
        acquire_obsys $argsf -strict \
                      -drc $aero_obsdbrc \
                      $bnymd $bnhms 030000 $msteps $aod_obsclass
        setup_gaas_obs.pl $FVWORK -v -avhrr -modis
    fi

    /bin/rm -f $ACQ_FAILED
    bnymd=$(tick $bnymd) # increment by one day

done

# TODO: Acquire tomorrow's obsys data but do not wait

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
