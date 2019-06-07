#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="AerosolAnalysis"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common aod aod_mpi mpi mkl"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
DateTimeDir=$ThisScriptDir/utils/datetime
CWD=$(pwd) # current location
TIMEFILE=$FVWORK/simtimes.yaml
FVBIN=$FVROOT/bin

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Exit if not doing GAAS analysis
if [ $GAAS_ANA -eq 0 ]; then
    echo "GAAS_ANA==0 --> Nothing to do. Exiting..."
    exit 0
fi

if [ -e $FVWORK/GAAS.BOOTSTRAP ]; then # if bootstrapping...

    $FVROOT/bin/vED -i $FVWORK/GEOS_ChemGridComp.rc -vv ENABLE_GAAS=.FALSE.
    cat $FVWORK/GEOS_ChemGridComp.rc

    for histfile in $(ls $FVWORK/HIST*.rc.tmpl); do
	    $FVROOT/bin/edhist.pl $histfile -i -Xpm _gas_N # exclude
    done

    mv $FVWORK/GAAS.BOOTSTRAP $FVWORK/GAAS.BOOTSTRAP_

    echo "Not running AerosolAnalysis during this segment"
    exit 0

else

    if [ $GAASFDBK -ne 0 ]; then
	    $FVROOT/bin/vED -i $FVWORK/GEOS_ChemGridComp.rc -vv ENABLE_GAAS=.TRUE.
	    cat $FVWORK/GEOS_ChemGridComp.rc
    fi

    for histfile in $(ls $FVWORK/HIST*.rc.tmpl); do
	    $FVROOT/bin/edhist.pl $histfile -i -Ipm _gas_N # include
    done

fi

source $DateTimeDir/GaasTimes.sh
GetGaasTimes_ # sets gaasDateBeg etc.
aod_date=$gaasDateBeg
aod_time=$gaasTimeBeg

# set up separate work directory
AODWORK=$FVWORK/aod.$aod_date.$aod_time
mkdir $AODWORK
touch $AODWORK/.no_archiving

# Copy over pcf files for NNR
/bin/cp $FVHOME/run/gaas/avhrr_l2a.pcf $AODWORK
/bin/cp $FVHOME/run/gaas/modis_l2a.pcf $AODWORK

# Reset value of MODIS_L2A_L2_DIR to $MODIS_STAGE_DIR?
# "set -u" ensures that this code fails if MODIS_STAGE_DIR is not set
if [ $USE_MODIS_STAGE -ne 0 ]; then
    if [ ! -d $MODIS_STAGE_DIR ]; then
        echo "Error. MODIS_STAGE_DIR [$MODIS_STAGE_DIR] directory not found. Aborting..."
        exit 1
    fi
    vED -i -vv MODIS_L2A_L2_DIR=$MODIS_STAGE_DIR $AODWORK/modis_l2a.pcf
fi

# Prepare ana.rc from template
ana_rc_tmpl=$FVHOME/run/gaas/ana.rc.tmpl
ana_rc=$AODWORK/ana.rc
vED -env $ana_rc_tmpl -o $ana_rc

# TODO: CHECK: Is this in the right place???
if [ $SKIP_PSAS -ne 0 ]; then
    vED -i -vv do_you_want_to_skip_PSAS=yes $ana_rc
else
    vED -i -vv do_you_want_to_skip_PSAS=no $ana_rc
fi

# nsteps/bnymd/bnhms
nsteps=$(grep ^acq_nsteps $TIMEFILE | cut -d":" -f2)
bnymd=$(grep ^acq_bnymd $TIMEFILE | cut -d":" -f2)
bnhms=$(grep ^acq_bnhms $TIMEFILE | cut -d":" -f2)

aero_obsdbrc=${AERO_OBSDBRC:-obsys-gaas.rc} # for custom rc, set this env var
rcflag="-rc $AERO_OBSDBRC"
MODIS_L2_HDF=0
patmosxFLG=0
numhrs=$(( $nsteps*6 ))
aod_obclass=$(obsclass_filter.pl $rcflag $AOD_OBSCLASS $bnymd $bnhms $numhrs)
aod_obs_list=($(echo $aod_obclass | sed "s/,/ /g"))

for aod_obs in ${aod_obs_list[@]}; do
    if [ "$aod_obs" == "0" ]; then continue; fi
    if [ "$aod_obs" == "patmosx_asc" ]; then
        patmosxFLG=1
        sed -i "s/#___AVHRR___//" $ana_rc
    fi

    if [ "$aod_obs" == "patmosx_asc" ]; then
        sed -i "s/#___AVHRR___//"   $ana_rc
    fi
    if [ "$aod_obs" == "aeronet_obs" ]; then
        sed -i "s/#___AERONET___//" $ana_rc
    fi
    if [ "$aod_obs" == "misr_F12_bright" ]; then
        sed -i "s/#___MISR___//"    $ana_rc
    fi
    if [ "$aod_obs" == "mod04_land_nnr" ]; then
        sed -i "s/#___TERRA___//"   $ana_rc
    fi
    if [ "$aod_obs" == "myd04_land_nnr" ]; then
       sed -i "s/#___AQUA___//"    $ana_rc
    fi

    dtype=$($FVROOT/bin/aod_data.py aod_type $aod_obs $aero_obsdbrc)

    if [ "$dtype" == "aqua_L2" ]; then
       MODIS_L2_HDF=1
       sed -i "s/#___AQUA_NRT___//" $ana_rc
    fi
    if [ "$dtype" == "terra_L2" ]; then
        MODIS_L2_HDF=1
        sed -i "s/#___TERRA_NRT___//" $ana_rc
    fi
done

if [ $USE_MODIS_STAGE -ne 0 ]; then
     MODIS_L2_HDF=1
     sed -i "s/#___AQUA_NRT___//"  $ana_rc
     sed -i "s/#___TERRA_NRT___//" $ana_rc
fi

echo "cat $ana_rc"
cat $ana_rc

# copy other resource files and aerosol bkgs to work directory
/bin/cp $FVHOME/run/gaas/*.rc $AODWORK
for aod_time_offset in 000000 030000; do
    gaastime=($(tick $aod_date $aod_time 0 $aod_time_offset))
    ymd=${gaastime[0]}
    hms=${gaastime[1]}

    flg1="-template $EXPID $ymd $hms"
    flg2="-rc $AODWORK/gaas.rc gaas_bkg_filename"
    aer_f=$(echorc.x $flg1 $flg2)
    echo $aer_f

    # this test is to catch funky behavior under MPT
    # where sometimes env is not correct
    if [ "$aer_f" == "" ]; then
        echo "file $aer_f not found in $AODWORK, aborting ... "
        exit 1
    fi

    /bin/cp $aer_f $AODWORK
done

# Write job script to AODWORK
# TODO: I don't like the idea of writing and submitting a job script that
# is not managed by the workflow. We need to take a closer look at this
tmpl="$FVHOME/run/gaas/ana_aod.j.tmpl"
jobf="$AODWORK/ana_aod.j"
aod_label=`echo ${aod_date}_${aod_time} | cut -c1-11`
GAASLOG=$FVWORK/${EXPID}.ana_aod.log.${aod_label}z.txt

# TODO: Should we put back the NAS-specific logic?

line5="#PBS -l ncpus=$NCPUS_AOD"

GAASFAIL=$AODWORK/ana_aod.FAILED.$$
touch $GAASFAIL

sed -e 5i"$line5" < $tmpl > $jobf
sed -i "s|>>>PATMOSX<<<|${patmosxFLG}|" $jobf
sed -i "s|>>>MODIS_L2_HDF<<<|${MODIS_L2_HDF}|" $jobf

sed -i "s|>>>AODWORK<<<|${AODWORK}|" $jobf
sed -i "s|>>>FVHOME<<<|${FVHOME}|" $jobf
sed -i "s|>>>FVWORK<<<|${FVWORK}|" $jobf
sed -i "s|>>>GAASFAIL<<<|${GAASFAIL}|" $jobf
sed -i "s|>>>MPIRUN_AOD<<<|${MPIRUN_AOD}|" $jobf
sed -i "s|>>>NCPUS_AOD<<<|${NCPUS_AOD}|" $jobf
sed -i "s|>>>NYMD<<<|${aod_date}|" $jobf
sed -i "s|>>>NHMS<<<|${aod_time}|" $jobf

echo "cat $jobf"
if [ $? -ne 0 ]; then
    exit 1
fi

# Run job
export EXTDATA=$FVWORK/ExtData
## submit job
# jobid=$(qsub -W block=true -V -o $GAASLOG $jobf)
chmod u+x $jobf
$jobf 0 &> $GAASLOG

if [ -e $GAASFAIL ]; then
    echo "Job [$jobf] failed"
    exit 1
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
