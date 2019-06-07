#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="RunAnalysis"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana ana_mpi iau obs mpi mkl"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Segment times
nymdb=$(grep ^nymdb: simtimes.yaml | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep ^nhmsb: simtimes.yaml | cut -d":" -f2 | tr -d " ")

#
# Analysis
#

Viter_=0
Final_=1

hhb=$(echo $nhmsb | cut -c1-2)
echo "nymdb: $nymdb, hhb: $hhb"

DONE_FILE=$FVWORK/.DONE_MEM001_AnalysisRun.${nymdb}${hhb}

if [ -e $DONE_FILE ]; then
    exit 0
fi

# Get dimension of analysis (this defines berror dim)
gsinlat=$(echorc.x -rc GSI_GridComp.rc "GSI JM")
gsinlon=$(echorc.x -rc GSI_GridComp.rc "GSI IM")
gsinlev=$(echorc.x -rc GSI_GridComp.rc "GSI LM")

sfcfile=$(echorc.x -template $EXPID $nymdb $nhmsb surface_bkg_filename)
bkgfile=$(echorc.x -template $EXPID $nymdb $nhmsb upper-air_bkg_filename)
anafile=$(echorc.x -template $EXPID $nymdb $nhmsb upper-air_ana_filename)
odsfile=$(echorc.x -template $EXPID $nymdb $nhmsb post-analysis_obs_filename)
biasfile=$(echorc.x -template $EXPID $nymdb $nhmsb forecast-bias_filename)

#
# Run analysis via its wrapper, analyzer
#

if [ $SKIPANA -ne 0 ]; then

    /bin/cp $bkgfile $anafile

else

    # Check tcvitals for valid entries
    # (assumes tcvitals.yyyymmddhh naming convention)
    for vitals in $(ls *tcvitals*); do
	    echo "Checking $vitals for valid entries."
	    #cat $vitals
	    tcvdate=$(echo $vitals | cut -d . -f2 | cut -c 1-8)
	    tcvtime=$(echo $vitals | cut -d . -f2 | cut -c 9-10)00
	    /bin/rm -f ${vitals}.tmp
	    /bin/mv $vitals ${vitals}.tmp
	    # ignore error in case of empty file (|| true)
	    cat ${vitals}.tmp | grep "$tcvdate $tcvtime" > $vitals || true
	    echo "Corrected ${vitals}:"
	    #cat  $vitals
	    /bin/rm -f ${vitals}.tmp
    done

    # Run analyzer
    dryrun=""
    $dryrun analyzer $nymdb $nhmsb -expid $EXPID -t $SPECRES \
	        -levs $gsinlev -x $gsinlon -y $gsinlat \
	        -jiter $Viter_ -lnobs -log ana.log
    if [ $? -ne 0 ]; then
	    exit 1
    fi
    # TODO: activate ana_check
    # set ana_check = ( `grep ">>> WARNING <<< does not exist" ana.log | cut -d '<' -f4` )
    # if ( ( $#ana_check ) && ( $log ) ) then
    #      echo $myname"$ana_check"
    #      Err_Log.pl -N ${EXPID}.j -I $ERROR_ID -X $ERROR_EXP -C 99 \
    #              -E 1 $ERROR_LOG_NAME \
    #              -D "${EXPID}.j WARNING: analyzer: $ana_check"
    # endif

    if [ $ANGLEBC -ne 0 ] && [ $DIAGTAR -ne 0 ]; then
        ls -l radstat
        $dryrun make_diagtarfile.csh $EXPID $nymdb $nhmsb radstat
        ls -l radstat
    fi

fi

# Handle aod analysis files when doing ensemble ADAS
if [ $GAAS_ANA -ne 0 ] && [ $HYBRIDGSI != "/dev/null" ]; then
    if [ ! -e $FVHOME/run/atmens_replay.acq ]; then
        notfound=1
        for aod_type in "aod_a.sfc" "aod_d.sfc" "aod_f.sfc" "aod_k.sfc"; do
            /bin/cp *.${aod_type}.*.$NCSUFFIX $STAGE4HYBGSI/
	        if [ $? -eq 0 ]; then
		        notfound=0
	        fi
	    done
        if [ $notfound -ne 0 ] && [ ! -f $FVHOME/run/gaas/GAAS.BOOTSTRAP ]; then
            echo "Cannot find AOD Analysis and related files. Aborting ..."
            exit 1
        fi
    fi
fi

# Signal successful completion
if [ $Final_ -ne 0 ]; then
    touch $DONE_FILE
fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
