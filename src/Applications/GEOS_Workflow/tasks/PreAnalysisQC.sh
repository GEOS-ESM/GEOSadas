#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="PreAnalysisQC"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd) # current location

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ ! -e obsys.acq ]; then
    echo "Could not find obsys.acq in $FVWORK. Aborting..."
    exit 1
fi
acq="obsys.acq"

# Copy acquire rc file to run directory
/bin/cp $acq  ${FVHOME}/run/obsys.acq.latest

# Check if pre-qc file are present in acquire rc file
if [ $(grep -c pre-qc $acq) -ne 0 ]; then
    # Remove hard-wired times
    grep pre-qc $acq | grep -v ^\# | grep "%h2" | cut -d\> -f2 > pre-qc_acq
    grep pre-qc $acq | grep -v ^\# | grep -v "%h2" | sed 's/\.t..z\./.t%h2z./' | cut -d\> -f2 >> pre-qc_acq

    # Remove duplicate entries
    cat pre-qc_acq | sort | uniq > pre-qc.acq

    # Check for pre-existing prepbufr file
    ls *.prepbufr.*

    # Prepare obsys file for lnlist processing
    if [ $? -ne 0 ] && [ -z pre-qc.acq ] && [ -e pre-qc.acq ]; then
        /bin/mv $acq obsys.acq.orig
        grep -v pre-qc obsys.acq.orig | grep -v ^\# | sort | uniq > obsys_acq
        echo "/this/is/a/dummy/path =>  $EXPID.prepbufr.%y4%m2%d2.t%h2z.blk" >> obsys_acq
        /bin/mv ${FVHOME}/run/obsys.acq.latest ${FVHOME}/run/obsys.acq.raw
        /bin/cp obsys_acq  ${FVHOME}/run/obsys.acq.latest
    fi
else
    cat $acq | grep -v ^\# | sort | uniq > obsys_acq
fi

# Fix unblocked files
zeit_ci.x FixUnblocked
for ublk in $(ls *.ublk)
do
    FixUnblocked.csh $ublk &
done
wait
if [ -e FAILED_BLOCK ]; then
    exit 96
fi
zeit_co.x FixUnblocked

# Flip QM=9 flag for ASCAT in prepbufr
if [ ! -z ${USE_ASCAT+x} ]; then
    echo "Will flip ASCAT flag in prepbufr."
    for prepbf in $(ls *prepbuf*.ublk)
    do
	fix_ascat.x $prepbf ${prepbf}_ascat
	/bin/ls -l $prepbf ${prepbf}_ascat
	/bin/mv ${prepbf}_ascat $prepbf
    done
fi

# Fix big/little endian in the upa-buffer files
zeit_ci.x FixEndian
arch=$(uname -m)
if [ "$arch" == "ia64" ] || [ "$arch" == "x86_64" ]; then
    b64=1
else
    b64=0
fi
myOS=$(uname -s)
if [ "$myOS" == "OSF1" ] || ([ "$myOS" == "Linux" ] && [ $b64 -eq 1 ]); then
    for upa in $(ls *upabufr*bfr)
    do
	Reblock.csh $upa &
    done
    for blk in $(ls *.blk)
    do
	Reblock.csh $blk &
    done
fi
wait
if [ -e FAILED_BLOCK ]; then
    exit 96
fi
zeit_co.x FixEndian

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
