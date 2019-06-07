#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CreateWorkspace"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana" # ana is needed for HYBRIDGSI
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Create FVWORK directory
if [ -d $FVWORK ]; then
    echo "FVWORK [$FVWORK] already exists. Aborting..."
    exit 1
fi
/bin/mkdir -p $FVWORK
if [ -e /usr/bin/lfs ]; then
    /usr/bin/lfs setstripe -c 1 -s 1048576 -i -1 $FVWORK
fi

# Create ExtData
EXTDATA=${FVWORK}/ExtData
/bin/mkdir -p $EXTDATA
/bin/touch $EXTDATA/.no_archiving
/bin/rm -f $EXTDATA/agcmpert
/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/agcmpert $EXTDATA/
/bin/rm -f $EXTDATA/g5chem
/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/g5chem $EXTDATA/
/bin/rm -f $EXTDATA/g5gcm
/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/g5gcm $EXTDATA/
/bin/rm -f $EXTDATA/PIESA
/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/PIESA $EXTDATA/
/bin/rm -f $EXTDATA/MERRA2
/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/MERRA2 $EXTDATA/
/bin/rm -f $EXTDATA/AeroCom
/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/AeroCom $EXTDATA/

# Create $FVHOME/atmens/central and $FVHOME/atmens/rs for hybrid runs
if [ "$HYBRIDGSI" != "/dev/null" ] && [ ! -e $FVHOME/run/atmens_replay.acq ]; then
    touch $HYBRIDGSI/.no_archiving
    /bin/mkdir -p $STAGE4HYBGSI
    /bin/mkdir -p $RSTSTAGE4AENS
fi

