#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CoupleAnaToGcm_3DIAU"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common iau iau_mpi mpi"
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
Final_=1 # TODO: has not been used

# Segment times
gcm_nymd0=$(grep ^gcm_nymd0: simtimes.yaml | cut -d":" -f2 | tr -d " ")
gcm_nhms0=$(grep ^gcm_nhms0: simtimes.yaml | cut -d":" -f2 | tr -d " ")
nymdb=$(grep ^nymdb: simtimes.yaml | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep ^nhmsb: simtimes.yaml | cut -d":" -f2 | tr -d " ")

beg_ana=($gcm_nymd0 $gcm_nhms0)

ddz=${beg_ana[0]}
hhz=$(echo ${beg_ana[1]} | cut -c1-2)

DONE_FILE=$FVWORK/.DONE_MEM001_CoupleAnaToGcm_${Viter_}.${ddz}${hhz}

if [ -e $DONE_FILE ]; then
    exit 0
fi

# QUESTION: What's the point of DOETA2BIN

$FVRUN/lnbcs $nymdb # QUESTION: again? a different date?

# Convert ana "eta" files to rst

bkgfile=$(echorc.x -template $EXPID $nymdb $nhmsb upper-air_bkg_filename)
anafile=$(echorc.x -template $EXPID $nymdb $nhmsb upper-air_ana_filename)
incfile=$(echorc.x -template $EXPID $nymdb $nhmsb upper-air_inc_filename)
bkg06upa=$(echorc.x -template $EXPID $nymdb $nhmsb upper-air_bkg06_filename)
if [ -e $bkg06upa ]; then
    bkg4iau=$bkg06upa
else
    bkg4iau=$bkgfile
fi

if [ -e mkiau.rc.tmpl ]; then
    /bin/rm -f sed_file
    echo "s/>>>EXPID<<</${EXPID}/1"         > sed_file
    echo "s/>>>BKGFNAME<<</${bkg4iau}/1"   >> sed_file
    echo "s/>>>ANAFNAME<<</${anafile}/1"   >> sed_file
    echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> sed_file
    echo "s/>>>ANADATE<<</${nymdb}/1"      >> sed_file
    echo "s/>>>ANATIME<<</${nhmsb}/1"      >> sed_file
    /bin/rm -f ./mkiau.rc
    sed -f sed_file ./mkiau.rc.tmpl > ./mkiau.rc
    # run mkiau.x
    $MPIRUN_IAU
else
    imout=$(grep AGCM_IM: AGCM.rc.tmpl | cut -d : -f2)
    jmout=$(grep AGCM_JM: AGCM.rc.tmpl | cut -d : -f2)
    # run mkiau.x
    $MPIRUN_IAU -ana $anafile -bkg $bkg4iau -imout $imout -jmout $jmout \
        -divr -iau agcm_import_rst -nymd $nymdb -nhms $nhmsb
fi

if [ -e IAU_EGRESS ]; then
    echo "IAU increment generated"
    /bin/rm -f IAU_EGRESS  # clean up and move on
else
    echo "Failed to generate IAU increment"
    exit 1
fi

gcm_nhr0=$(echo $gcm_nhms0 | cut -c1-2)
/bin/cp -f agcm_import_rst ${EXPID}.agcm_import_rst.${gcm_nymd0}_${gcm_nhr0}z.bin

# Create increment file to allow for multiple options of running DAS
dyn_recenter.x -g5 $anafile $bkg4iau NONE -o $incfile

touch $DONE_FILE

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
