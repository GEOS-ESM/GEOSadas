#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="CopyAnaRestarts"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana iau"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
FVRUN="$FVHOME/run"
CWD=$(pwd) # current location
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ $DO4DVAR -ne 0 ]; then
    echo "$myname: 4DVAR-specific code not implemented yet"
    exit 1
fi

nbkgfiles=$(($TIMEINC/$ASYNBKG+1))
if [ "$DOIAU" -eq 0 ]; then
    nbkgfiles=1
fi
nbkgtotal=$((2*$nbkgfiles))

upa_brs_list=($(ls $FVHOME/recycle/$EXPID.bkg*eta*.$NCSUFFIX))
sfc_brs_list=($(ls $FVHOME/recycle/$EXPID.bkg*sfc*.$NCSUFFIX))

if  [ ! -e $FVRUN/replay.acq ] && \
    [ ${#upa_brs_list[@]} -lt $nbkgfiles ] && \
    [ ${#sfc_brs_list[@]} -lt $nbkgfiles ]; then
    echo "$myname: cannot acquire bkg files from $FVHOME/recycle/"
    exit 90
fi

# if [ $DO4DVAR -ne 0 ]; then
#     thisrc=GSI_GridComp_1.rc.tmpl
# else
    thisrc=GSI_GridComp.rc.tmpl
# fi

# Check for need of chemistry bkg file (any date below)
dummy=$(echorc.x -rc $thisrc -template $EXPID 17760704 00000 chem_bkg_filename)
if ! [ $dummy == "NONE" ]; then #TODO: Check w/ Ricardo if NONE is right
    chm_brs_list=($(ls $FVHOME/recycle/$EXPID.cbkg*eta*.$NCSUFFIX))
    if  [ ! -e $FVRUN/replay.acq ] && [ ${#chm_brs_list[@]} -lt $nbkgfiles ]; then
        echo $myname": cannot acquire chem bkg files from $FVHOME/recycle/"
        exit 90
    fi
fi

# Check for need of aerosols bkg file (any date below)
dummy=$(echorc.x -rc $thisrc -template $EXPID 17760704 00000 aero_bkg_filename)
if [ "$dummy" != "NONE" ]; then #TODO: Check w/ Ricardo if NONE is right
    aer_brs_list=($(ls $FVHOME/recycle/$EXPID.abkg*eta*.$NCSUFFIX))
    if  [ ! -e $FVRUN/replay.acq ] && \
	[ ${#aer_brs_list[@]} -lt $nbkgfiles ]; then
        echo $myname": cannot acquire aero bkg files from $FVHOME/recycle/"
        exit 90
    fi
fi

# satbias/satbang
ars_list=(satbias satbang)
if [ $NEWRADBC -ne 0 ] || [ $ANGLEBC -ne 0 ]; then
    ars_list=(${ars_list[@]} satbiaspc)
fi
if [ $ACFTBIAS -ne 0 ]; then
    ars_list=(${ars_list[@]} acftbias)
fi
for rs in ${ars_list[@]}; do
    /bin/cp $FVHOME/recycle/$EXPID.ana_${rs}_rst.*.txt $rs
    # TODO: Double check the logic in the following if statement
    if [ $? -ne 0 ]; then
	echo $myname": cannot acquire $FVHOME/recycle/$EXPID.ana_${rs}_rst"
        exit 90
    fi
done

if [ $ANGLEBC -ne 0 ] && [ $DIAGTR -ne 0 ]; then
    /bin/cp $FVHOME/recycle/$EXPID.ana_radstat_rst.*.tar radstat
fi

# GAAS restart files
# if [ $DO4DVAR -eq 0 ]; then
if [ $GAAS_ANA -ne 0 ] && [ ! -e $FVRUN/gaas/GAAS.BOOTSTRAP ]; then
    gaas_brs_list=($(ls $FVHOME/recycle/$EXPID.gaas_bkg_sfc_rst*.$NCSUFFIX))
    # TODO: Double check logic of this if statement
    if [ $? -ne 0 ] || [ ${#gaas_brs_list[@]} -ne 2 ]; then
        echo $myname": cannot acquire $FVHOME/recycle/$EXPID.gaas_bkg_sfc_rst"
        exit 90
    fi
    for rs in ${gaas_brs_list[@]}; do
        fvdate=($(fndate.pl $rs))
        bkgfile=$(echorc.x -template $EXPID ${fvdate[0]} ${fvdate[1]} gaas_bkg_filename)
        /bin/cp $rs $bkgfile
        if [ $? -ne 0 ]; then
	    echo $myname": failed to copy $FVHOME/recycle/$EXPID.gaas_bkg_sfc_rst"
	    exit 90
        fi
    done
fi
# fi # <DO4DVAR>

# Copy restarts
for rs in ${sfc_brs_list[@]}; do
    fvdate=($(fndate.pl $rs))
    bkgfile=$(echorc.x -template $EXPID ${fvdate[0]} ${fvdate[1]} surface_bkg_filename)
    /bin/cp ${rs} $bkgfile &
done
for rs in ${upa_brs_list[@]}; do
    fvdate=($(fndate.pl $rs))
    bkgfile=$(echorc.x -template $EXPID ${fvdate[0]} ${fvdate[1]} upper-air_bkg_filename)
    /bin/cp ${rs} $bkgfile &
done
if [ ! -z ${chm_brs_list+x} ]; then  # chm_brs_list exists
    for rs in ${chm_brs_list[@]}; do
	fvdate=($(fndate.pl $rs))
	bkgfile=$(echorc.x -template $EXPID ${fvdate[0]} ${fvdate[1]} chem_bkg_filename)
	/bin/cp ${rs} $bkgfile &
    done
fi
if [ ! -z ${aer_brs_list+x} ]; then  # aer_brs_list exists
    for rs in ${aer_brs_list[@]}; do
	fvdate=($(fndate.pl $rs))
	bkgfile=$(echorc.x -template $EXPID ${fvdate[0]} ${fvdate[1]} aero_bkg_filename)
	/bin/cp ${rs} $bkgfile &
    done
fi
wait

# initial time
TIMEFILE=$FVWORK/simtimes.yaml
gcm_nymd0=$(grep "^gcm_nymd0:" $TIMEFILE | cut -d":" -f2 | tr -d " ")
gcm_nhr0=$(grep "^gcm_nhms0:" simtimes.yaml | cut -d":" -f2 | tr -d " " | cut -c1-2)
itime=${gcm_nymd0}_${gcm_nhr0}z

if [ -e $FVHOME/recycle/$EXPID.biasinp_rst.$itime.ctl ]; then
    /bin/cp $FVHOME/recycle/$EXPID.biasinp_rst.$itime.ctl biasinp.ctl &
fi

if [ -e $FVHOME/recycle/$EXPID.biasinp_rst.$itime.$RSTSUFFIX ]; then
    /bin/cp $FVHOME/recycle/$EXPID.biasinp_rst.$itime.$RSTSUFFIX biasinp.$RSTSUFFIX &
fi

brs_list=($(ls $EXPID.bkg*.$NCSUFFIX)) # redefine bkg rst list

if [ ! -e $FVHOME/run/replay.acq ] && [ ${#brs_list[@]} -lt $nbkgtotal ]; then
    echo $myname": cannot acquire background files from $FVHOME/recycle/"
    exit 90
fi

if [ -e $FVHOME/recycle/$EXPID.trak.GDA.rst.*.txt ]; then
    gda_file=$(ls $FVHOME/recycle/$EXPID.trak.GDA.rst.*.txt)
    /bin/cp $gda_file  .
    trksufx=$(echo $(basename $gda_file) | cut -d. -f5,6)
    /bin/mv $EXPID.trak.GDA.rst.*.txt $EXPID.trak.GDA.all.$trksufx
    if [ $? -ne 0 ] || [ ! -e $EXPID.trak.GDA.all.$trksufx ] || [ -z $EXPID.trak.GDA.all.$trksufx ]; then
        echo $myname": cannot acquire $gda_file"
        exit 90
    fi
fi

# if [ $DO4DVAR -ne 0 ]; then # no need to refer to traj otherwise
#     trjdate=($(rst_date d_rst))
#     trajfile1=$(echorc.x -template $EXPID ${trjdate[0]} ${trjdate[1]} uparst_traj_filename)
#     trajfile2=$(echorc.x -template $EXPID ${trjdate[0]} ${trjdate[1]} upalcv_traj_filename)
#     /bin/cp $FVHOME/recycle/$trajfile1 $trajfile2
#     # ptrjfile1=$(echorc.x -template $EXPID ${trjdate[0]} ${trjdate[1]} uparst_ptrj_filename)
#     # ptrjfile2=$(echorc.x -template $EXPID ${trjdate[0]} ${trjdate[1]} upaprs_traj_filename)
#     # /bin/cp $FVHOME/recycle/$ptrjfile1  $ptrjfile2
# fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
