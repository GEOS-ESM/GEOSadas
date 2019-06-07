#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="TagAndCopyForecastOutput"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common adj finalize"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
fcstage=$FVHOME/fcst/stage

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

if [ $STAGE4FCST -eq 0 ] && [ $(grep -c "@" ./saverst.rc) -eq 0 ]; then
    fcst_times=$(cat ./saverst.rc)
    grs_list=($(grs_list.pl -rc ./AGCM.rc))
    for ftime in $fcst_times; do
	agcm_import=""

	if [ $AGCMFCST -ne 0 ] && [ $(($ftime%2)) -ne 0 ]; then
	    agcm_import="agcm_import"
	fi

        /bin/cp -f $EXPID.rst.lcv.*_${ftime}z.bin $fcstage &
        /bin/cp -f $EXPID.traj_lcv_rst.????????_${ftime}00z.$NCSUFFIX $fcstage &
	#/bin/cp -f $EXPID.mtrj_lcv_rst.????????_${ftime}00z.$NCSUFFIX $fcstage &
	#/bin/cp -f $EXPID.ptrj_prs_rst.????????_${ftime}00z.$NCSUFFIX $fcstage &

	rstlist=(${grs_list[@]} biasinp $agcm_import)
	for restart in ${rstlist[@]}; do
	    /bin/cp -f $EXPID.${restart}_rst.????????_${ftime}z.??? $fcstage &
	done
    done
    /bin/ls -l $fcstage
    wait
fi

# TODO: Check for HREPACK and wait for it to finish

# NOTE: TIME0 is an ecflow variable exported by the ecf file

# Forecasting files need extra tagging for archiving
vortex=0 # We don't really need this, do we??
allftypes=$(read_HIST.csh ./HISTORY.rc)
for ftype in $allftypes; do
    # Special handling for repacked files
    ftype=$(echo $ftype | sed -e 's/_unz//')
    files=$(/bin/ls -1 ${ftype}*.$NCSUFFIX) || true # do not exit on error
    if [ $? -eq 0 ]; then # at least one file of this type
	for file in $files; do
	    # Special handling for repacked files
	    # TODO: Check the output of this if statement
	    if [[ "$file" =~ *.fana.eta.* ]] ||
		   [[ "$file" =~ *.${TIME0}+* ]] ||
		   [[ "$file" =~ *+-* ]]; then
		echo "Not renaming file: $file"
	    else
		# e.g. expid.prog.eta.yyyymmdd_hhz.nc4
		noext=${file%.*}   # expid.prog.eta.yyyymmdd_hhz
		vtime=${noext##*.} # yymmdd_hhz
		bname=${noext%.*}  # expid.prog.eta
		nname="$bname.$TIME0+$vtime.$NCSUFFIX"
		/bin/mv $file $nname
	    fi
	done
    fi
done
wait # Do we really need this wait?

# Copy gradient vectors for later analysis sensitivity runs
# grsv==0 => do not copy gradient vectors
if [ $grsv -ne 0 ]; then
    grfiles=$(/bin/ls *fsens_*.eta* *fsensainc_*.eta*) || true # suppress error
    for grfile in $grfiles; do
	noext=${grfile%.*}
	vtime=${noext##*.}
	bname=${noext%.*}
	nname="$bname.$TIME0+$vtime.$NCSUFFIX"
	/bin/mv -f $grfile $nname
	/bin/cp -f $nname  $GRSTAGE &
    done
fi
wait

# # TODO: This should be a separate post-processing task
# # Convert prog.eta output w/ lcv2prs.x
# if [ $CONVPROG -ne 0 ]; then
#     zeit_ci.x cnv2prs
#     cnv2prs.pl -prog
#     zeit_co.x cnv2prs
# fi

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
