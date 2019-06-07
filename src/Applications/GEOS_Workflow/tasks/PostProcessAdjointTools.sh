#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="PostProcessAdjointTools"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common adj"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

RenameFile_(){
    # Input argument: $file
    if [ $# -ne 1 ]; then
        echo -n "Usage: RenameFile_ <File>"
        return 1
    fi
    local infile_ fname fext vtime bname

    infile_=$1
    fext=${infile_##*.}  # file extension
    fname=${infile_%.*}  # file name
    vtime=${fname##*.}
    bname=${fname%.*}

    /bin/mv $infile_ $bname.$TIME0+$vtime.$fext
}

# Initial- and final-time singular vector files
# ---------------------------------------------
if [ -e fvsvec.rc ] || [ -e initadj.rc ] || [ -e oseledec.rc ]; then

    echo "WARNING: ftypes isvec.eta fsvec.eta icnop.eta fcnop.eta are not getting renamed"
    # TODO: Translate csh -> bash
    # for ftype in isvec.eta fsvec.eta icnop.eta fcnop.eta; do
    #     files=$(/bin/ls -1 $EXPID.$ftype*.$NCSUFFIX)
    #     if [ !($status) ]; then        # at least 1 file of this type
    #         for file in $files; do
    #             set vtime = $file:r  # verification time
    #             set vtime = $vtime:r # verification time
    #             set vtime = $vtime:e # verification time
    #             set vtype = $file:r  # verification time
    #             set vtype = $vtype:e # verification time
    #             set bname = $file:r  # base name $expid.prog.eta
    #             set bname = $bname:r # base name $expid.prog.eta
    #             set bname = $bname:r # base name $expid.prog.eta
    #             /bin/mv $file $bname.$itime+$vtime.$vtype.$NCSUFFIX
    # 	    done
    # 	fi
    # done

    # Rename nc4 files
    ncFileTypes=(
	Jgradf_ Jgradfainc_
	itraj.lcv ftraj.lcv finc.eta
    )
    if [ $grsv -eq 0 ]; then # grsv comes from sourcing adj_env.sh
	ncFileTypes+=(fsens_ fsensainc_ EMferr_)
    fi
    for ftype in ${ncFileTypes[@]}; do
        files=$(/bin/ls -1 $EXPID.${ftype}*.$NCSUFFIX) || true # suppress error
        if [ $? -eq 0 ]; then # at least 1 file of this type
            for file in $files; do
		RenameFile_ $file
	    done
        fi
    done

    # Rename txt files
    txtFileTypes=(Jnormf EMferrnorm Xdot)
    for ftype in ${txtFileTypes[@]}; do
        files=$(/bin/ls -1 $EXPID.${ftype}*.txt) || true # suppress error
        if [ $? -eq 0 ]; then # at least 1 file of this type
            for file in $files; do
		RenameFile_ $file
	    done
        fi
    done

fi


# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
