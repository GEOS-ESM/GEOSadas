#!/usr/bin/env bash

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

ThisScriptDir=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )
myname="UpdateGsiGridCompResource_3DVAR"

# Set environment
env_dir=$WORKFLOW_DIR/env # WORKFLOW_DIR is exported by ecf script
env_list="common ana obs"
for env_name in $env_list; do
    source $env_dir/${env_name}.sh
done

# Shorthands
CWD=$(pwd)
TIMEFILE=$FVWORK/simtimes.yaml

# Switch to FVWORK directory
cd $FVWORK
zeit_ci.x $myname

# Segment times
gcm_nymd0=$(grep ^gcm_nymd0: simtimes.yaml | cut -d":" -f2 | tr -d " ")
gcm_nhms0=$(grep ^gcm_nhms0: simtimes.yaml | cut -d":" -f2 | tr -d " ")
nymdb=$(grep ^nymdb: simtimes.yaml | cut -d":" -f2 | tr -d " ")
nhmsb=$(grep ^nhmsb: simtimes.yaml | cut -d":" -f2 | tr -d " ")

Viter_=0
Final_=1

varwindow_sec=$(($TIMEINC*60))
adtfwd=$varwindow_sec


beg_ana=($gcm_nymd0 $gcm_nhms0)
end_ana=($(tick ${beg_ana[@]} $adtfwd))


# Prepare GSI_GridComp.rc
/bin/rm -f sed_file
/bin/rm -f ./GSI_GridComp.rc

echo "s/>>>EXPID<<</${EXPID}/1"         > sed_file
echo "s/>>>IOBBKGD<<</${beg_ana[0]}/1" >> sed_file
echo "s/>>>IOBBKGT<<</${beg_ana[1]}/1" >> sed_file
echo "s/>>>IOEBKGD<<</${end_ana[0]}/1" >> sed_file
echo "s/>>>IOEBKGT<<</${end_ana[1]}/1" >> sed_file
echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> sed_file

echo "s/>>>ANADATE<<</${nymdb}/1"      >> sed_file
echo "s/>>>ANATIME<<</${nhmsb}/1"      >> sed_file
echo "s/>>>RECANA<<</YES/1"            >> sed_file
sed -f sed_file  ./GSI_GridComp.rc.tmpl  > ./GSI_GridComp.rc

# Append observation table to the gridcomp
append_gsigcrc.pl $FVWORK/obsys.rc GSI_GridComp.rc

cat GSI_GridComp.rc

# Extract analysis resolution
gsinlat=$(echorc.x "GSI JM" -rc GSI_GridComp.rc)
gsinlon=$(echorc.x "GSI IM" -rc GSI_GridComp.rc)
gsinlev=$(echorc.x "GSI LM" -rc GSI_GridComp.rc)

# Prepare main GSI resource file
/bin/rm -f sed_file
miter=$NVAROUTER # total num of iterations
echo "s/>>>MITER<<</$miter/g"            > sed_file    # maximum number of iteration
if [ $Viter_ -eq 0 ] || [ $Final_ -ne 0 ]; then
    echo "s/>>>GSIWRTDIAG<<</.true./g"  >> sed_file    # turn on diagnostics in GSI (omf/oma)
else
    echo "s/>>>GSIWRTDIAG<<</.false./g" >> sed_file    # turn off diagnostics during intermediate iterations
fi
if [ $Viter_ -eq 0 ]; then
    echo "s/>>>IGUESS<<</0/g"           >> sed_file     # in the 1st iteration, only write initial guess file
else
    echo "s/>>>IGUESS<<</1/g"           >> sed_file     # all other iterations, read and write initial guess file
fi
jiter=$(($Viter_+1))
echo "s/>>>JITERSTART<<</$jiter/g"      >> sed_file     # set current iteration

# Control aircraft bias correction
case "$ACFTBIAS" in
    0)
        echo "s/>>>AIRCFT_BIAS<<<//g"   >> sed_file
	echo 'Not using aircraft bias correction in GSI'
	;;
    1)
        echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc_ext=.true.,/g"  >> sed_file
	echo 'Setting aircraft_t_bc_ext to true, using external bias correction'
	;;
    2)
        echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc=.true.,/g"  >> sed_file
	echo 'Setting aircraft_t_bc to true, using VV.VV^2 bias correction'
	;;
    3)
        echo "s/>>>AIRCFT_BIAS<<</aircraft_t_bc_pof=.true.,/g"  >> sed_file
	echo 'Setting aircraft_t_bc_pof to true, using POF bias correction'
	;;
    *)
        echo "s/>>>AIRCFT_BIAS<<<//g"   >> sed_file
	echo 'Using default setting, not using aircraft bias correction in GSI'
	;;
esac

# Control satwnd source based on date ...
echo "beg_ana: ${beg_ana[@]}"
if [ ${beg_ana[0]} -lt 20100701 ]; then
        echo "s/>>>USE_PREPB_SATWND<<</.true./g"  >> sed_file
else
    echo "s/>>>USE_PREPB_SATWND<<</.false./g" >> sed_file
fi

/bin/rm -f ./gsi.rc
sed -f sed_file  ./gsi.rc.tmpl       > ./gsi.rc

cat gsi.rc

# All done
cd $FVWORK
zeit_co.x $myname

# Back to orig location
cd $CWD
