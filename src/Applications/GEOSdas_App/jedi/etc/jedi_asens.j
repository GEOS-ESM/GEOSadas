#!/bin/csh -fx
#SBATCH --account=$GID
@GEOSJEDI_QOS
@GEOSJEDI_PARTITION
#SBATCH --job-name=jasens
#SBATCH --output=jasens.log.o%j.txt
#SBATCH --ntasks=216
#SBATCH --constraint=mil
#SBATCH --time=2:00:00

#######################################################
#######################################################
######                                           ######
######   CAUTION: NOT READY YET                  ######
######                                           ######
#######################################################
#######################################################

setenv EXPID $EXPID
setenv FVHOME $FVHOME
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv JEDI_RUN_ADANA 1

setenv JFSENSLOC /discover/nobackup/projects/gmao/dadev/rtodling/Debug/Convert/4JEDI

setenv GID $GID
setenv BATCH_SUBCMD sbatch

if ( ! -d $FVHOME/run/jedi ) then
   echo "No JEDI settings have been found within GEOS, abort"
   exit 1
endif

set path = ( . $FVHOME/run/jedi $FVHOME/run $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

cd $JFSENSLOC
if ( $?this_nymd ) then
   set fsenslst = `ls $EXPID.jfsens_twe.eta.????????_??z+????????_??z-${this_nymd}_00z.nc4`
else
   set fsenslst = `ls *jfsens*nc4`
endif
echo "Will work on the following forecast sensitivities:"
echo $fsenslst
echo " "

cd -

setenv FVWORK /discover/nobackup/projects/gmao/obsdev/rtodling/jediwork/tmpjasens.$$
setenv HOLDRESULTS $FVWORK/Results
mkdir -p $FVWORK
mkdir -p $HOLDRESULTS
cd $FVWORK

foreach sensfn ( $fsenslst )

  echo $sensfn
  set ttag = `echo $sensfn | cut -d. -f4 | cut -d- -f2`
  echo $ttag
  set nymda = `echo $ttag | cut -c1-8`
  set hha   = `echo $ttag | cut -c10-11`
  set nhmsa = ${hha}0000

  /bin/cp $JFSENSLOC/$sensfn jedi.fsens.eta.nc4

  set bdatetime = ( `tick $nymda $nhmsa -10800` )

  if ( -d $FVWORK/jana ) /bin/rm -r $FVWORK/jana

  jedi_driver.csh $bdatetime[1] $bdatetime[2] $nymda $nhmsa |& tee -a $FVWORK/$EXPID.jedi_asens.log.${nymda}_${hha}z.txt

  /bin/mv *.tar *.txt $HOLDRESULTS/

end
# /bin/rm $FVWORK
cd -
