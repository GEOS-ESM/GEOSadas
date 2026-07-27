#!/bin/sh

#-----------------------------------------------
yyyy=2015
mm=08
dd=21
hh=00
#-----------------------------------------------

# GNSSRO BUFR directory
#export gpsro_bufr=/discover/nobackup/projects/gmao/input/dao_ops/ops/reanalysis/GPSRO/bufr/COSMIC
export gpsro_bufr=/discover/nobackup/projects/gmao/input/dao_ops/ops/flk/ncep_g5obs/bufr/GPSRO
# JEDI build  (Dan's)
export build=/discover/nobackup/drholdaw/JediDev/develop/build-intel-release
#source modules
source modules_updated
## excutable to Transfer BUFR to IODA NetCDF
export bufr2nc_x=${build}/bin/bufr2nc_fortran.x 

export locdir=`pwd`
[ ! -d testinput ]  && mkdir testinput
[ ! -d testrun ]  && mkdir testrun
#[ ! -d testinput_2024 ]  && mkdir testinput_2024
#[ ! -d testrun_2024 ]  && mkdir testrun_2024

cd ${locdir}/testinput
#cd ${locdir}/testinput_2024
   yy=`echo $yyyy |cut -b3-4`
   filename=gdas1_modified.${yyyy}${mm}${dd}.t${hh}z.gpsro.tm00
#   filename=gdas1.${yy}${mm}${dd}.t${hh}z.gpsro.tm00
   ln -s ${gpsro_bufr}/Y$yyyy/M$mm/${filename}.bufr_d  ${filename}.bufr
cd ${locdir}
   $bufr2nc_x -i testinput -o testrun ${filename}.bufr 
#   $bufr2nc_x -i testinput_2024 -o testrun_2024 ${filename}.bufr 
