#!/bin/csh -x

# THIS IS TEMPORARY: should be replaced w/ better script
setenv MYNAME "mom6diag.csh"

if ( $#argv < 5 ) then
   echo " Usage: mom6diag.csh EXPID YYYYMMDD HHMMSS YYYYMMDD HHMMSS" 
   exit 1
endif

setenv FAILED 0
if ( !($?FVHOME) ) setenv FAILED 1
if ( !($?FVROOT) ) setenv FAILED 1
if ( !($?FVWORK) ) setenv FAILED 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set path = ( . $FVHOME/run $FVROOT/bin $path )

set expid   = $1
set begdate = $2
set begtime = $3
set enddate = $4
set endtime = $5

set bhh = `echo $begtime | cut -c1-2`
set ehh = `echo $endtime | cut -c1-2`

set begtag = ${begdate}_${bhh}z
set endtag = ${enddate}_${ehh}z

cd $FVWORK
foreach ftype ( prog_z forcing sfc_ave ) 
    if ( -e $ftype.nc ) then
      /bin/mv $ftype.nc $expid.$ftype.${begtag}+${endtag}.nc4
   else
      echo "${MYNAME}: trouble finding expected output $ftype.nc"
      exit 2
   endif
end
cd -
exit 0
