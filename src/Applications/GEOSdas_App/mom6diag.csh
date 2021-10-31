#!/bin/csh

# THIS IS TEMPORARY: should be replaced w/ better script

if ( $#argv < 5 ) then
   echo " Usage: mom6diag.csh EXPID YYYYMMDD HHMMSS YYYYMMDD HHMMSS" 
   exit 1
endif

set expid   = $1
set begdate = $2
set begtime = $3
set enddate = $4
set endtime = $5

set bhh = `echo $begtime | cut -c1-2``
set ehh = `echo $endtime | cut -c1-2``

set begtag = ${begdate}_${bhh}z
set endtag = ${enddate}_${bhh}z

foreach ftype ( prog_z forcing sfc_ave ) 
   /bin/mv $ftype.nc $expid.${begtag}+${endtag}.nc4
end
