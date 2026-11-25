#!/bin/csh -f
#
# swell_upd4geos.csh - make the GEOS model use the JEDI IAU forcing of this cycle
#
# usage: swell_upd4geos.csh nymdb nhmsb nymda nhmsa
#
# Links $JEDIWORK/iau/$EXPID.agcm_import_rst.<date>.nc4 into $FVWORK as
# agcm_import_rst.<date>.nc4 and agcm_import_rst (replacing the GSI IAU).
# Port of jedi/jedi_upd4geos.csh.
# ---------------------------------------------------------------------------

set myname = swell_upd4geos.csh

if ( $#argv < 4 ) then
   echo "usage: $myname nymdb nhmsb nymda nhmsa"
   exit 1
endif

set nymdb = $1
set nhmsb = `echo $2 | awk '{printf "%06d", $1}'`
set nymda = $3
set nhmsa = `echo $4 | awk '{printf "%06d", $1}'`
set hhb   = `echo $nhmsb | cut -c1-2`
set hhmmb = `echo $nhmsb | cut -c1-4`
set yyyymmddhh = ${nymdb}${hhb}

if ( ! $?EXPID || ! $?FVWORK ) then
   echo "${myname}: EXPID and FVWORK must be defined, aborting"
   exit 1
endif

set donefile = $FVWORK/.DONE_${myname}.$yyyymmddhh
if ( -e $donefile ) then
   echo "${myname}: already done"
   exit 0
endif

set iaudir = $FVWORK/jedi.${nymda}.${nhmsa}/iau
set iaufn  = $iaudir/$EXPID.agcm_import_rst.${nymdb}_${hhmmb}z.nc4
if ( ! -e $iaufn ) then
   echo "${myname}: $iaufn not found, aborting"
   exit 1
endif

cd $FVWORK
foreach fn ( `find -H $iaudir -maxdepth 1 -name "$EXPID.agcm_import_rst.*.nc4"` )
   set noexpid = `basename $fn | cut -d. -f2-`
   /bin/ln -sf $fn $noexpid
end
/bin/ln -sf $iaufn agcm_import_rst
echo "${myname}: GEOS will use the JEDI IAU forcing $iaufn"
ls -l *agcm_import_rst*

touch $donefile
echo "${myname}: Complete"
exit 0
