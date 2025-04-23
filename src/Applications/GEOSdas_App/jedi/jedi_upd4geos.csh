#!/bin/csh -x

setenv MYNAME jedi_upd4geos.csh

if ( $#argv < 2 ) then
   echo " ${MYNAME}: invalid arg list, aborting"
   exit(1)
endif

set nymdb = $1
set nhmsb = $2
set hhb = `echo $nhmsb | cut -c1-2`
set yyyymmddhh = ${nymdb}${hhb}

setenv FAILED 0
if ( !($?EXPID)   )  setenv FAILED   1
if ( !($?FVWORK)  )  setenv FAILED   1
if ( !($?JEDI_VAROFFSET) ) setenv FAILED   1
if ( !($?JEDI_HYBRID)    ) setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

if ( -e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

set anadate  = `tick $nymdb $nhmsb $JEDI_VAROFFSET`
set nymda    = $anadate[1]
set nhmsa    = $anadate[2]

setenv JEDIWORK $FVWORK/jedi.$nymda.${nhmsa}

# Defaults
if ( !($?JEDI_IAU_OVERWRITE)   )  setenv JEDI_IAU_OVERWRITE   0

if ( -d $JEDIWORK/Data/iau ) then
   cd $FVWORK
   if ( $JEDI_HYBRID ) then
     foreach fn ( `ls $JEDIWORK/Data/iau/*.agcm_import_rst.*nc4` )
       set bname   = `basename $fn`
       set noexpid = `echo $bname | cut -d. -f2-`
       echo /bin/ln -sf $fn $noexpid
            /bin/ln -sf $fn $noexpid
     end
     /bin/ln -sf $JEDIWORK/Data/iau/$EXPID.agcm_import_rst.${nymdb}_${hhb}00z.nc4 agcm_import_rst
     echo " ${MYNAME}: overwrote GSI IAU increments with JEDI IAU increments. "
     ls -lrt  *agcm_import_rst*
   else
     @ nc = 0
     foreach fn ( `ls $JEDIWORK/Data/iau/*.agcm_import_rst.*nc4` )
        @ nc++
        if ( $nc == 1 ) then
          /bin/ln -sf $fn agcm_import_rst
        else
          echo " ${MYNAME}:  too many IAU tendencies found ..."
          exit (1)
        endif
     end
   endif
else
   echo " ${MYNAME}: JEDI IAU increments not found, no overwrite taken place. "
   if ( $JEDI_IAU_OVERWRITE ) then
      echo " ${MYNAME}: Aborting ..."
      exit (1)
   endif
endif

# If here, likely successful
# --------------------------
touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
