#!/bin/csh -f

setenv MYNAME "atmens_rst_regrid.csh"

@ nmax = $#argv

# Usage
# -----
if( $nmax == 0 ) then
    echo " "
    echo " \begin{verbatim}"
    echo " "
    echo "Usage -- To regrid Non-MERRA restarts:"
    echo "--------------------------------------"
    echo "$MYNAME   -rc    REGRIDRC  "
    echo "          -out   DIRECTORY "
    echo "          -indir INPUTDIR  "
    echo "          -nodyn           "
    echo " "
    echo "     where:     REGRIDRC  is the full pathname of a user-defined regrid.rc file"
    echo "                DIRECTORY is the full pathname of the desired OUTPUT location"
    echo "                INPUTDIR  is the full where the input RSTs reside "
    echo "                          (optional; other path in rc file)"
    echo "                -nodyn    is optional, allowing bypass of dynamics rst regrid"
    echo " "
    echo " \end{verbatim}"
    echo " \clearpage"
    exit 0
endif

# Process Input String
# --------------------
set regridrc = NULL
set nymd     = NULL
set im       = NULL
set jm       = NULL
set OUTDIR   = NULL
set INDIR    = NULL
set DODYN    = 1

setenv FAILED 0
if ( !($?FVROOT)  ) setenv FAILED 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

@       n  = 1
while( $n <= $nmax )

       if( "$argv[$n]" == "-rc" ) then
                  @  n  = $n + 1
            set regridrc = $argv[$n]
       endif
       if( "$argv[$n]" == "-nymd" ) then
                  @  n  = $n + 1
            set nymd = $argv[$n]
       endif
       if( "$argv[$n]" == "-im" ) then
                  @  n  = $n + 1
            set im = $argv[$n]
       endif
       if( "$argv[$n]" == "-jm" ) then
                  @  n  = $n + 1
            set jm = $argv[$n]
       endif
       if( "$argv[$n]" == "-out" ) then
                  @  n  = $n + 1
            set OUTDIR = $argv[$n]
       endif
       if( "$argv[$n]" == "-indir" ) then
                  @  n  = $n + 1
            set INDIR  = $argv[$n]
       endif
       if( "$argv[$n]" == "-nodyn" ) then
            set DODYN = 0
       endif

     @ n = $n + 1
end

if( $regridrc != NULL && $nymd != NULL ) then
     echo " "
     echo You must only choose ONE method:  -rc  or -nymd \!
     echo " "
     exit
endif
if( $nymd != NULL && ( $im == NULL | $jm == NULL) ) then
     echo " "
     echo You must set Horizontal Dimension: IM \& JM \!
     echo " "
     exit
endif
if( $OUTDIR == NULL ) then
     echo " "
     echo You must define an OUTPUT directory \!
     echo " "
     exit
endif

set datetime = `date +%Y%m%d_%H%M%S`
set OUTPUT   = $OUTDIR/$datetime
if(! -e $OUTPUT ) mkdir -p $OUTPUT


# Find location of regrid utility
# -------------------------------
#set REGRID = `which $0` 
#set root = `echo $REGRID | cut -d / -f1`
#if($root == . ) set REGRID = `echo $cwd`/regrid

# Set GEOS Directories
# --------------------
#@ n = 1
#set root = `echo $REGRID | cut -d"/" -f$n`
#while( .$root == . )
#@ n = $n + 1   
#set root = `echo $REGRID | cut -d"/" -f$n`
#end
   
#setenv ESMADIR ''
#while( $root != 'src' )
#setenv ESMADIR `echo ${ESMADIR}/${root}`
#@ n = $n + 1
#set root = `echo $REGRID | cut -d / -f$n`
#end

set GEOSSRC  = $FVROOT/../src
# the following is a temporary hack
set GEOSUTIL = $GEOSSRC/GMAO_Shared/GEOS_Util
set  MK_RST  = $GEOSSRC/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSsurface_GridComp/GEOSland_GridComp/GEOScatch_GridComp/mk_restarts


# Read regrid.rc variables
# ------------------------
set BC_TAG  = `grep BC_TAG: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set IM_OLD  = `grep IM_OLD: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set JM_OLD  = `grep JM_OLD: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set DYN     = `grep DYN_INTERNAL_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set MOIST   = `grep MOIST_INTERNAL_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set CATCH   = `grep CATCH_INTERNAL_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set LAKE    = `grep  LAKE_INTERNAL_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set LANDICE = `grep LANDICE_INTERNAL_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set SALT    = `grep SALTWATER_INTERNAL_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set AGCM    = `grep AGCM_IMPORT_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set PCHEM   = `grep PCHEM_INTERNAL_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set GOCART  = `grep GOCART_INTERNAL_RESTART_FILE: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set IM_NEW  = `grep IM_NEW: $regridrc | cut -d'#' -f1 | cut -d':' -f2`
set JM_NEW  = `grep JM_NEW: $regridrc | cut -d'#' -f1 | cut -d':' -f2`

# Check for completeness
# ----------------------
set NOGO = FALSE
if( .$BC_TAG == . ) then
     echo You must supply the INPUT boundary condition tag: BC_TAG
     set NOGO = TRUE
endif
if( .$IM_OLD == . ) then
     echo You must supply the INPUT zonal dimension: IM_OLD
     set NOGO = TRUE
endif
if( .$JM_OLD == . ) then
     echo You must supply the INPUT meridional dimension: JM_OLD
     set NOGO = TRUE
endif
if( .$DYN == . && $DODYN ) then
     echo You must supply the INPUT:  DYN_INTERNAL_RESTART_FILE
     set NOGO = TRUE
endif
if( .$MOIST == . && $DODYN ) then
     echo You must supply the INPUT:  MOIST_INTERNAL_RESTART_FILE
     set NOGO = TRUE
endif
if( .$CATCH == . ) then
     echo You must supply the INPUT:  CATCH_INTERNAL_RESTART_FILE
     set NOGO = TRUE
endif
if( .$LAKE == . ) then
     echo You must supply the INPUT:  LAKE_INTERNAL_RESTART_FILE
     set NOGO = TRUE
endif
if( .$LANDICE == . ) then
     echo You must supply the INPUT:  LANDICE_INTERNAL_RESTART_FILE
     set NOGO = TRUE
endif
if( .$SALT == . ) then
     echo You must supply the INPUT:  SALT_INTERNAL_RESTART_FILE
     set NOGO = TRUE
endif
if( .$IM_NEW == . ) then
     echo You must supply the OUTPUT zonal dimension: IM_NEW
     set NOGO = TRUE
endif
if( .$JM_NEW == . ) then
     echo You must supply the OUTPUT meridional dimension: JM_NEW
     set NOGO = TRUE
endif

if( $NOGO == TRUE ) exit

set RSLV_OLD = ${IM_OLD}x${JM_OLD}
set RSLV_NEW = ${IM_NEW}x${JM_NEW}
set  BCS_OLD = /discover/nobackup/ltakacs/bcs/$BC_TAG/$RSLV_OLD
set  BCS_NEW = /discover/nobackup/ltakacs/bcs/Fortuna-2_1/$RSLV_NEW
set TOPO_OLD = $BCS_OLD/topo_DYN_ave_${RSLV_OLD}_DC.data
set TOPO_NEW = $BCS_NEW/topo_DYN_ave_${RSLV_NEW}_DC.data
set TILE_OLD = $BCS_OLD/FV_${RSLV_OLD}_DC_360x180_DE.til
set TILE_NEW = $BCS_NEW/FV_${RSLV_NEW}_DC_360x180_DE.til


cd $OUTPUT

set files = `/bin/ls -1 $DYN $MOIST $AGCM $PCHEM $GOCART $CATCH $LAKE $LANDICE $SALT`
foreach file ($files)
  if( -e `basename $file` ) /bin/rm -f `basename $file`
end

if ( $DODYN ) then

set files = `/bin/ls -1 $DYN $MOIST $AGCM $PCHEM $GOCART`
foreach file ($files)
  /bin/cp $file .
end
                    set DYN    = `basename $DYN`
                    set MOIST  = `basename $MOIST`
if( .$AGCM   != . ) set AGCM   = `basename $AGCM`
if( .$PCHEM  != . ) set PCHEM  = `basename $PCHEM`
if( .$GOCART != . ) set GOCART = `basename $GOCART`


if(! -e $GEOSUTIL/post/rs_hinterp.x ) then
     cd $GEOSUTIL/post
     gmake install rs_hinterp.x
     rehash
     cd $OUTPUT
endif
$GEOSUTIL/post/rs_hinterp.x -dyn $DYN \
                            -moist $MOIST \
                            -topo_old $TOPO_OLD \
                            -topo_new $TOPO_NEW \
                            -im $IM_NEW -jm $JM_NEW \
                            -other $AGCM $PCHEM $GOCART

set files = `/bin/ls -1 $DYN $MOIST $AGCM $PCHEM $GOCART`
foreach file ($files)
  if( -e $file ) /bin/rm -f $file
end

else

echo " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "
echo " WARNING: skipping interpolation of dynamics RSTs by request "
echo " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "

endif # <DODYN>

# LAND Regridding
# ---------------

mkdir land
cd    land

/bin/cp -r $MK_RST .
cd mk_restarts/InData
set files = `/bin/ls -1 | grep -v CVS | grep -v README`
foreach file ($files)
/bin/rm -f $file
end
/bin/ln -s $TILE_OLD .
if ( $INDIR == "NULL" ) then
  /bin/cp    $CATCH .
  /bin/cp    $LAKE  .
  /bin/cp    $LANDICE .
  /bin/cp    $SALT .
else
  /bin/cp    $INDIR/$CATCH .
  /bin/cp    $INDIR/$LAKE  .
  /bin/cp    $INDIR/$LANDICE .
  /bin/cp    $INDIR/$SALT .
endif

cd ../OutData
set files = `/bin/ls -1 | grep -v CVS | grep -v README`
foreach file ($files)
/bin/rm -f $file
end
if( -e clsm ) /bin/rm -r clsm
/bin/ln -s $TILE_NEW .
/bin/ln -s $BCS_NEW/clsm .

cd ../
./mk_Restarts

cd OutData
set CATCH   = `basename $CATCH`
set LAKE    = `basename $LAKE`
set LANDICE = `basename $LANDICE`
set SALT    = `basename $SALT`

                 set IM = $IM_NEW
if( $IM < 1000 ) set IM = 0$IM
if( $IM < 100  ) set IM = 0$IM
                 set JM = $JM_NEW
if( $JM < 1000 ) set JM = 0$JM
if( $JM < 100  ) set JM = 0$JM

/bin/mv $CATCH   $OUTPUT/$CATCH.${IM}x${JM}
/bin/mv $LAKE    $OUTPUT/$LAKE.${IM}x${JM}
/bin/mv $LANDICE $OUTPUT/$LANDICE.${IM}x${JM}
/bin/mv $SALT    $OUTPUT/$SALT.${IM}x${JM}

cd $OUTPUT
/bin/rm -r land
/bin/mv *${IM}x${JM} $OUTDIR
/bin/mv regrid.rc    $OUTDIR
cd $OUTDIR
/bin/rm -r $OUTPUT


