#!/bin/csh -x

setenv MYNAME jedi_post.csh

# orig from: /gpfsm/dnb31/drholdaw/JediWork/GeosRun4Ricardo

if ( $#argv < 2 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - set up environment and retrieve data needed to run JEDI analysis within GEOSadas"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  nymd nhms "
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Initial version: 18Oct2020    by: R. Todling"
   echo "     Last   modified: 18Oct2020    by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?EXPID)            )  setenv FAILED   1
if ( !($?FVWORK)           )  setenv FAILED   1
if ( !($?JEDI_CONCAT_IODA) )  setenv FAILED   1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

echo "${MYNAME}: this script needs updating ..."
exit (1)

# Command line arguments
set nymdb = $1   # initial date of var window
set nhmsb = $2   # initial time of var window
set opt   = $3   # hofx or sens
set yyyyb    = `echo $nymdb | cut -c1-4`
set mmb      = `echo $nymdb | cut -c5-6`
set ddb      = `echo $nymdb | cut -c7-8`
set hhb      = `echo $nhmsb | cut -c1-2`

# Get positioned
cd $FVWORK/jana

# Concatenate IODA files
# ----------------------
if ( $JEDI_CONCAT_IODA ) then
#  module purge
#  setenv PATH /home/dao_ops/.local/bin:$PATH
#  setenv PYTHONUSERBASE /home/dao_ops/.local/
   module purge
   setenv PATH /home/jardizzo/.local/bin:$PATH
   setenv PYTHONUSERBASE /home/jardizzo/.local
   if ( ! -d Data/$opt/Concat ) mkdir -p Data/$opt/Concat
   set this = `grep $opt Config/envarfgat.yaml | grep obsfile`
   if ( $opt == "osen" ) then
      set this = `grep $opt Config/adenvarfgat.yaml | grep obsfile`
   endif
   foreach fn ( $this )
       if ($fn != "obsfile:") then
           set pfx = `basename $fn | cut -d. -f1`
           echo "will concat type: $pfx ..."
           netcdf-concat -o Data/$opt/Concat/$EXPID.$pfx.nc4 Data/$opt/${pfx}_*.nc4 &
           if( -e Data/$opt/Concat/$pfx.nc4 ) echo "Concat: Data/$opt/Concat/$fn"
       endif
   end
   wait
endif

cd Data/$opt/Concat
tar cvf $FVWORK/$EXPID.jedi_${opt}.${nymdb}_${hhb}z.tar  $EXPID.*_${opt}_*nc4
cd -
