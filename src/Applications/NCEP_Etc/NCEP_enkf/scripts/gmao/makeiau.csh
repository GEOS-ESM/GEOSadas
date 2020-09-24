#!/bin/csh -f

# makeiau.csh - stript off ens2gcm to allow use of general 
#               pbs script
#
# !REVISION HISTORY:
#
#  05Nov2011  Todling   Initial script
#  13Aug2012  Todling   Add cubed support (eventually the same
#                       mkiau code should handle either case)
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME makeiau.csh

# need usage here
# ---------------
if ( $#argv < 4 ) then
   echo " "
   echo " \begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - create IAU increment for given member"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  expid member nymd nhms "
   echo " " 
   echo " where" 
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   member -  number of member to operate on"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo " " 
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo "  mkiau.rc.tmpl - when running cubed, this must be under ATMENSETC"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC - location of ensemble RC files     "
   echo "    FVHOME    - location of experiment            "
   echo "    FVROOT    - location of DAS build             "
   echo "    MPIRUN_ENSIAU - mpi-run command to handle executable" 
   echo " " 
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " " 
   echo "    NCSUFFIX  - suffix for netcdf files (default: nc4)"
   echo " " 
   echo " REMARKS "
   echo "  1. This procedure is largely obsolete." 
   echo " " 
   echo " SEE ALSO "
   echo " " 
   echo "   atmos_ens2gcm.csh - driver of IAU-increment calculation" 
   echo " " 
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \end{verbatim} "
   echo " \clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?MPIRUN_ENSIAU) ) setenv FAILED 1

if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

#source $FVROOT/bin/g5_modules
if ( ($?FVHOME) ) then
   set path = ( . $FVHOME/run $FVROOT/bin $path )
else
   set path = ( . $FVROOT/bin $path )
endif

set expid  = $1
set memtag = $2
set nymd   = $3
set nhms   = $4

set hh = `echo $nhms | cut -c1-2`
set hhmn = `echo $nhms | cut -c1-4`

if ( -e $ATMENSETC/mkiau.rc.tmpl ) then # this means: running cubed

     /bin/rm -f sed_file
     echo "s/>>>EXPID<<</${expid}/1"         > sed_file
     echo "s/>>>NCSUFFIX<<</${NCSUFFIX}/1"  >> sed_file
     echo "s/>>>ANADATE<<</${nymd}/1"       >> sed_file
     echo "s/>>>ANATIME<<</${nhms}/1"       >> sed_file
     echo "s/>>>NMEMTAG<<</mem${memtag}/1"  >> sed_file
     /bin/rm -f ./mkiau.rc
     sed -f sed_file  $ATMENSETC/mkiau.rc.tmpl  > ./mkiau.rc
     $MPIRUN_ENSIAU
     if (! -e IAU_EGRESS ) then
        echo " ${MYNAME}: iau increment generation failed for member $memtag "
        exit(1)
     endif

else

     $MPIRUN_ENSIAU -ana $expid.ana.eta.${nymd}_${hhmn}z.mem$memtag.$NCSUFFIX \
                    -bkg $expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX -divr \
                    -iau agcm_import_mem${memtag}_rst -nymd $nymd -nhms ${hhmn}00
     if ( $status ) then
        echo " ${MYNAME}: iau increment generation failed for member $memtag "
        exit(1)
     else
        touch IAU_EGRESS
     endif

endif

