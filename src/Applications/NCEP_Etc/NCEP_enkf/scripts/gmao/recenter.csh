#!/bin/csh

# recenter.csh - command line for recentering program
#                to facilitate pbs script
#
# !REVISION HISTORY:
#
#  22Apr2012  Todling   Initial script
#  31Aug2012  Todling   Add usage
#  09Mar2013  Todling   Rename inflating perturbation file
#  12Mar2014  Todling   - Remove optional env variables (after Amal findings)
#                       - Modified interface accordingly
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME recenter.csh

# need usage here
# ---------------
if ( $#argv < 8 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - recenter ensemble of analyses"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  fvroot expid member nymd hhmn ftype1 ftype2 "
   echo "           BLEND ADDINF CENLOC INFLOC RCFILE"
   echo " " 
   echo " where" 
   echo "   fvroot -  ROOT of build "
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   member -  number of member to operate on"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   hhmn   -  time of analysis, as in HHNM"
   echo "   ftype1 -  type of input field (i.e, ana.eta or inc.eta)"
   echo "   ftype2 -  type of central field (i.e, ana.eta)"
   echo "   BLEND  -  option to blend with central (-damp)"
   echo "   ADDINF -  additive inflation factor"
   echo "   CENLOC -  location of central analysis"
   echo "   INFLOC -  location of inflating perturbations"
   echo "             (set to NONE when not applicable)"
   echo "   RCFILE -  RC file for dyn_recenter program"
   echo " " 
   echo " DESCRIPTION"
   echo "    This script provides a wrapper for the call to dyn_recenter.x, which "
   echo "  is the program actually responsible for recentering an ensemble member" 
   echo "  around a given (typically, central DAS) analysis. " 
   echo " " 
   echo "    The dyn_recenter program is also responsible for applying additive"
   echo " inflation to each member of the ensemble."
   echo " " 
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC - location of ensemble resource files"
   echo "    FVHOME    - location of experiment            "
   echo "    FVROOT    - location of DAS build             "
   echo " " 
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "   NOTE: this procedure cannot have optional env variables"
   echo " " 
   echo " SEE ALSO"
   echo " " 
   echo "  atmens_recenter.csh - driver script for recentering of members"
   echo "  dyn_recenter.x      - program that actually recenters given member"
   echo " " 
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0

if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit(1)
endif

set fvroot  = $1
set expid   = $2
set memtag  = $3
set nymd    = $4
set hhmn    = $5
set ftype1  = $6
set ftype2  = $7
set blend   = $8
set addinf_factor = $9
set cenloc  = $10
set infloc  = $11
set rcfile  = $12

set hh = `echo $hhmn | cut -c1-2`

#source $fvroot/bin/g5_modules
if ( ($?FVHOME) ) then
   set path = ( . $FVHOME/run $fvroot/bin $path )
else
   set path = ( .             $fvroot/bin $path )
endif

 if ( "$blend" != "NONE" ) then
    set blend = "-damp"
 else
    set blend = 
 endif

 if ( "$infloc" == "NONE" ) then
     set addinflation = 
 else
     if ( "$addinf_factor" == "NONE" ) then
        set addinflation = "-inflate $infloc/$expid.nmcpert.eta.${nymd}_${hh}z.$memtag.$NCSUFFIX"
     else
        set addinflation = "-a $addinf_factor -inflate $infloc/$expid.nmcpert.eta.${nymd}_${hh}z.$memtag.$NCSUFFIX"
     endif
 endif
 if ( "$ftype2" == "NONE" && "$cenloc" == "NONE" ) then
   dyn_recenter.x -g5 -rc $rcfile $blend $addinflation \
                       $expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX \
                       NONE \
                       NONE
 else
     if ( "$ftype2" == "NONE" ) then
        dyn_recenter.x -g5 -rc $rcfile $blend $addinflation \
                        $expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX \
                        $cenloc/$expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX \
                        $cenloc/$expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX
     else
        dyn_recenter.x -g5 -rc $rcfile $blend $addinflation \
                        $expid.${ftype1}.${nymd}_${hhmn}z.$NCSUFFIX \
                        ../ensmean/$expid.${ftype2}.${nymd}_${hhmn}z.$NCSUFFIX \
                        $cenloc/$expid.${ftype2}.${nymd}_${hhmn}z.$NCSUFFIX
     endif
 endif
 if ( $status ) then
    echo " ${MYNAME}: dyn_recent failed for member $memtag "
    exit(1)
 endif

