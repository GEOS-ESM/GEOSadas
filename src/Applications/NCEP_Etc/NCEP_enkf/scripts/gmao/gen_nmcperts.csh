#!/bin/csh 

#######################################################################
# gen_nmcperts.csh - generate 48-minus-24-hour NMC-like perturbations
#
# !REVISION HISTORY:
#
#  30Mar2013  Todling   Initial script
#  13Jun2020  Todling   Update to add rh info to perturbation files
#                       Mild parallelization.
#
#######################################################################

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE ) then
        set echo
    endif
endif

setenv MYNAME gen_nmcperts.csh

if ( $#argv < 7 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - generate NMC-perturbations from (OPS) forecasts"
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  fcstdir workdir expid expout nymd nhms ndays"
   echo " "
   echo " where"
   echo "   fcstdir - path of experiment holding forecasts"
   echo "   workdir - path of directory where to write NMC perturbations to"
   echo "   expid  -  name of experiment holding forecasts"
   echo "   expout -  name of NMC perturbation"
   echo "   nymd   -  date of initial forecast, as in YYYYMMDD "
   echo "   time   -  time of intial forecast, as HHMMSS"
   echo "   ndays  -  number of days to generate pert from current date/time"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "   This script looks in the archive and generates the 48-24-hr NMC-like "
   echo "   perturbations making up the database used in the additive inflation"
   echo "   strategy of the ensemble DAS."
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME /archive/u/dao_ops/GEOS-5.7.2/GEOSadas-5_7_2_p5_m1 \\"
   echo "         /discover/nobackup/rtodling/NMCperts \\"
   echo "         e572p5_fp e572_fp 20120523 000000 1"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLE"
   echo " "
   echo "    FVROOT        - location of DAS build             "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLE"
   echo " "
   echo "    ATMENSETC      - alternative location for RC nmcperts.rc file"
   echo "    ATMENS_VERBOSE - set verbose on"
   echo "    GEN_NMCPERTS_NCPUS - allows for mild parallelization"
   echo "    NCSUFFIX       - suffix for SDF files (default: nc4)"
   echo " "
   echo " SEE ALSO"
   echo "   ut_nmcperts.j - unit tester (helps generate perts when needed)"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 13Jun2020      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit 0
endif

setenv FAILED 0
if ( !($?FVROOT)   ) setenv FAILED 1

if ( !($?ATMENSETC)) then
   setenv ATMENSETC $FVROOT/etc/atmens   
endif
if ( !($?GEN_NMCPERTS_NCPUS) ) setenv GEN_NMCPERTS_NCPUS 1
if ( !($?GEN_NMCPERTS_LOCAL) ) setenv GEN_NMCPERTS_LOCAL 0
if ( !($?OPTRH)    ) setenv OPTRH 2
if ( !($?DRYRUN)   ) setenv DRYRUN
if ( !($?NCSUFFIX) ) setenv NCSUFFIX nc4
if ( !($?OFFSET_HR) ) setenv OFFSET_HR 3

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set fcstdir = $1   # location of experiment hold fcsts
set workdir = $2   # location of experiment hold fcsts
set expid   = $3   # expid of exp holding fcsts
set expout  = $4   # expid of exp holding fcsts
set nymdb   = $5   # date to generate pert for
set nhmsb   = $6   # time to generate pert for
set ndays   = $7   # number of perturbations to generate from given date/time

if ( ($?FVHOME) ) then
   set path = ( . $FVHOME/run $FVROOT/bin $path )
else
   set path = ( . $FVROOT/bin $path )
endif

# loop over number of days to process
# -----------------------------------
set nymd = $nymdb
set nhms = $nhmsb
@ ofs_sc = $OFFSET_HR * 3600 # time offset (usually 3-hr) in seconds 
@ mype = 0
@ nd = 0
while ( $nd < $ndays )
  @ nd++

   set hh = `echo $nhms | cut -c1-2`
   set ttagv = ${nymd}_${hh}z
   if ( $ATMENS_VERBOSE ) then
      echo "${MYNAME}: Valid date/time of forecast: $nymd $nhms"
   endif

   # initial date/time of 48-hr forecasts
   # ------------------------------------
   @ hr48_sc = 48 * 3600 # 48-hours in seconds 
   @ hr51_sc = $hr48_sc + $ofs_sc
   set date0_51 = `tick $nymd $nhms -$hr51_sc`
   set yyyy51 = `echo $date0_51[1] | cut -c1-4`
   set   mm51 = `echo $date0_51[1] | cut -c5-6`
   set   dd51 = `echo $date0_51[1] | cut -c7-8`
   set   hh51 = `echo $date0_51[2] | cut -c1-2`
   set ttag51 = ${yyyy51}${mm51}${dd51}_${hh51}z
   if ( $ATMENS_VERBOSE ) then
      echo "${MYNAME}: initial date/time of 48-hr forecast: $date0_51"
   endif

   # initial date/time of 24-hr forecasts
   # ------------------------------------
   @ hr24_sc = 24 * 3600 # 24-hours in seconds 
   @ hr27_sc = $hr24_sc + $ofs_sc
   set date0_27 = `tick $nymd $nhms -$hr27_sc`
   set yyyy27 = `echo $date0_27[1] | cut -c1-4`
   set   mm27 = `echo $date0_27[1] | cut -c5-6`
   set   dd27 = `echo $date0_27[1] | cut -c7-8`
   set   hh27 = `echo $date0_27[2] | cut -c1-2`
   set ttag27 = ${yyyy27}${mm27}${dd27}_${hh27}z
   if ( $ATMENS_VERBOSE ) then
      echo "${MYNAME}: initial date/time of 24-hr forecast: $date0_27"
   endif

   # form path of 48/24-hr files
   # ---------------------------
   if ($GEN_NMCPERTS_LOCAL) then
     set fname48 = $fcstdir/$expid.prog.eta.${ttag51}+${ttagv}.$NCSUFFIX
     set fname24 = $fcstdir/$expid.prog.eta.${ttag27}+${ttagv}.$NCSUFFIX
   else
     set fname48 = $fcstdir/$expid/prog/Y$yyyy51/M$mm51/D$dd51/H$hh51/$expid.prog.eta.${ttag51}+${ttagv}.$NCSUFFIX
     set fname24 = $fcstdir/$expid/prog/Y$yyyy27/M$mm27/D$dd27/H$hh27/$expid.prog.eta.${ttag27}+${ttagv}.$NCSUFFIX
   endif

   if (! -e $fname48 ) then
      echo "${MYNAME}: failed to find 48-hour forecast file at ${ttagv}, $fname48" 
      exit (48)
   endif
   if (! -e $fname24 ) then
      echo "${MYNAME}: failed to find 24-hour forecast file at ${ttagv}, $fname24" 
      exit (24)
   endif

   if (! $GEN_NMCPERTS_LOCAL ) then
      $DRYRUN dmget $fname48 $fname24
   endif

   if (! -d $workdir ) mkdir -p $workdir
   set pertfname = `echorc.x -rc $ATMENSETC/nmcperts.rc -template dummy $nymd ${hh}0000 nmc_perts_fnametmpl`
   
   if (! -e $workdir/$expout.f48m24_rh.eta.${ttagv}.$NCSUFFIX ) then
      $DRYRUN dyndiff.x -g5 $fname48 $fname24 -addrh -$OPTRH -o $workdir/$expout.f48m24_rh.eta.${ttagv}.$NCSUFFIX &
      if ( $mype < $GEN_NMCPERTS_NCPUS ) then
         @ mype++
      else
        wait
        @ mype = 0
      endif
   endif

   set newdate = `tick $nymd $nhms $hr24_sc`
   set nymd = $newdate[1]
   set nhms = $newdate[2]
end # <ndays>
wait
