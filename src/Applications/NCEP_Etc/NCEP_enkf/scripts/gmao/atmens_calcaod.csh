#!/bin/csh

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmens_calcaod.csh
setenv DRYRUN #echo 

if ( $#argv < 7 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - calculate AOD from Concentration files"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms conc aod workdir "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   conc   -  Concentration file label (e.g., abkg)"
   echo "   aod    -  AOD file label (e.g., aod_f)"
   echo "   rcdir  -  location of RC files used for Mie calculation, etc"
   echo "   workdir-  frequency of AOD analysis, as in HHMMSS"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    Uses the background or analyzed aerosol concentration files to produce calculate " 
   echo "  total aerosol optional depth (AOD). Though in principle the offline calculate "
   echo "  performed here should reproduce the online calculation done within the atmospheric model,"
   echo "  in practice differing resolution betwen the underlying model and the aerosol file "  
   echo "  provided to this procedure will lead to small differences (e.g., cubed AGCM, lat/lon history)."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 030000 aana aod_a /rcdir /wrkdirname"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "  FVHOME        - location of experiment            "
   echo "  FVROOT        - location of DAS build             "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "  NCSUFFIX          - suffix of hdf/netcdf files (default: nc4)"
   echo " "
   echo " RESOURCE FILES"
   echo " "
   echo "   Chem_MieRegistry.rc "
   echo "   GAAS_Mie.rc         "
   echo " "
   echo " SEE ALSO"
   echo " "
   echo "   atmos_eaod.csh"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 26Mar2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1

if ( !($?ATMENS_CALCAOD) ) setenv ATMENS_CALCAOD 1
if ( !($?NCSUFFIX) )       setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3;  set hh = `echo $nhms | cut -c1-2`
set conc  = $4
set aod   = $5
set rcdir = $6
set work  = $7

setenv MYWORK $work

#set path =  ( . $FVHOME/run $FVROOT/bin $path )
#source $FVROOT/bin/g5_modules.sh

# SP1 mambo-jumbo (just in case)
unsetenv PMI_RANK
unsetenv PMI_FD
unsetenv PMI_JOBID
unsetenv PMI_SIZE

if ( ! -d $MYWORK ) mkdir -p $MYWORK
cd $MYWORK

ln -sf $rcdir/GAAS_AerRegistry.rc Chem_MieRegistry.rc
ln -sf $rcdir/GAAS_Mie.rc         myMie.rc

setenv EXTDATA $MYWORK/ExtData
if ( ! -d $EXTDATA ) then
  /bin/mkdir -p $EXTDATA
  /bin/touch $EXTDATA/.no_archiving
  /bin/ln -sf $FVHOME/fvInput/g5chem $EXTDATA/
  /bin/ln -sf $FVHOME/fvInput/g5gcm $EXTDATA/
  /bin/ln -sf $FVHOME/fvInput/PIESA $EXTDATA/
  /bin/ln -sf $FVHOME/fvInput/MERRA2 $EXTDATA/
  /bin/ln -sf $FVHOME/fvInput/AeroCom $EXTDATA/
endif

which Chem_Aod.x
if ($status) then
  env
  echo " ${MYNAME}: cannot find Chem_Aod.x, aborting ..."
  exit 2
endif

if ( $ATMENS_CALCAOD ) then
   set dirs = (`/bin/ls -d mem0*`)
   foreach dir ($dirs)
       set nnn = `echo $dir | cut -c4-6`
       $DRYRUN Chem_Aod.x -only_taod -totext2d -t myMie.rc -o $dir/$expid.$aod.sfc.${nymd}_${hh}00z.mem$nnn.$NCSUFFIX $dir/$expid.$conc.eta.${nymd}_${hh}z.mem$nnn.$NCSUFFIX &
   end
   wait

   if ( $DRYRUN == "" ) then
      foreach dir ($dirs)
        set nnn = `echo $dir | cut -c4-6`
        if ( ! -e $dir/$expid.$aod.sfc.${nymd}_${hh}00z.mem$nnn.$NCSUFFIX ) then
           echo " ${MYNAME}: unable to find $dir/$expid.$aod.sfc.${nymd}_${hh}00z.mem$nnn.$NCSUFFIX, aborting ..."
           exit 3
        else
        # $BASEDIR/Linux/bin/ncrename -h -v taod,AOD $dir/$expid.$aod.sfc.${nymd}_${hh}00z.$NCSUFFIX
        endif
      end
   endif # DRYRUN
else

   $DRYRUN Chem_Aod.x -totext2d -t myMie.rc -o $expid.$aod.sfc.${nymd}_${hh}00z.$NCSUFFIX $expid.$conc.eta.${nymd}_${hh}z.$NCSUFFIX
   # $BASEDIR/Linux/bin/ncrename -h -v taod,AOD $expid.$aod.sfc.${nymd}_${hh}00z.$NCSUFFIX

endif

# if made it to here
exit 0
