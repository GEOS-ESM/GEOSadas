#!/bin/csh -x

# atmens_interp - interpolate ensemble to desired resolution.
#
# !REVISION HISTORY:
#
#  05May2018  Todling   Initial script
#  21Feb2020  Todling   Allow for high freq bkg (up to 1mn)
#
# !TO DO:
#   1. remove interp of ens mean; replace w/ re-calculation of mean
#      after members have neen interpolated
#------------------------------------------------------------------
if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmens_interp.csh

if ( $#argv < 8 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - interpolate upper-air ensemble to desired resolution"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms nlon nlat nlev idir odir "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   nlon   -  number of longitude points"
   echo "   nlat   -  number of latitude points"
   echo "   nlev   -  number of vertical levels"
   echo "   idir   -  location of ensemble "
   echo "   odir   -  location where to place interpolated ensemble"
   echo "              (if odir=idir, original files are renamed and"
   echo "               interpolated files are placed in their place)"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "   This is a driver for converting the members of the ensemble to another "
   echo " horizontal and/or vertical resolution."
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091019 000000 288 181 132 somedir someotherdir"
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo " OPTIONAL RESOURCE FILES"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    FVHOME         - location of experiment            "
   echo "    FVROOT         - location of DAS build             "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo " REMARKS"
   echo " "
   echo " SEE ALSO"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 05May2018      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1

if ( !($?NCSUFFIX) )        setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined "
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3
set nlon  = $4
set nlat  = $5
set nlev  = $6
set idir  = $7
set odir  = $8

set hh    = `echo $nhms | cut -c1-2`
set hhmn  = `echo $nhms | cut -c1-4`
set yyyymmddhh = ${nymd}${hh}

setenv MAXJOBS 7
if ( -d $idir/atmens ) then
   setenv ENSWORK $idir
else
   echo " ${MYNAME}: error, atmens dir must be inside $idir"
   exit(1)
endif
if ( -e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

setenv FROMENS 1
setenv WORKDIR $ENSWORK/${MYNAME}_workdir

# local date/time initialization

set nymd0 = $nymd
set nhms0 = $nhms

# From this point down ... should be clear

set members = `/bin/ls -d $ENSWORK/atmens/mem* | wc`
set nmem = $members[1]
set ftype = "bkg.eta"
cd $ENSWORK/atmens/mem001
set lstbkgeta = `/bin/ls $expid.$ftype.*.$NCSUFFIX`
cd -

if ( -d $WORKDIR ) /bin/rm -r $WORKDIR
mkdir $WORKDIR
mkdir $WORKDIR/interp_ens
cd    $WORKDIR/interp_ens

if ( $FROMENS ) then

   set ihave_ens_mean = 0
   if ( -e $ENSWORK/atmens/ensmean/$expid.$ftype.${nymd}_${hhmn}z.$NCSUFFIX ) then
      set ihave_ens_mean = 1
   endif

   # figure out background dimensions
   if ( -e $ENSWORK/atmens/mem001/$expid.$ftype.${nymd}_${hhmn}z.$NCSUFFIX ) then
      set mem001_eta_file = $ENSWORK/atmens/mem001/$expid.$ftype.${nymd}_${hhmn}z.$NCSUFFIX
      set ens_mres  = `getgfiodim.x $mem001_eta_file`
      set ens_nlons = $ens_mres[1]
      set ens_nlats = $ens_mres[2]
      set ens_nlevs = $ens_mres[3]

      # check for need to interpolated
      if ( $ens_nlons == $nlon && $ens_nlats == $nlat && $ens_nlevs == $nlev ) then
         echo "${MYNAME}: no need to interpolate ensemble ..."
         touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
         echo " ${MYNAME}: Complete "
         exit(0)
      endif

      set hres = "z"
      if ( $nlon ==   72 && $nlat ==  46 ) set hres = "a"
      if ( $nlon ==  144 && $nlat ==  91 ) set hres = "b"
      if ( $nlon ==  288 && $nlat == 181 ) set hres = "c"
      if ( $nlon ==  576 && $nlat == 361 ) set hres = "d"
      if ( $nlon == 1152 && $nlat == 721 ) set hres = "e"
      if ( $hres == "z" ) then
         echo " ${MYNAME}: trouble resetting resolution, aborting ..."
         exit 98
      endif

   else
      echo " ${MYNAME}: cannot file ens-mem001 file $expid.$ftype.${nymd}_${hhmn}z.$NCSUFFIX, aborting ..."
      exit 2
   endif

   # interpolate ensemble mean
   if ( $ihave_ens_mean ) then
      # convert resolution of mean state (would be more accurate to 
      #                                   recompure mean after members interp)
      mkdir ensmean
      @ nn = 0
      foreach fn ( $lstbkgeta )
         if ( $nn < $MAXJOBS ) then
            @ nn++
            echo "${MYNAME}: dyn2dyn.x -g5 -res $hres -nlevs $nlev ..."
            dyn2dyn.x -g5 -indxlevs -res $hres -nlevs $nlev -o ensmean/$fn $ENSWORK/atmens/ensmean/$fn &
            if ( $nn == $MAXJOBS ) then
                wait
                @ nn = 0
            endif
         endif
      end
   endif

   # now take care of the ensemble members themselves
   @ nn = 0
   @ ic = 0
   while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     if ( ! -d mem$memtag ) mkdir mem$memtag

     foreach fn ( $lstbkgeta )
        if ( $nn < $MAXJOBS ) then
            @ nn++
             echo "${MYNAME}: dyn2dyn.x -g5 -res $hres -nlevs $nlev ... mem$memtag ..."
             dyn2dyn.x -g5 -indxlevs -res $hres -nlevs $nlev -o mem$memtag/$fn $ENSWORK/atmens/mem$memtag/$fn &
             if ( $nn == $MAXJOBS ) then
                 wait
                 @ nn = 0
             endif
        endif
     end # each fn
   end # while nmem

else # test

   echo " ${MYNAME}: program should not be here, aborting ..."
   exit 3

endif

cd $WORKDIR

# Figure out when to place interpolated ensemble
# ----------------------------------------------
if ( $odir == $idir ) then
   # reposition original atmens diretory
   if ( ! -d $idir/atmens_Hold ) then
      /bin/mv $idir/atmens $idir/atmens_Hold
   endif 
endif
if ( ! -d $odir/atmens ) then
   # reposition directory w/ interpolated ensemble
   /bin/mv interp_ens $odir/atmens
endif

# If make it up to here ... all should be good
# --------------------------------------------
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
