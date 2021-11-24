#!/bin/csh

# atmens_berror - create static (parameterized) background error covariance
#                 based on available atmospheric ensemble background members.
#
# !REVISION HISTORY:
#
#  10Dec2015  Todling   Initial script
#  22Dec2016  Todling   Allows for creating Berror at resolution other
#                       then ensemble actual resolution.
#  02Apr2018  Todling   Allow for vertical interpolation
#  26May2018  Todling   Unwired namelist from script
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
setenv MYNAME atmens_berror.csh

if ( $#argv < 5 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - create parameterized B-error from ensemble members"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms idir odir "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   idir   -  location of ensemble "
   echo "   odir   -  location where to link output B-error file to (named: berror_stats)"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "   This is a driver for running calcstats over the members of the "
   echo " atmospheric ensemble"
   echo " "
   echo " Example of valid command line:"
   echo " $MYNAME b541iau 20091019 000000 somedir someotherdir"
   echo " "
   echo " REQUIRED RESOURCE FILES"
   echo " "
   echo " OPTIONAL RESOURCE FILES"
   echo "    atmens_berror.rc - when found in idir allow for B-error to be"
   echo "                       created at desired resolution"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    FVHOME         - location of experiment            "
   echo "    FVROOT         - location of DAS build             "
   echo "    MPIRUN_CALCSTATS - controls executable for B-err generation"
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    BERROR_FROMENS - when specified, replaces berror_stats file with that produced here"
   echo "    BERROR_NMODES  - specify number of modes to use in balance calculation (def.: 10)"
   echo " "
   echo " REMARKS"
   echo " "
   echo " SEE ALSO"
   echo "   calcstats.x - program doing actual work"
   echo "   ut\_atmens\_berror.j - off-line unit tester"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 22Dec2016      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?MPIRUN_CALCSTATS) ) setenv FAILED 1

if ( !($?BERROR_FROMENS) )  setenv BERROR_FROMENS 0
if ( !($?BERROR_NMODES)  )  setenv BERROR_NMODES  10
if ( !($?NCSUFFIX) )        setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined "
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3
set hh    = `echo $nhms | cut -c1-2`
set hhmn  = `echo $nhms | cut -c1-4`
set yyyymmddhh = ${nymd}${hh}
set idir  = $4
set odir  = $5

setenv MAXJOBS 4
setenv ENSWORK $idir
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
set nhrinc = -9

# From this point down ... should be clear

set members = `/bin/ls -d $ENSWORK/mem* | wc`
set nmem = $members[1]
set ftype = "bkg.eta"

if ( -d $WORKDIR ) /bin/rm -r $WORKDIR
mkdir $WORKDIR
mkdir $WORKDIR/samples
cd    $WORKDIR/samples

if ( $FROMENS ) then

   # figure out background dimensions
   if ( -e $ENSWORK/ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX ) then
      set mean_eta_file = $ENSWORK/ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX
      set ens_mres  = `getgfiodim.x $mean_eta_file`
      set ens_nlons = $ens_mres[1]
      set ens_nlats = $ens_mres[2]
      set ens_nlevs = $ens_mres[3]

      setenv NLEV   $ens_nlevs
      setenv NSIG   $ens_nlevs
      setenv NLAT   $ens_nlats
      setenv NLON   $ens_nlons
      setenv HFAC   -1.0

      # if desired resolution differs form ensemble resolution ... 
      if ( -e $idir/atmens_berror.rc ) then
         set want_lon = `echorc.x -rc $idir/atmens_berror.rc berror_nlon`
         set want_lat = `echorc.x -rc $idir/atmens_berror.rc berror_nlat`
         set want_lev = `echorc.x -rc $idir/atmens_berror.rc berror_nlev`
         if ( $ens_nlons == $want_lon && $ens_nlats == $want_lat && $ens_nlevs == $want_lev ) then
             echo "${MYNAME}: deriving B-error for resolution of ensemble"
         else
             set hres = "z"
             if ( $want_lon ==   72 && $want_lat ==  46 ) set hres = "a"
             if ( $want_lon ==  144 && $want_lat ==  91 ) set hres = "b"
             if ( $want_lon ==  288 && $want_lat == 181 ) set hres = "c"
             if ( $want_lon ==  576 && $want_lat == 361 ) set hres = "d"
             if ( $want_lon == 1152 && $want_lat == 721 ) set hres = "e"
             if ( $hres == "z" ) then 
                echo " ${MYNAME}: trouble resetting resolution, aborting ..."
                exit 98
             endif

             echo "${MYNAME}: interpolating background to get B-error ..."
             set ftype = "bkgeta.${hres}l${want_lev}"

             setenv NLEV   $want_lev
             setenv NSIG   $want_lev
             setenv NLAT   $want_lat
             setenv NLON   $want_lon
         endif
      endif

   #  The following is set consistent w/ the climatological error cov
      setenv JCAP -1
      if ( $NLAT == 721 ) then
         setenv JCAP 512
      endif
      if ( $NLAT == 361 ) then
         setenv JCAP 382
      endif
      if ( $NLAT == 181 ) then
         setenv JCAP 142
      endif
      if ( $NLAT ==  91 ) then
         setenv JCAP 84
      endif
      if ( $NLAT ==  46 ) then
         setenv JCAP 42
      endif
      if ( $JCAP < 0 ) then
         echo " ${MYNAME}: cannot define JCAP, aborting ..."
         exit 1
      endif

   else
      echo " ${MYNAME}: cannot file ens-mean file $expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX, aborting ..."
      exit 2
   endif

   # bring in ensmean and take care of interpolation if needed
   ln -sf $ENSWORK/ensmean/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX  $expid.bkg.eta.${nymd}_${hhmn}z.ensmean.$NCSUFFIX
   if ( $ftype != "bkg.eta" ) then
      # convert resolution of mean state (would be more accurate to 
      #                                   recompure mean after members interp)
      echo "${MYNAME}: dyn2dyn.x -g5 -res $hres -nlevs $want_lev ..."
      dyn2dyn.x -g5 -res $hres -nlevs $want_lev -o $expid.$ftype.${nymd}_${hhmn}z.ensmean.$NCSUFFIX \
                                                   $expid.bkg.eta.${nymd}_${hhmn}z.ensmean.$NCSUFFIX
   endif
   # prepare fake 24-hr forecasts
   #   first copy files ...
   @ ic = 0
   @ nn = 0
   while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     if ( $nn < $MAXJOBS ) then
        @ nn++
        /bin/cp $expid.$ftype.${nymd}_${hhmn}z.ensmean.$NCSUFFIX ${expid}_mem$memtag.$ftype.${nymd}_${hhmn}z+24 &
        if ( $nn == $MAXJOBS ) then 
            wait
            @ nn = 0
        endif
     endif
   end # while nmem
   #   then link and time-stamp files ...
   set newdate = ( $nymd0 $nhms0 )
   echo "${MYNAME}: B-error from ensemble on: $newdate"
   @ offset_sec = 24 * 3600
   @ ic = 0
   while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     echo "${MYNAME}: mean         ${expid}_mem$memtag.$ftype.${nymd}_${hhmn}z+24 $newdate[1] $newdate[2] $nhrinc"
                      reset_time.x ${expid}_mem$memtag.$ftype.${nymd}_${hhmn}z+24 $newdate[1] $newdate[2] $nhrinc
     set newdate  = ( `tick $newdate $offset_sec` )
   end # while nmem

   # link members to mimic 48-hr fcst and take care of interpolation if needed
   @ nn = 0
   @ ic = 0
   while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`

     ln -sf  $ENSWORK/mem$memtag/$expid.bkg.eta.${nymd}_${hhmn}z.$NCSUFFIX $expid.bkg.eta.${nymd}_${hhmn}z.mem$memtag.$NCSUFFIX
     if ( $ftype != "bkg.eta" ) then
         if ( $nn < $MAXJOBS ) then
             @ nn++
              echo "${MYNAME}: dyn2dyn.x -g5 -res $hres -nlevs $want_lev ... mem$memtag ..."
              dyn2dyn.x -g5 -res $hres -nlevs $want_lev -o $expid.$ftype.${nymd}_${hhmn}z.mem$memtag.$NCSUFFIX \
                                                           $expid.bkg.eta.${nymd}_${hhmn}z.mem$memtag.$NCSUFFIX &
              if ( $nn == $MAXJOBS ) then
                  wait
                  @ nn = 0
              endif
         endif
     endif
   end # while nmem

   # final link to prepare fake 48-hr forecasts
   @ ic = 0
   set newdate = ( $nymd0 $nhms0 )
   echo $newdate
   @ offset_sec = 24 * 3600
   while ( $ic < $nmem )
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     set newdate  = ( `tick $newdate $offset_sec` )

     ln -sf  $expid.$ftype.${nymd}_${hhmn}z.mem$memtag.$NCSUFFIX  ${expid}_mem$memtag.$ftype.${nymd}_${hhmn}z+48
     echo "${MYNAME}: member       ${expid}_mem$memtag.$ftype.${nymd}_${hhmn}z+48 $newdate[1] $newdate[2] $nhrinc"
                      reset_time.x ${expid}_mem$memtag.$ftype.${nymd}_${hhmn}z+48 $newdate[1] $newdate[2] $nhrinc

   end # while nmem

else # test

   echo " ${MYNAME}: program should not be here, aborting ..."
   exit 3

endif

cd $WORKDIR

if (-e infiles ) /bin/rm infiles
/bin/ls samples/*+24 >> infiles
/bin/ls samples/*+48 >> infiles
ln -sf infiles fort.10

# Prepare resource files
set this_param = $FVHOME/run/berror_stats.nml.tmpl
if ( -e $this_param ) then
   if ( -e stats.param ) /bin/rm  stats.parm
   if ( -e sed_file    ) /bin/rm  sed_file
   echo "s/>>>JCAP<<</${JCAP}/1"       > sed_file
   echo "s/>>>HFAC<<</${HFAC}/1"      >> sed_file
   echo "s/>>>NSIG<<</${NSIG}/1"      >> sed_file
   echo "s/>>>NLAT<<</${NLAT}/1"      >> sed_file
   echo "s/>>>NLON<<</${NLON}/1"      >> sed_file
   echo "s/>>>NMODES<<</${BERROR_NMODES}/1"    >> sed_file
   sed -f sed_file  $this_param  > ./stats.parm
else
   echo "${MYNAME}: cannot find $this_param ..."
   exit (1)
endif

# Set "boundary conditions" ...
ln -sf $FVHOME/fvInput/Static/fvgsi/etc/sst2dvar_stat0.5 berror_sst
#ln -sf /discover/nobackup/aelakkra/BKGERR/addoz/l72x576y361.berror_stats_psoz.bin oz.dat

$MPIRUN_CALCSTATS

if ( -e gsi.berror_stats ) then
   /bin/mv gsi.berror_stats $expid.gsi.berror_stats.${nymd}_${hh}z.bin
   foreach fn ( balvar_sp \
                berror_stats \
                bgstats_sp \
                sststats_sp \
                stst_sp \
                tst_sp )
       if ( -e $fn.grd ) then
          /bin/mv $fn.grd $expid.gsi.$fn.${nymd}_${hh}z.grd
       endif
   end
   tar cvf $ENSWORK/$expid.gsi.berror_stats.${nymd}_${hh}z.tar  $expid.gsi.berror_stats.${nymd}_${hh}z.bin \
                                                                $expid.gsi.*${nymd}_${hh}z.grd
   
   echo " ${MYNAME}: done creating B-error file"
   if ( $BERROR_FROMENS ) then
      /bin/rm $odir/berror_stats
      /bin/ln -sf $WORKDIR/$expid.gsi.berror_stats.${nymd}_${hh}z.bin $odir/berror_stats
      echo " ${MYNAME}: berror_stats -> $expid.gsi.berror_stats.${nymd}_${hh}z.bin" 
      echo " ${MYNAME}: replaced B-error file with ensemble-based on"
   endif

else
   echo " ${MYNAME}: failed to create B-error file, aborting ..."
   exit 4
endif

/bin/rm -r fort.*00*
/bin/rm -r fort.*01*
/bin/rm -r fort.*02*

# Reset date/time of ensemble members to leave them as originals
# --------------------------------------------------------------
if ( $FROMENS ) then
   if ( $ftype == "bkg.eta" ) then
      @ ic = 0
      while ( $ic < $nmem )

        @ ic++
        set memtag  = `echo $ic |awk '{printf "%03d", $1}'`

        ln -sf  $ENSWORK/mem$memtag/$expid.$ftype.${nymd}_${hhmn}z.$NCSUFFIX  ${expid}_mem$memtag.$ftype.${nymd}_${hhmn}z+48
        echo "member       $ENSWORK/mem$memtag/$expid.$ftype.${nymd}_${hhmn}z.$NCSUFFIX  $nymd $nhms $nhrinc"
              reset_time.x $ENSWORK/mem$memtag/$expid.$ftype.${nymd}_${hhmn}z.$NCSUFFIX  $nymd $nhms $nhrinc

      end # while nmem
   endif
   
endif

# If make it up to here ... all should be good
# --------------------------------------------
touch $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
