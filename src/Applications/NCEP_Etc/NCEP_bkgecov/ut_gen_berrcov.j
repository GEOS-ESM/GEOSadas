#!/bin/csh  -x
#SBATCH --account=g0613
#SBATCH --constraint=sky
#_SBATCH --ntasks=8
#_SBATCH --ntasks=36
#_SBATCH --ntasks=96
#SBATCH --ntasks=1000
#_SBATCH --ntasks-per-node=24
#SBATCH --time=12:00:00
#SBATCH --qos=dastest
#SBATCH --partition=preops
#SBATCH --output=gen_bkgecov.log.o%j

###################################################################
#
# Note that this script now implements two options to link to the
# forecast files required by the NMC method:
#
# R. Todling, 18 April 2020
#
###################################################################

 setenv DRYRUN #echo
 setenv SKIPSETTING 0
 setenv MYNCPUS 8
# Set initial time and number of samples required
 setenv EXPID x0039_p5_REPLAY_L132
 setenv EXPID x41Lrt
 setenv EXPID x0041
 setenv EXPID f521_fp
 setenv EXPID f525_p5_fp
 setenv EXPID f522_fp
 setenv EXPID f525_fp
 setenv EXPID f525_p7_fp
 setenv EXPID f5271_fp
#setenv FVHOME /home/dao_ops/$EXPID
#setenv ARCROOT /home/dao_ops/$EXPID/run/.../archive/prog
#setenv FVHOME /discover/nobackup/projects/gmao/obsdev/rtodling/$EXPID
#setenv FVHOME /discover/nobackup/projects/gmao/obsdev/rtodling/x0041
setenv FVHOME /discover/nobackup/projects/gmao/dadev/rtodling/prePP
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv PLAINDIR 0
setenv ARCROOT /archive/u/$user/$EXPID/prog
if ( $EXPID == "x0041" ) then
   setenv ARCROOT /archive/u/dao_it/$EXPID/prog
endif
if ( $EXPID == "f521_fp" || $EXPID == "f522_fp" || $EXPID == "f525_fp" || $EXPID == "f525_p5_fp" || $EXPID == "f525_p7_fp" || $EXPID == "f5271_fp" ) then
   setenv ARCROOT /home/dao_ops/$EXPID/run/.../archive/prog
endif
if ($EXPID == "x0039_p5_REPLAY_L132") then
   setenv ARCROOT /gpfsm/dnb78s1/projects/p18/ltakacs/REPLAY_Experiments/x0039_p5_REPLAY_L132/forecasts/Regular_RPLFRQ:7200_ANA:x0039_p5
   setenv PLAINDIR 1
endif
setenv DMGET        0
setenv GET_SET      0

setenv DO_ACQUIRE   0
setenv GEN_NMCDIFFS 0

setenv GET_BERROR    1
setenv BERROR_NMODES 25

set these_lats = ( 25 46 91 181 361 721 )
#set these_lats = ( 721 )

# Basic settings (weak dependency on version of DAS)
# --------------------------------------------------
set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

setenv MYNAME ut_gen_berrcov.j
setenv ATMENS_BATCHSUB sbatch
setenv GID g0613
setenv FCSTACQ_WALLCLOCK 2:00:00
setenv FCSTWORK /discover/nobackup/projects/gmao/obsdev/$user/fcst4berrcov.$EXPID
setenv FCSTWORK /discover/nobackup/projects/gmao/dadev/$user/fcst4berrcov.$EXPID

if ($?I_MPI_ROOT ) then
  setenv MPIRUN_CALCSTATS "mpirun -np 864 calcstats.x"
# setenv MPIRUN_CALCSTATS "mpirun -np 64 calcstats.x"
else
  setenv MPIRUN_CALCSTATS "mpirun_exec -np 96 calcstats.x"
endif
#
# initial verification date and number of samples (will get samples ahead of initial date)
set vnymd0 = 20191125
set vnymd0 = 20181201
set vnymd0 = 20200410
set vnymd0 = 20181101
set vnymd0 = 20200203
set vnymd0 = 20200401
set vnymd0 = 20200301
set vnymd0 = 20210203 # start
set vnymd0 = 20200804
#if ( $EXPID == "f522_fp") then
#   set vnymd0 = 20190304
#   @ nsamples = 350 
#   set vnymd0 = 20200206
#   @ nsamples = 23  
#
#endif
set vnhms0 = 000000
@ nsamples = 58
#setenv FCSTWRK $FCSTWORK.$vnymd0.$vnhms0
setenv FCSTWRK $FCSTWORK.all
mkdir $FCSTWRK
set diren = `dirname $FCSTWRK`
set spool = "-s $diren/spool "

@ oned_sc = 24 * 3600

if ( $GET_SET ) then

  set foffset_hr = 3  # offset in initial time from synoptic hour
  set nmc_hrv = 24    # begin time of NMC method / verification time
  set nmc_hrf = 48    # end   time of NMC method / fcst time
  set vmn = 00 # either black or 00
  set vmn =    # either black or 00

  # ----------------------------------
  # No user change from this part down
  # ----------------------------------
  @ foffset_sc = $foffset_hr * 3600 
  @ gap_sc  = $nmc_hrv * 3600
  @ nmc_scv = $nmc_hrv * 3600 + $foffset_sc
  @ nmc_scf = $nmc_hrf * 3600 + $foffset_sc

# **********************************************************
#             Acquire forecast files 
# **********************************************************
  cd $FCSTWRK
  if ( ! -d ACQedFiles ) mkdir ACQedFiles

  if ( ! $SKIPSETTING )  then

     set acqfile = fcst4berr.acq
     if ( -e $acqfile ) /bin/rm $acqfile
     if ( -e sfcst4berr.txt ) /bin/rm sfcst4berr.txt # list of short-range fcsts
     if ( -e lfcst4berr.txt ) /bin/rm lfcst4berr.txt # list of longer-range fcsts
     touch $acqfile sfcst4berr.txt lfcst4berr.txt
     set inidate  = ( $vnymd0 $vnhms0 )
     set vhh = `echo $vnhms0 | cut -c1-2`
     set vyyyymmddhh0 = ${vnymd0}_${vhh}z
     @ nc = 0
     while ($nc < $nsamples) 
        @ nc++
        set vyyyy = `echo $inidate[1] | cut -c1-4`
        set vmm   = `echo $inidate[1] | cut -c5-6`
       set vdd   = `echo $inidate[1] | cut -c7-8`
       set vhh   = `echo $inidate[2] | cut -c1-2`
       set vtmpl = $inidate[1]_${vhh}${vmn}z
   
       # treat short-range fcsts
       set sdate = ( `tick $inidate -$nmc_scv` )
       set syyyy = `echo $sdate[1] | cut -c1-4`
       set smm   = `echo $sdate[1] | cut -c5-6`
       set sdd   = `echo $sdate[1] | cut -c7-8`
       set shh   = `echo $sdate[2] | cut -c1-2`
       set stmpl = $sdate[1]_${shh}z
       if ($PLAINDIR) then
          set sdatadir = ${ARCROOT}_{vtmpl}z:
       else
          set sdatadir = $ARCROOT/Y$syyyy/M$smm/D$sdd/H$shh
       endif
       set sfname   = $EXPID.prog.eta.${stmpl}+$vtmpl # should use echorc.x to define filenames ...
       set spfname  = $sdatadir/$sfname.nc4
   
       # treat longer-range fcsts
       set ldate = ( `tick $inidate -$nmc_scf` )
       set lyyyy = `echo $ldate[1] | cut -c1-4`
       set lmm   = `echo $ldate[1] | cut -c5-6`
       set ldd   = `echo $ldate[1] | cut -c7-8`
       set lhh   = `echo $ldate[2] | cut -c1-2`
       set ltmpl = $ldate[1]_${lhh}z
       if ($PLAINDIR) then
          set ldatadir = ${ARCROOT}_{vtmpl}z:
       else
          set ldatadir = $ARCROOT/Y$lyyyy/M$lmm/D$ldd/H$lhh
       endif
       set lfname   = $EXPID.prog.eta.${ltmpl}+$vtmpl  # should use echorc.x to define filenames ...
       set lpfname  = $ldatadir/$lfname.nc4
   
       # data location
       if (! -e ACQedFiles/$sfname.nc4 ) echo $spfname >> $acqfile
       if (! -e ACQedFiles/$lfname.nc4 ) echo $lpfname >> $acqfile
       echo $sfname >> sfcst4berr.txt
       echo $lfname >> lfcst4berr.txt
       # next initial day
       set inidate = ( `tick $inidate $gap_sc` )
    end
   
    set lst = `cat $acqfile`
    if (-e missing.txt ) /bin/rm missing.txt
    touch  missing.txt
    foreach fn ($lst)
       if (! -e $fn ) echo $fn >> missing.txt
    end
    if ( $DMGET ) then
      echo $lst
      dmget $lst
      exit
    endif

#  Launch acquire job to retrieve forecast files
#  ---------------------------------------------
   if ( ! -z $acqfile && $DO_ACQUIRE ) then
     setenv JOBGEN_NCPUS          1
     setenv JOBGEN_NCPUS_PER_NODE 1
     jobgen.pl \
       -expid $EXPID         \
       -q datamove           \
       acq_fcst              \
       $GID                  \
       $FCSTACQ_WALLCLOCK    \
       "acquire -v -rc $acqfile -d $FCSTWRK $spool -ssh $vnymd0 $vnhms0 060000 1" \
       $FCSTWRK             \
       $MYNAME               \
       $FCSTWRK/.DONE_${MYNAME}.$vyyyymmddhh0 \
       "Acquire existing Fcsts Failed"

       if ( -e acq_fcst.j ) then
          if ( $ATMENS_BATCHSUB == "sbatch" ) then
             $ATMENS_BATCHSUB -W acq_fcst.j
          else
             $ATMENS_BATCHSUB -W block=true acq_fcst.j
          endif
       else
          echo " ${MYNAME}: Failed to generate PBS jobs to acquire existing Forecast Files. Aborting ... "
          touch $FVWORK/.FAILED
          exit(1)
       endif

     # relocate files just acquired
     # ----------------------------
     /bin/mv $EXPID.prog.eta*  ACQedFiles
   
   endif
 
  endif # SKIPSETTING

endif # GET_SET

if ( $GEN_NMCDIFFS ) then
  echo "***********************************"
  echo "Generating NMC-method perturbations"
  echo "***********************************"
  cd $FCSTWRK
  if (! -d NMC48m24 ) mkdir NMC48m24
  set thistag = ${vnymd0}_00
  set newdate = ( $vnymd0 $vnhms0 )
  @ nc = 0; @ np = 0
  while ($nc < $nsamples)
     
     set fls = (`ls ACQedFiles/*+${thistag}*`)
     set sfx1 = `echo $fls[1] | cut -d+ -f2-`
     set sfx2 = `echo $fls[2] | cut -d+ -f2-`
     if ( $sfx1 != $sfx2 ) then
         echo "Something is a missing, $fls"
         exit (1)
     endif
     set sfx = `echo $sfx1 | cut -d. -f1`
     if ($#fls == 2) then
        if (! -e NMC48m24/$EXPID.f48m24_rh.eta.$sfx.nc4 ) then
           if ( $np < $MYNCPUS ) then
               $DRYRUN dyndiff.x -g5 -addrh -2 $fls -o NMC48m24/$EXPID.f48m24_rh.eta.$sfx.nc4 &
              @ np++ 
              if ( $np == $MYNCPUS ) then
                 wait
                 @ np = 0
              endif
           endif
        endif
     else
        echo "missing files for ${thistag}"
     endif
     set newdate = `tick ${newdate} $oned_sc`
     set thistag = $newdate[1]
     @ nc++
  end
# set slst = `cat $FCSTWRK/sfcst4berr.txt`
# set llst = `cat $FCSTWRK/lfcst4berr.txt`
# set nall = $#slst
# echo "number of samples to process: $nall[1]"
# @ nc = 0; @ np = 0
# while ($nc < $nall[1]) 
#    @ nc++
#    set pfx = `echo $slst[$nc] | cut -d. -f1`
#    set sfx = `echo $slst[$nc] | cut -d+ -f2-`
#    if ( -e ACQedFiles/$slst[$nc].nc4 && -e ACQedFiles/$llst[$nc].nc4 ) then
#       if (! -e NMC48m24/$pfx.f48m24.eta.$sfx.nc4 ) then
#          if ( $np < $MYNCPUS ) then
#             $DRYRUN dyndiff.x -g5 -addrh -2 ACQedFiles/$llst[$nc].nc4 ACQedFiles/$slst[$nc].nc4 -o NMC48m24/$pfx.f48m24_rh.eta.$sfx.nc4 &
#             @ np++ 
#             if ( $np == $MYNCPUS ) then
#                wait
#                @ np = 0
#             endif
#          endif
#       endif
#    else
#       echo "missing either $slst[$nc] or $llst[$nc]"
#    endif
# end
  wait
  exit(0)
endif # GEN_NMCDIFFS

if ( $GET_BERROR ) then
 foreach NLAT ( $these_lats )

  setenv HFAC -1.0  
  if ( $NLAT == 25 ) then
     setenv JCAP  12
     setenv NLON  48
  endif
  if ( $NLAT == 46 ) then
     setenv JCAP  42
     setenv NLON  72
  endif
  if ( $NLAT == 91 ) then
     setenv JCAP   84
     setenv NLON  144
  endif
  if ( $NLAT == 181 ) then
     setenv JCAP  142
     setenv NLON  288
  endif
  if ( $NLAT == 361 ) then
     setenv JCAP  382
     setenv NLON  576
  endif
  if ( $NLAT == 721 ) then
     setenv JCAP   512
     setenv NLON  1152
     setenv HFAC   0.6
  endif
# Hack: use same JCAP  for all resolution as done in previous version of BERROR generation
# Hack: also redefine NMODES as used in previous version
#  see: fvInput/gsi/etc/berror_gmao/gmao24Jun2016_fp+oz_fix/README
  setenv JCAP 268
  setenv BERROR_NMODES 20

  if ( ! -d $FCSTWRK/BERROR.WORK ) mkdir -p $FCSTWRK/BERROR.WORK

   # Get positioned and set namelist parameters
   # ------------------------------------------
   cd $FCSTWRK/BERROR.WORK/

   mkdir samples
   cd samples
   ln -sf $FCSTWRK/NMC48m24/*f48m24*nc4 .
   cd -

   # Get set to run berror stats code
   # --------------------------------
   if (-e infiles ) /bin/rm infiles
#  cat $FCSTWRK/shrt_fcst.txt >> infiles
#  cat $FCSTWRK/long_fcst.txt >> infiles
   ls samples/*f48m24*nc4 > infiles
   ln -sf infiles fort.10

   # Figure out dimensions of forecast files (assumes that checking one suffices)
   # ----------------------------------------------------------------------------
   set fcst_files = (`cat infiles`)
   set fcst_res = (`getgfiodim.x $fcst_files[1]`)

   # wire for now
   set NSIG   = $fcst_res[3]

   # Prepare resource files
   set this_param = $FVROOT/etc/berror_stats.nml.tmpl
   if ( -e $this_param ) then
      if ( -e stats.param ) /bin/rm  stats.parm
      if ( -e sed_file    ) /bin/rm  sed_file
      echo "s/>>>JCAP<<</${JCAP}/1"       > sed_file
      echo "s/>>>HFAC<<</${HFAC}/1"      >> sed_file
      echo "s/>>>NSIG<<</${NSIG}/1"      >> sed_file
      echo "s/>>>NLAT<<</${NLAT}/1"      >> sed_file
      echo "s/>>>NLON<<</${NLON}/1"      >> sed_file
      echo "s/>>>NMODES<<</${BERROR_NMODES}/1"  >> sed_file
      sed -f sed_file  $this_param  > ./stats.parm
      /bin/rm sed_file
   else
      echo "${MYNAME}: cannot find $this_param ..."
      exit (1)
   endif

   # Set "boundary conditions" ...
   ln -sf $FVHOME/fvInput/Static/fvgsi/etc/sst2dvar_stat0.5 berror_sst
   #ln -sf /discover/nobackup/aelakkra/BKGERR/addoz/l72x576y361.berror_stats_psoz.bin oz.dat

   $MPIRUN_CALCSTATS

   if ( -e gsi.berror_stats ) then
      /bin/mv gsi.berror_stats $EXPID.gsi.berror_stats.clim.bin
      foreach fn ( balvar_sp \
                   berror_stats \
                   bgstats_sp \
                   sststats_sp \
                   stst_sp \
                   tst_sp )
          if ( -e $fn.grd ) then
             /bin/mv $fn.grd $EXPID.gsi.$fn.clim.grd
          endif
      end
      if ( -e $FCSTWRK/$EXPID.gsi.berror_stats.clim.y$NLAT.tar ) /bin/rm $FCSTWRK/$EXPID.gsi.berror_stats.clim.y$NLAT.tar
      tar cvf $FCSTWRK/$EXPID.gsi.berror_stats.clim.y$NLAT.tar  $EXPID.gsi.berror_stats.clim.bin \
                                                                $EXPID.gsi.*clim.grd
      
      echo " ${MYNAME}: done creating B-error file"

   else
      echo " ${MYNAME}: failed to create B-error file, aborting ..."
      exit 4
   endif

   /bin/rm -r fort.*00*
   /bin/rm -r fort.*01*
   /bin/rm -r fort.*02*
   /bin/rm stats.parm
   /bin/rm $EXPID.gsi.berror_stats.clim.bin $EXPID.gsi.*clim.grd

 end # these_lats
endif # GET_BERROR
