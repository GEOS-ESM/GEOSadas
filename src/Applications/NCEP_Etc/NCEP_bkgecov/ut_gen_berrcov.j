#!/bin/csh 
#SBATCH --account=g0613
#SBATCH --constraint=sky
#SBATCH --ntasks=1
#_SBATCH --ntasks=24
#_SBATCH --ntasks=96
#_SBATCH --ntasks=384
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
# 1) when the 24- and 48-hour forecasts are valid at the same time
#    and therefore are obtained from two independent forecasts 
#    issues from two analyses 24 hours appart.
#
# and
#
# 2) when the 24- and 48-hour forecasts are obtained from the 
#    same initial condition (i.e., single forecast), and therefore
#    the fields are not verified at the same time.
# 
# Clearly, (1) is the correct way of constructing the 24-minus-48
# hour differences needed by the NMC method, and (2) is not. If 
# nothing else, (2) is physically inconsistent since it diffs 
# states valid at different times. However, (2) is what the FORTRAN
# code expects; it then does another odd pairing inside to match file
# correctly! Go figure.
#
# R. Todling, 18 April 2020
#
###################################################################

 setenv DRYRUN #echo
 setenv SKIPSETTING 0
 setenv MYNCPUS 10
# Set initial time and number of samples required
 setenv EXPID x0039_p5_REPLAY_L132
 setenv EXPID x41Lrt
 setenv EXPID x0041
 setenv EXPID f525_fp
 setenv EXPID f525_p5_fp
 setenv EXPID f522_fp
 setenv EXPID f521_fp
#setenv FVHOME /home/dao_ops/$EXPID
#setenv ARCROOT /home/dao_ops/$EXPID/run/.../archive/prog
#setenv FVHOME /discover/nobackup/projects/gmao/obsdev/rtodling/$EXPID
setenv FVHOME /discover/nobackup/projects/gmao/obsdev/rtodling/x0041
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv PLAINDIR 0
setenv ARCROOT /archive/u/$user/$EXPID/prog
if ( $EXPID == "x0041" ) then
   setenv ARCROOT /archive/u/dao_it/$EXPID/prog
endif
if ( $EXPID == "f521_fp" || $EXPID == "f522_fp" || $EXPID == "f525_fp" || $EXPID == "f525_p5_fp" ) then
   setenv ARCROOT /home/dao_ops/$EXPID/run/.../archive/prog
endif
if ($EXPID == "x0039_p5_REPLAY_L132") then
   setenv ARCROOT /gpfsm/dnb78s1/projects/p18/ltakacs/REPLAY_Experiments/x0039_p5_REPLAY_L132/forecasts/Regular_RPLFRQ:7200_ANA:x0039_p5
   setenv PLAINDIR 1
endif
setenv DMGET 0
setenv DO_ACQUIRE  1
setenv GET_SET     1
setenv ODD_PAIRING_OF_FCSTS 1
setenv GEN_NMCDIFFS 1
setenv RESET_TIMES 0

setenv BERROR_NMODES 25

if ( $GEN_NMCDIFFS ) then
   setenv ODD_PAIRING_OF_FCSTS 0 # NMC perturbations are created from proper verification time pairs 
endif

# Basic settings (weak dependency on version of DAS)
# --------------------------------------------------
set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

setenv MYNAME ut_gen_berrcov.j
setenv ATMENS_BATCHSUB sbatch
setenv GID g0613
setenv FCSTACQ_WALLCLOCK 2:00:00
setenv FCSTWORK /discover/nobackup/projects/gmao/obsdev/$user/fcst4berrcov.$EXPID

if ($?I_MPI_ROOT ) then
  setenv MPIRUN_CALCSTATS "mpirun -np 32 calcstats.x"
else
  setenv MPIRUN_CALCSTATS "mpirun_exec -np 32 calcstats.x"
endif
#
# initial verification date and number of samples (will get samples ahead of initial date)
set vnymd0 = 20191125
set vnymd0 = 20181201
set vnymd0 = 20200303
set vnymd0 = 20200410
set vnymd0 = 20181101
#if ( $EXPID == "f522_fp") then
#   set vnymd0 = 20190304
#   @ nsamples = 350 + $ODD_PAIRING_OF_FCSTS  # need one extra fcst to get same number of samples between odd and correctly linked files
#   set vnymd0 = 20200206
#   @ nsamples = 23  + $ODD_PAIRING_OF_FCSTS  # need one extra fcst to get same number of samples between odd and correctly linked files
#
#endif
set vnhms0 = 000000
@ nsamples =  32 + $ODD_PAIRING_OF_FCSTS  # need one extra fcst to get same number of samples between odd and correctly linked files
setenv FCSTWRK $FCSTWORK.$vnymd0.$vnhms0
mkdir -p $FCSTWRK
set diren = `dirname $FCSTWRK`
set spool = "-s $diren/spool "

if ( $GET_SET ) then

# Just in case things need to be reset by hand - typically not active
# -------------------------------------------------------------------
if ( $RESET_TIMES ) then
   cd $FCSTWRK/BERROR.WORK/samples
   foreach fn (`ls *+48`)
      set ttag = `echo $fn | cut -d. -f2`
      set this_nymd = `echo $ttag | cut -c1-8`
      set this_hh   = `echo $ttag | cut -c10-11`
      reset_time.x $fn $this_nymd ${this_hh}0000 -9
   end
   cd -
   exit
endif
set foffset_hr = 3  # offset in initial time from synoptic hour
set nmc_hrv = 24 # begin time of NMC method / verification time
set nmc_hrf = 48 #   end time of NMC method / fcst time
set vmn = 00 # either black or 00
set vmn =    # either black or 00

# ----------------------------------
# No user change from this part down
# ----------------------------------
@ foffset_sc = $foffset_hr * 3600 
@ gap_sc  = $nmc_hrv * 3600
@ oned_sc = 24 * 3600
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
   if ( $ODD_PAIRING_OF_FCSTS ) then
      set ldate = ($sdate)
      set wdate = (`tick $inidate $oned_sc`)
      set wyyyy = `echo $wdate[1] | cut -c1-4`
      set wmm   = `echo $wdate[1] | cut -c5-6`
      set wdd   = `echo $wdate[1] | cut -c7-8`
      set whh   = `echo $wdate[2] | cut -c1-2`
      set vtmpl = $wdate[1]_${whh}${vmn}z
   else
      set ldate = ( `tick $inidate -$nmc_scf` )
   endif
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

if ( $DMGET ) then
  set lst = `cat $acqfile`
  echo $lst
# dmget $lst
  exit
endif

# Launch acquire job to retrieve forecast files
# ---------------------------------------------
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

endif # SKIP setting

if ( $GEN_NMCDIFFS ) then
  echo "***********************************"
  echo "Generating NMC-method perturbations"
  echo "***********************************"
  cd $FCSTWRK
  if (! -d NMC48m24 ) mkdir NMC48m24
  set slst = `cat $FCSTWRK/sfcst4berr.txt`
  set llst = `cat $FCSTWRK/lfcst4berr.txt`
  set nall = $#slst
  echo "number of samples to process: $nall[1]"
  @ nc = 0; @ np = 0
  while ($nc < $nall[1]) 
     @ nc++
     set pfx = `echo $slst[$nc] | cut -d. -f1`
     set sfx = `echo $slst[$nc] | cut -d+ -f2-`
     if ( -e ACQedFiles/$slst[$nc].nc4 && -e ACQedFiles/$llst[$nc].nc4 ) then
        if (! -e NMC48m24/$pfx.f48m24.eta.$sfx.nc4 ) then
           if ( $np < $MYNCPUS ) then
              $DRYRUN dyndiff.x -g5 ACQedFiles/$llst[$nc].nc4 ACQedFiles/$slst[$nc].nc4 -o NMC48m24/$pfx.f48m24.eta.$sfx.nc4 &
              @ np++ 
              if ( $np == $MYNCPUS ) then
                 wait
                 @ np = 0
              endif
           endif
        endif
     else
        echo "missing either $slst[$nc] or $llst[$nc]"
     endif
  end
  wait
  exit(0)
endif

# Make sure aquired files are paired up
# -------------------------------------
cd ACQedFiles
if ( $ODD_PAIRING_OF_FCSTS ) then
   # paired by initial time
   set vdate = ( $vnymd0 $vnhms0 )
   set idate = ( `tick $vdate -$nmc_scv` )
   @ nc = 0
   while ($nc < $nsamples) 
      @ nc++
      set nymd =  $idate[1] 
      set hh   = `echo $idate[2] | cut -c1-2`
      set lsfn = (`ls $EXPID.prog.eta.${nymd}_${hh}z+????????_??${vmn}z.nc4`)
      if ($#lsfn == 1 ) then
         echo "will remove $lsfn"
         /bin/rm $lsfn
         grep -v $lsfn $FCSTWRK/sfcst4berr.txt > $FCSTWRK/stmp.txt
         /bin/mv $FCSTWRK/stmp.txt $FCSTWRK/sfcst4berr.txt 
         grep -v $lsfn $FCSTWRK/lfcst4berr.txt > $FCSTWRK/ltmp.txt
         /bin/mv $FCSTWRK/ltmp.txt $FCSTWRK/lfcst4berr.txt 
      endif
      # next initial day
      set idate = ( `tick $idate $gap_sc` )
   end
else
   # paired by verification time
   set vdate  = ( $vnymd0 $vnhms0 )
   @ nc = 0
   while ($nc < $nsamples) 
      @ nc++
      echo "case $nc of $nsamples"
      set nymd =  $vdate[1] 
      set hh   = `echo $vdate[2] | cut -c1-2`
      set lsfn = ( `ls $EXPID.prog.eta.????????_??z+${nymd}_${hh}${vmn}z.nc4` )
      if ($#lsfn == 1) then
         echo "will remove $lsfn"
         /bin/rm $lsfn
         grep -v $lsfn $FCSTWRK/sfcst4berr.txt > $FCSTWRK/stmp.txt
         /bin/mv $FCSTWRK/stmp.txt $FCSTWRK/sfcst4berr.txt 
         grep -v $lsfn $FCSTWRK/lfcst4berr.txt > $FCSTWRK/ltmp.txt
         /bin/mv $FCSTWRK/ltmp.txt $FCSTWRK/lfcst4berr.txt 
      endif
      # next initial day
      set vdate = ( `tick $vdate $gap_sc` )
   end
endif
echo "Complete check on acquired files."
cd -

# Allow testing w/ odd list of files (one less sample)
# ----------------------------------------------------
if ( $ODD_PAIRING_OF_FCSTS ) then
  # remove  last file in list of short-range forecasts
  set lstfns = (`cat sfcst4berr.txt`)
  set nfiles = (`cat sfcst4berr.txt | wc`)
  if(-e odd_sfcst4berr.txt) /bin/rm odd_sfcst4berr.txt
  touch odd_sfcst4berr.txt
  @ nf = 1
  while ($nf < $nfiles[1])
     echo $lstfns[$nf] >> odd_sfcst4berr.txt 
     @ nf++
  end
  # remove last file in list of short-range forecasts
  set lstfns = (`cat lfcst4berr.txt`)
  set nfiles = (`cat lfcst4berr.txt | wc`)
  if(-e odd_lfcst4berr.txt) /bin/rm odd_lfcst4berr.txt
  touch odd_lfcst4berr.txt
  @ nf = 1
  while ($nf < $nfiles[1])
     echo $lstfns[$nf] >> odd_lfcst4berr.txt 
     @ nf++
  end
  # overwrite original list of files
  /bin/cp odd_sfcst4berr.txt sfcst4berr.txt
  /bin/cp odd_lfcst4berr.txt lfcst4berr.txt
endif

# create link to acquired files as required by berror code
# warning: linked name cannot have more than a single "+"
# --------------------------------------------------------
if ( ! -d BERROR.WORK ) mkdir BERROR.WORK
if (   -d BERROR.WORK/samples ) /bin/rm -r BERROR.WORK/samples
mkdir BERROR.WORK/samples
if ( -e shrt_fcst.txt ) /bin/rm shrt_fcst.txt
if ( -e long_fcst.txt ) /bin/rm long_fcst.txt
touch  shrt_fcst.txt long_fcst.txt
cd BERROR.WORK/samples
foreach fn (`cat $FCSTWRK/sfcst4berr.txt`)
  set real_date = `echo $fn | cut -d+ -f2` 
  ln -sf ../../ACQedFiles/$fn.nc4 shrt_fcst.${real_date}+24
  echo samples/shrt_fcst.${real_date}+24 >> $FCSTWRK/shrt_fcst.txt
end
foreach fn (`cat $FCSTWRK/lfcst4berr.txt`)
  set real_date = `echo $fn | cut -d+ -f2` 
  ln -sf ../../ACQedFiles/$fn.nc4 long_fcst.${real_date}+48
  echo samples/long_fcst.${real_date}+48 >> $FCSTWRK/long_fcst.txt
end
echo "Create links in sample directory"

# In case the time sequence is not continuous there will be dead links ...
# ... clear out dead links
# ------------------------
@ ns = 0
/bin/cp $FCSTWRK/shrt_fcst.txt $FCSTWRK/shrt_fcst.orig.txt 
foreach fn (`ls *+24`)
   if ( ! -e $fn ) then
      /bin/rm $fn
      @ ns++
      grep -v $fn $FCSTWRK/shrt_fcst.txt > $FCSTWRK/stmp.txt
      /bin/mv $FCSTWRK/stmp.txt $FCSTWRK/shrt_fcst.txt
   endif
end
echo "Remove dead links related to short-fcsts"
/bin/cp $FCSTWRK/long_fcst.txt $FCSTWRK/long_fcst.orig.txt 
@ nl = 0
foreach fn (`ls *+48`)
   if ( ! -e $fn ) then
      /bin/rm $fn
      @ nl++
      grep -v $fn $FCSTWRK/long_fcst.txt > $FCSTWRK/ltmp.txt
      /bin/mv $FCSTWRK/ltmp.txt $FCSTWRK/long_fcst.txt
   endif
end
echo "Remove dead links related to long-fcsts"
#
if ( $nl == $ns ) then
   echo "Time series of fcsts not continuous, removed dead link ..."
   echo "Original nsamples = $nsamples"
   @ nsamples = $nsamples - $ns
   echo "Redefined nsamples =  $nsamples"
else
   echo "Samples are not paired up, trying to fix it ..."
   foreach fn ( `cat $FCSTWRK/shrt_fcst.txt`) 
      set in_shrt = `echo $fn | cut -d+ -f1 | cut -d. -f2`
      # check to see if in long_fcst.txt
      grep $in_shrt $FCSTWRK/long_fcst.txt
      if ($status) then
         grep -v $fn $FCSTWRK/shrt_fcst.txt > $FCSTWRK/stmp.txt
         /bin/mv $FCSTWRK/stmp.txt $FCSTWRK/shrt_fcst.txt
      endif
   end
   foreach fn ( `cat $FCSTWRK/long_fcst.txt`) 
      set in_long = `echo $fn | cut -d+ -f1 | cut -d. -f2`
      # check to see if in shrt_fcst.txt
      grep $in_long $FCSTWRK/shrt_fcst.txt
      if ($status) then
         grep -v $fn $FCSTWRK/long_fcst.txt > $FCSTWRK/ltmp.txt
         /bin/mv $FCSTWRK/ltmp.txt $FCSTWRK/long_fcst.txt
      endif
   end
   @ nnl = `cat $FCSTWRK/long_fcst.txt`
   @ nns = `cat $FCSTWRK/shrt_fcst.txt`
   if ( $nnl[1] == $nns[1] ) then
       echo "Time series of fcsts not continuous, removed dead link ..."
       echo "Original nsamples = $nsamples"
       @ nsamples = $nsamples - $nns[1]
       echo "Redefined nsamples =  $nsamples"
   else
       echo "Time series has problems, failed to match samples, aborting ..."
       exit (1)
   endif
endif

#echo "premature exit for debugging"
#exit

cd $FCSTWRK/BERROR.WORK

# calcstats does not pair fcsts valid at the same time (as it should), 
# instead it expects that pairs of forecasts have a 48-24 hr gap between 
# them; or f-b hours, so the following artificially changes the date of
# the f-hr forecasts.
if ( ! $ODD_PAIRING_OF_FCSTS ) then
   echo "resetting time in longer-range fcst files"
   foreach fn (`cat $FCSTWRK/long_fcst.txt`)
      set ttag = `echo $fn | cut -d. -f2`
      set this_nymd = `echo $ttag | cut -c1-8`
      set this_hh   = `echo $ttag | cut -c10-11`
      set fake_date = ( `tick $this_nymd ${this_hh}0000 $gap_sc` )
      reset_time.x $fn $fake_date[1] $fake_date[2] -9
   end
endif

endif # GET_SET

# Get positioned and set namelist parameters
# ------------------------------------------
cd $FCSTWRK/BERROR.WORK/

# Get set to run berror stats code
# --------------------------------
if (-e infiles ) /bin/rm infiles
cat $FCSTWRK/shrt_fcst.txt >> infiles
cat $FCSTWRK/long_fcst.txt >> infiles
ln -sf infiles fort.10


# just for the record ...
ls -lrt samples

# Figure out dimensions of forecast files (assumes that checking one suffices)
# ----------------------------------------------------------------------------
set fcst_files = (`cat samples`)
set fcst_res = (`getgfiodim.x $fcst_files[1]`)

# wire for now
set JCAP = 512
set NSIG   = $fcst_res[3]
set NLON   = 576
set NLAT   = 361

# Prepare resource files
set this_param = $FVROOT/etc/berror_stats.nml.tmpl
if ( -e $this_param ) then
   if ( -e stats.param ) /bin/rm  stats.parm
   if ( -e sed_file    ) /bin/rm  sed_file
   echo "s/>>>JCAP<<</${JCAP}/1"       > sed_file
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
   if ( -e $FCSTWRK/$EXPID.gsi.berror_stats.clim.tar ) /bin/rm $FCSTWRK/$EXPID.gsi.berror_stats.clim.tar
   tar cvf $FCSTWRK/$EXPID.gsi.berror_stats.clim.tar  $EXPID.gsi.berror_stats.clim.bin \
                                                                $EXPID.gsi.*clim.grd
   
   echo " ${MYNAME}: done creating B-error file"

else
   echo " ${MYNAME}: failed to create B-error file, aborting ..."
   exit 4
endif

/bin/rm -r fort.*00*
/bin/rm -r fort.*01*
/bin/rm -r fort.*02*

# reverse back to original date of f-hr fcsts
if ( ! $ODD_PAIRING_OF_FCSTS ) then
   echo "resetting time in longer-range fcst files back to correct time"
   cd $FCSTWRK/BERROR.WORK
   foreach fn (`cat $FCSTWRK/long_fcst.txt`)
      set ttag = `echo $fn | cut -d. -f2`
      set this_nymd = `echo $ttag | cut -c1-8`
      set this_hh   = `echo $ttag | cut -c10-11`
      reset_time.x $fn $this_nymd ${this_hh}0000 -9
   end
endif
