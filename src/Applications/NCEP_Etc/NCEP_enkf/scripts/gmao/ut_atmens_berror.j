#!/bin/csh -x
# *********************************
# *********************************
# This needs to run under SLURM/PBS
# *********************************
# *********************************

setenv EXPID x0041
setenv FVHOME /discover/nobackup/projects/gmao/obsdev/$user/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv FVWORK $ODV/atmens_berror.VRES2
setenv EXPARCH $ARCHIVE/$EXPID
setenv NCSUFFIX nc4
setenv NMEM 32
setenv ATMENS_VERBOSE 1

# Env required by acquire script ...
setenv ATMENS_BATCHSUB sbatch
setenv GID g0613
setenv TIMEINC 360
setenv VAROFFSET 180

setenv GETDATA 1
setenv INPUTDATA $PRJ/$EXPID/atmens
setenv INPUTDATA $FVWORK
setenv INPUTDATA /dev/null

set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

setenv MPIRUN_CALCSTATS "mpirun -np 96 calcstats.x"
#setenv MPIRUN_CALCSTATS "mpiexec_mpt -np 96 calcstats.x"

set nymd = 20191126
set nhms =   000000
set nmem = $NMEM
set   hh = `echo $nhms | cut -c1-2`

mkdir -p $FVWORK
cd $FVWORK
if ( -e $FVHOME/run/atmens/atmens_berror.rc ) /bin/cp $FVHOME/run/atmens/atmens_berror.rc .

if ( $INPUTDATA != "/dev/null" ) then
   # generate parameterized B-error based on ensemble ...
   atmens_berror.csh $EXPID $nymd $nhms $INPUTDATA $FVWORK
   if ( -e "$EXPID.gsi.berror_stats.${nymd}_${hh}z.tar" ) then
      echo "All good, output under: $EXPID.gsi.berror_stats.${nymd}_${hh}z.tar"
      exit (0)
   else
      echo "Failed to produced desired results ... "
      exit (99)
   endif
endif

# acquire ensemble information from archive
@ varoffset_sc = 60 * $VAROFFSET
set beg_ana = `tick $nymd $nhms -$varoffset_sc`
set nymd0 = $beg_ana[1]
set nhms0 = $beg_ana[2]
set yyyy0 = `echo $nymd0 | cut -c1-4`
set   mm0 = `echo $nymd0 | cut -c5-6`
set   hh0 = `echo $nhms0 | cut -c1-2`

if ( -e $EXPID.atmens_stat.${nymd0}_${hh0}z.tar && -e $EXPID.atmens_ebkg.${nymd0}_${hh0}z.tar ) setenv GETDATA 0
cat << EOF >! my.acq
$EXPARCH/atmens/Y%y4/M%m2/$EXPID.atmens_stat.%y4%m2%d2_%h2z.tar
$EXPARCH/atmens/Y%y4/M%m2/$EXPID.atmens_ebkg.%y4%m2%d2_%h2z.tar
EOF
if ( $GETDATA ) then
   acquire_atmens.csh $EXPID $nymd $nhms my.acq
endif

# create local links
if ( -d atmens ) then
   if ( -d atmens/ensmean ) then
      ln -s atmens/ensmean .
   else
      echo "ERROR, failed to find dir w/ ens-mean, aborting ..."
      exit (2)
   endif
   if ( -d atmens/mem001 ) then
      ln -s atmens/mem*    .
   else
      echo "ERROR, failed to find dirs w/ ens-members, aborting ..."
      exit (2)
   endif
else
   echo "ERROR, failed to set atmens dir, aborting ..."
   exit (2)
endif

# generate parameterized B-error based on ensemble ...
atmens_berror.csh $EXPID $nymd $nhms $FVWORK $FVWORK

if ( -e "$EXPID.gsi.berror_stats.${nymd}_${hh}z.tar" ) then
   echo "All good, output under: $EXPID.gsi.berror_stats.${nymd}_${hh}z.tar"
   exit (0)
else
   echo "Failed to produced desired results ... "
   exit (99)
endif
