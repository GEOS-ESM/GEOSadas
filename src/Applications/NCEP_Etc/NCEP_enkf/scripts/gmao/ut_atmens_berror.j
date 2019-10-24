#!/bin/csh -x
# This needs to run under SLURM/PBS

setenv EXPID prePP_rt
setenv FVHOME /discover/nobackup/projects/gmao/advda/$user/$EXPID
setenv FVROOT $SWDEV/Latest/g516plus/GEOSadas/Linux
setenv FVWORK $PRJ/atmens_berror.HIRES
setenv EXPARCH $ARCHIVE/$EXPID
setenv NCSUFFIX nc4
setenv NMEM 32
setenv ATMENS_VERBOSE 1

# Env required by acquire script ...
setenv GID g0613
setenv TIMEINC 360
setenv VAROFFSET 180

setenv GETDATA 1
setenv INPUTDATA $PRJ/$EXPID/atmens
setenv INPUTDATA /dev/null
setenv INPUTDATA $FVWORK

set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

#setenv MPIRUN_CALCSTATS "mpirun -np 96 calcstats.x"
setenv MPIRUN_CALCSTATS "mpirun -np 96 calcstats.x"

set nymd = 20160328
set nhms =   060000
set nmem = $NMEM
set   hh = `echo $nhms | cut -c1-2`

mkdir -p $FVWORK
cd $FVWORK

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

# unfold tar balls
if ( -e $EXPID.atmens_stat.${nymd0}_${hh0}z.tar ) then
   tar -xvf $EXPID.atmens_stat.${nymd0}_${hh0}z.tar $EXPID.atmens_stat.${nymd0}_${hh0}z/ensmean/$EXPID.bkg.eta.${nymd}_${hh}z.$NCSUFFIX
else
   echo "ERROR, failed to find stats tar ball, aborting ..."
   exit (1)
endif
if ( -d atmens ) then
   ln -s atmens/mem* .
else
   if ( -d $EXPID.atmens_ebkg.${nymd0}_${hh0}z.tar ) then
     @ n = 0
     while ( $n < $nmem )
        @ n = $n + 1
        set nnn = `echo $n | awk '{printf "%03d", $1}'`
        tar -xvf $EXPID.atmens_ebkg.${nymd0}_${hh0}z.tar $EXPID.atmens_ebkg.${nymd0}_${hh0}z/mem$nnn/$EXPID.bkg.eta.${nymd}_${hh}z.$NCSUFFIX
     end
   else
      echo "ERROR, failed to find bkg tar ball, aborting ..."
      exit (2)
   endif
endif

# link ensemble information to work dir ...
ln -s $EXPID.atmens_stat.${nymd0}_${hh0}z/ensmean .
ln -s $EXPID.atmens_ebkg.${nymd0}_${hh0}z/mem*    .

# generate parameterized B-error based on ensemble ...
atmens_berror.csh $EXPID $nymd $nhms $FVWORK $FVWORK

if ( -e "$EXPID.gsi.berror_stats.${nymd}_${hh}z.tar" ) then
   echo "All good, output under: $EXPID.gsi.berror_stats.${nymd}_${hh}z.tar"
   exit (0)
else
   echo "Failed to produced desired results ... "
   exit (99)
endif
