#!/bin/csh

setenv dry_run #echo 
setenv SIMULATE_ENSEMBLE 0
setenv ATMENS_VERBOSE 1
setenv DOANA 0
setenv NEW  1
setenv STATCASE arec
setenv GET_TAR_BATCH 1
setenv ATMENS_BATCHSUB sbatch

# to run pegcm stats in parallel
setenv PEGCM_NCPUS 4
setenv PEGCM_WALLCLOCK 0:30:00
setenv PEGCM_QNAME nccs2

setenv ATMENS_BATCHSUB qsub
setenv EXPID prePP_rt
setenv FVHOME /discover/nobackup/projects/gmao/advda/$user/$EXPID
setenv EXPID x0028A_rt
setenv FVHOME /discover/nobackup/projects/gmao/obsdev/$user/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
if ( $STATCASE == "arec" ) then
   setenv WORK   /discover/nobackup/projects/gmao/advda/$user/AREC_STATS
else
   setenv WORK   /discover/nobackup/projects/gmao/advda/$user/BREC_STATS
endif
setenv ATMENSETC $FVHOME/run/atmens
setenv TIMEINC 360
setenv ASYNBKG 180

setenv GID g0613
setenv ENSPARALLEL 1
#setenv AENSTAT_NCPUS 28
setenv AENSTAT_NCPUS 4
setenv AENSTAT_WALLCLOCK 0:30:00
setenv AENSTAT_MPIRUN "mpirun -np $AENSTAT_NCPUS mp_stats.x"
setenv AENSTAT_QNAME nccs1

setenv MYNAME ut_rec_stats.csh

if( $SIMULATE_ENSEMBLE ) setenv dry_run echo

set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

set nsyn = 12
set nmem = 32
@ anaoffset = 3 * 3600
@ anatau = 6 * 3600

# Initial date/time
# -----------------
#set nymda = 20171124
set nymda = 20160720
set nhmsa = 060000


# loop over number of days to process
# -----------------------------------
@ ns = 0
while ( $ns < $nsyn )
  @ ns++

  mkdir -p $WORK
  cd $WORK
  setenv ENSWORK $WORK/$nymda.$nhmsa
  mkdir -p $ENSWORK
  cd $ENSWORK
  mkdir ensmean ensrms

  set buf  = ( `tick $nymda $nhmsa -$anaoffset` )

  set nymd0 = $buf[1]
  set nhms0 = $buf[2]
  set yyyy0 = `echo $buf[1] | cut -c1-4`
  set   mm0 = `echo $buf[1] | cut -c5-6`
  set   hh0 = `echo $buf[2] | cut -c1-2`
  set yyyymmddhh = ${nymd0}${hh0}
  set fname = $EXPID.atmens_eana_${STATCASE}.${nymd0}_${hh0}z
  set sname = $EXPID.atmens_stat_${STATCASE}.${nymd0}_${hh0}z

  if ( $GET_TAR_BATCH ) then
    if (! -e $EXPID.atmens_eana_${STATCASE}.${nymd0}_${hh0}z.tar ) then
      if (! -e my.rc) then
cat >> my.rc << EOF
/archive/u/$user/$EXPID/atmens/Y$yyyy0/M$mm0/$EXPID.atmens_eana_${STATCASE}.%y4%m2%d2_%h2z.tar
EOF
      endif

      set spool = "-s $ENSWORK/spool"
      jobgen.pl \
             -q datamove \
             getensana           \
             $GID                \
             $AENSTAT_WALLCLOCK  \
             "acquire -v -strict -rc ./my.rc  -d $cwd $spool -ssh $nymd0 $nhms0 060000 1" \
             $cwd                \
             $MYNAME             \
             $ENSWORK/.DONE_MEM001_GETENSANA.$yyyymmddhh \
             "Main job script Failed for Get Ensemble Analysis"

             if ( -e getensana.j ) then
                if ( $ATMENS_BATCHSUB == "sbatch" ) then
                   $ATMENS_BATCHSUB  -W getensana.j
                else
                   $ATMENS_BATCHSUB  -W block=true getensana.j
                endif
                touch .SUBMITTED
             else
                echo " $MYNAME: Failed for Get Ensemble Analysis, Aborting ... "
                touch $ENSWORK/.FAILED
                exit(1)
             endif

    endif
    tar xvf $fname.tar
  else
    tar xvf $ARCHIVE/$EXPID/atmens/Y$yyyy0/M$mm0/$fname.tar
  endif

  $dry_run atmens_stats.csh $nmem ana.eta $ENSWORK/ $nymda $nhmsa

  /bin/mkdir $sname
  /bin/cp -r ens* $sname
  tar cvf $sname.tar $sname
  /bin/rm -r $sname
  /bin/rm -r mem* $fname.tar
  
  cd -
  set buf  = ( `tick $nymda $nhmsa $anatau` )
  set nymda = $buf[1]
  set nhmsa = $buf[2]

end
