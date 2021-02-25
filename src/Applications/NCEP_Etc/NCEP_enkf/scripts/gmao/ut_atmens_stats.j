#!/bin/csh

setenv dry_run #echo 
setenv SIMULATE_ENSEMBLE 0
setenv ATMENS_VERBOSE 1
setenv DOANA 0
setenv NEW  1  # when set to 1 will submit little job to queue; other do work on interactve queue

setenv JOBGEN_QOS advda
#setenv JOBGEN_PARTITION preops
setenv JOBGEN_CONSTRAINT hasw

# to run pegcm stats in parallel
setenv PEGCM_NCPUS 4
setenv PEGCM_WALLCLOCK 1:30:00
setenv PEGCM_QNAME compute

setenv EXPID f513a_rt
setenv FVHOME /gpfsm/dnb02/projects/p61/$user/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv ENSWORK $FVHOME
setenv ATMENSETC $FVHOME/run/atmens
setenv TIMEINC 360
setenv ASYNBKG 180

setenv ATMENS_BATCHSUB qsub
setenv GID g0613
setenv ENSPARALLEL 1
setenv AENSTAT_NCPUS 4
setenv AENSTAT_WALLCLOCK 1:00:00
setenv AENSTAT_QNAME compute

setenv MYNAME ut_atmens_stats.csh

if( $SIMULATE_ENSEMBLE ) setenv dry_run echo

set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

if ($?I_MPI_ROOT) then
   setenv ATMENS_MPIRUN "mpirun "
else
   setenv ATMENS_MPIRUN "mpiexec_mpt "
endif
setenv AENSTAT_MPIRUN "$ATMENS_MPIRUN -np $AENSTAT_NCPUS mp_stats.x"

set nmem = 32

if ( $DOANA ) then
  set nymda = 20120401
  set nhmsa = 000000

  $dry_run atmens_stats.csh $nmem ana.eta $ENSWORK/atmens $nymda $nhmsa

else

  set nymdb = 20151211
  set nhmsb = 090000
  set hhb   = `echo $nhmsb | cut -c1-2`

  if ( $NEW ) then
      post_egcm.csh $EXPID $nymdb $nhmsb 0 $FVHOME/atmens
  else

  @ bkgfreq_sec = $ASYNBKG * 60
  @ anafreq_sec = $TIMEINC * 60
  @ iaufreq_sec = 0 #$iaumn   * 60
  @ nt = $anafreq_sec / $bkgfreq_sec + 1
  set adate = ( `tick $nymdb $nhmsb $iaufreq_sec` )
  @ n = 0
  while ( $n < $nt )
     @ n++
     set this_nymd = $adate[1]
     set this_nhms = $adate[2]
     set this_hh   = `echo $this_nhms | cut -c1-2`
     set this_yyyymmddhh = ${this_nymd}${this_hh}
     if (! -e $ENSWORK/.DONE_redone_bkgstat_$MYNAME.$this_yyyymmddhh ) then
       foreach ftype ( bkg.eta bkg.sfc cbkg.eta abkg.eta gaas_bkg.sfc )
          $dry_run atmens_stats.csh $nmem $ftype $ENSWORK/atmens $this_nymd $this_nhms
          if($status) then
             echo "Failed"
             exit(1)
          else
             $dry_run touch $ENSWORK/.DONE_redone_bkgstat_$MYNAME.$this_yyyymmddhh
          endif
       end # foreach
     endif
     set adate = (`tick $this_nymd $this_nhms $bkgfreq_sec`)
  end
  endif # NEW

endif # <DOANA>
