#!/bin/csh

setenv dry_run #echo 
setenv SIMULATE_ENSEMBLE 0
setenv ATMENS_VERBOSE 1
setenv DOANA 0

setenv JOBGEN_QOS advda
#setenv JOBGEN_PARTITION preops
setenv JOBGEN_CONSTRAINT hasw

# to run pegcm stats in parallel
setenv PEGCM_ALLPARALLEL 1
setenv PEGCM_ARRAY 1
setenv PEGCM_WALLCLOCK 1:30:00
setenv PEGCM_QNAME compute

setenv EXPID m21c_j98
setenv FVHOME /gpfsm/dnb05/projects/p139/$user/M21C/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv ENSWORK $FVHOME
setenv ATMENSETC $FVHOME/run/atmens
setenv TIMEINC 360
setenv ASYNBKG 180

setenv ATMENS_BATCHSUB sbatch
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

  $dry_run atmens_stats.csh $nmem ana.eta spread $ENSWORK/atmens $nymda $nhmsa

else

  set nymdb = 19971231
  set nhmsb = 210000
  set hhb   = `echo $nhmsb | cut -c1-2`

  post_egcm.csh $EXPID $nymdb $nhmsb $TIMEINC spread $ATMENSETC/post_egcm.rc $FVHOME/atmens
  if ($status) then
     echo "post_egcm (bkg) failed"
     exit(1)
  endif
  post_egcm.csh $EXPID $nymdb $nhmsb 0 variance $ATMENSETC/post_egcm_diag.rc $FVHOME/atmens/ensdiag
  if ($status) then
     echo "post_egcm (diag) failed"
     exit(1)
  endif

endif # <DOANA>
