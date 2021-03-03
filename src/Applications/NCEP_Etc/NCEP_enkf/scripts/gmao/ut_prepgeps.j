#!/bin/csh

setenv ATMENS_BATCHSUB qsub
setenv EXPID x0026
setenv FVHOME /discover/nobackup/projects/gmao/obsdev/$user/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv BIGNAME `echo "$EXPID" | tr -s '[:lower:]' '[:upper:]'`
setenv DATADIR /archive/u/rtodling

setenv AENSTAT_NCPUS 4
setenv AENSTAT_MPIRUN "mpiexec_mpt -np $AENSTAT_NCPUS mp_stats.x"

setenv FVWORK /discover/nobackup/projects/gmao/obsdev/$user/ensgepswork.$BIGNAME
setenv ATMENSETC $FVHOME/run/atmens/

set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

setenv ATMENS_GEPS_RECENTER 1

unsetenv PMI_RANK
unsetenv PMI_FD
unsetenv PMI_JOBID
unsetenv PMI_SIZE

set nymd  = 20161213
set nhms  = 000000
set atype = ana
set action = setrc

atmens_prepgeps.csh $EXPID $nymd $nhms $atype $action

#atmens_prepgeps.csh $EXPID $nymd $nhms $atype NULL

