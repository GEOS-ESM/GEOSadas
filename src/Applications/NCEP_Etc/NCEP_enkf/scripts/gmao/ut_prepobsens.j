#!/bin/csh

setenv ATMENS_BATCHSUB qsub
setenv EXPID x0026
setenv FVHOME /discover/nobackup/projects/gmao/obsdev/$user/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv BIGNAME `echo "$EXPID" | tr -s '[:lower:]' '[:upper:]'`
setenv DATADIR /archive/u/rtodling

setenv AENSTAT_NCPUS 4
setenv AENSTAT_MPIRUN "mpiexec_mpt -np $AENSTAT_NCPUS mp_stats.x"

setenv FVWORK /discover/nobackup/projects/gmao/obsdev/$user/ensosenswork.$BIGNAME
setenv ATMENSETC $FVHOME/run/atmens/

set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

setenv ATMENS_FSO_MFCST 2

unsetenv PMI_RANK
unsetenv PMI_FD
unsetenv PMI_JOBID
unsetenv PMI_SIZE

set nymd  = 20161126
set nhms  = 000000
set ftau  = 24
set aver  = asm
set action = setrc

atmens_prepobsens.csh $EXPID $nymd $nhms $ftau $aver $action

#atmens_prepobsens.csh $EXPID $nymd $nhms $ftau $aver NULL

