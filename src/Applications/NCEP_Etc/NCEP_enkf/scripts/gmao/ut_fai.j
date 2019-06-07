#!/bin/csh

setenv EXPID x0028C_rt

setenv ATMENSETC /dev/null
unsetenv AENSTAT_MPIRUN
setenv FAI_FACTOR 0.5
setenv FVHOME $PRJ/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv FVWORK $PRJ/enswork.X0028C_RT

source $FVROOT/bin/g5_modules

set path = ( $path $FVROOT/bin )

set nymd = 20160906
set nhms = 150000

atmos_efai.csh  $EXPID $nymd $nhms

