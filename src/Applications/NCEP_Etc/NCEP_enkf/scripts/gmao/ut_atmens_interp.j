#!/bin/csh

setenv EXPID x0033H_rt
setenv FVHOME /discover/nobackup/projects/gmao/advda/rtodling/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`

#setenv FVWORK `cat $FVHOME/.FVWORK`
setenv FVWORK  $FVHOME/atmens

set path = ( . $FVROOT/bin $path )

source $FVROOT/bin/g5_modules

unsetenv PMI_RANK
unsetenv PMI_FD
unsetenv PMI_JOBID
unsetenv PMI_SIZE

set nymd = 20161128
set nhms = 120000
set nlon_ens = 288
set nlat_ens = 181
set nsig     = 132

atmens_interp.csh $EXPID $nymd $nhms $nlon_ens $nlat_ens $nsig $FVWORK $FVWORK
