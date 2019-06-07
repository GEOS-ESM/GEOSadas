#!/bin/csh -x

setenv FVROOT $PRJ/4OPS/G515/GEOSadas/Linux
setenv EXPID fd515_1rt
setenv FVHOME $PRJ/$EXPID
setenv TIMEINC 360

set path = ( $path $FVROOT/bin )
source $FVROOT/bin/g5_modules

setenv NCSUFFIX nc4
setenv TLMRUN_OPT_BEGIN "mpirun -np 96 GEOSgcmPert.x"
setenv MPIRUN_UPDAINC "mpirun -np 4 updAincWpert.x"

#set workdir = $PRJ/fvwork.26448   # work directory gcm-heartbeat = 450
set workdir = $PRJ/fvwork.31199   #                 gcm-heartbeat = 900 (as TLM)
set etcdir  = $FVHOME/run/fwdpert # etc-directory (rc files)
set expid   = $EXPID              # name of experiment
set nymdb   = 20141203            # initial date of integration
set nhmsb   = 220000              # initial time of integration
set taub    =         60          # length of integration (min)
set ipertfn = fd515_1rt.xinc.eta.20141203_22z.nc4  # fullpath of input perturbation filename

$FVROOT/../src/Applications/GEOSdas_App/fvpert $workdir $etcdir $expid \
       $nymdb $nhmsb $taub \
       $ipertfn
