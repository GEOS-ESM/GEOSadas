#!/bin/csh -x

setenv SIMULATE_ENSEMBLE 0
setenv ATMENS_VERBOSE 1
setenv DOARCH 1

setenv EXPID f513a_rt
setenv FVHOME /gpfsm/dnb02/projects/p61/$user/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv ENSWORK $FVHOME
setenv ATMENSETC $FVHOME/run/atmens
setenv ATMENSLOC $FVHOME 
setenv ARCHLOC   /archive/u/$user
setenv NCSUFFIX  nc4
setenv VAROFFSET 180

setenv TIMEINC 360
setenv ASYNBKG 180

setenv GID g0613
setenv ENSPARALLEL 1

set path = ( . $FVROOT/bin $path )
source $FVROOT/bin/g5_modules
source $FVHOME/run/atmens/AtmEnsConfig.csh

if ( $DOARCH ) then

  set nymdb = 20151211
  set nhmsb = 030000
  set hhb   = `echo $nhmsb | cut -c1-2`

  atmens_arch.csh $EXPID $nymdb $nhmsb $ATMENSETC/atmens_storage.arc ut_arch_test atmens4arch.${nymdb}_${hhb}

endif # <DOARCH>
