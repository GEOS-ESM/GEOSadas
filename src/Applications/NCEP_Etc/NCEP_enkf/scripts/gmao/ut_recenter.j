#!/bin/csh

setenv ATMENSETC /dev/null
setenv FVROOT $PRJ/4OPS/G515/GEOSadas/Linux
setenv FVWORK $TMP/f513a_rt/work

source $FVROOT/bin/g5_modules

set path = ( $path $FVROOT/bin )

set nymd = 20141203
set nhms = 210000

atmens_recenter.csh  $EXPID $nymd $nhms bkg.eta ana.eta \
                     $FVWORK/atmens $FVWORK /dev/null

