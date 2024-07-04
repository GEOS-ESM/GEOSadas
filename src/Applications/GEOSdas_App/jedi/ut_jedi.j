#!/bin/csh

setenv GID g0613
setenv BATCH_SUBCMD "sbatch"

setenv EXPID j48rt4
setenv FVHOME /gpfsm/dnb05/projects/p139/rtodling/JEDI/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv JEDIDIR $FVHOME/run/jedi
setenv VAROFFSET 180

source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $JEDIDIR $FVROOT/bin $SHARE/dasilva/opengrads/Contents $BASEDIR/$ARCH/bin $path )

set nymd = 20211205
set hh   = 00
set nhms = ${hh}0000
setenv FVWORK $FVHOME/../fvwork.28374

@ varoffset_sec = $VAROFFSET * 60
set date0 = (`tick $nymd ${hh}0000 -$varoffset_sec`)
set gcm_nymd0 = $date0[1]
set gcm_nhms0 = $date0[2]
set this_hh0  = `echo $gcm_nhms0 | cut -c1-2`
jedi_driver.csh $gcm_nymd0 $gcm_nhms0 $nymd $nhms |& tee -a $FVWORK/$EXPID.jedi_drv.log.${gcm_nymd0}_${this_hh0}z.txt
