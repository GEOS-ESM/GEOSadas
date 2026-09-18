#!/bin/csh

setenv GID g0613
setenv BATCH_SUBCMD "sbatch"

setenv EXPID j3dfgat
setenv FVHOME /discover/nobackup/projects/gmao/dadev/rtodling/JEDI/x51/$EXPID
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv FVWORK `cat $FVHOME/.FVWORK`
setenv JEDIDIR $FVHOME/run/jedi
setenv VAROFFSET 180
setenv TIMEINC 360

source $FVROOT/bin/g5_modules
source $FVHOME/run/jedi/JEDIanaConfig.csh
set path = ( . $FVHOME/run $JEDIDIR $FVROOT/bin $SHARE/dasilva/opengrads/Contents $BASEDIR/$ARCH/bin $path )

set this   = `ls -d $FVWORK/jedi.*00`
set lstana = `basename $this`
set nymd = `echo $lstana | cut -d. -f2`
set nhms = `echo $lstana | cut -d. -f3`
set hh   = `echo $nhms | cut -c1-2`

@ varoffset_sec = $VAROFFSET * 60
set date0 = (`tick $nymd ${hh}0000 -$varoffset_sec`)
set gcm_nymd0 = $date0[1]
set gcm_nhms0 = $date0[2]
set this_hh0  = `echo $gcm_nhms0 | cut -c1-2`
jedi_driver.csh $gcm_nymd0 $gcm_nhms0 $nymd $nhms |& tee -a $FVWORK/$EXPID.jedi_drv.log.${gcm_nymd0}_${this_hh0}z.txt
