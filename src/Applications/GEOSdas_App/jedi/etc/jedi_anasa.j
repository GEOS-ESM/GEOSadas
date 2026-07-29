#!/bin/csh -fx
@GEOSJEDI_GID
@GEOSJEDI_QOS
@GEOSJEDI_PARTITION
#SBATCH --job-name=janasa
#SBATCH --output=janasa.log.o%j.txt
#SBATCH --nodes=1
#SBATCH --constraint=mil
#SBATCH --time=2:00:00

#######################################################
#######################################################
######                                           ######
######   Standlone jedi analysis                 ######
######                                           ######
###### Supports:                                 ######
######   1. Running just jedi analysis from      ######
######      GEOS-JEDI output                     ######
######   2. Running forecast verification vs     ######
######      observations                         ######
#######################################################
#######################################################

setenv myname jedi_anasa

setenv GID @GID
setenv EXPID @EXPID
setenv FVHOME @FVHOME
setenv FVROOT `cat $FVHOME/.FVROOT`
setenv VAROFFSET 180
setenv JEDI_RUN_ANASA 1
setenv ARCHIVE @ARCHIVE

setenv BATCH_SUBCMD sbatch

if ( ! -d $FVHOME/run/jedi ) then
   echo "No JEDI settings have been found within GEOS, abort"
   exit 1
endif

set path = ( . $FVHOME/anasa $FVHOME/run/jedi $FVHOME/run $FVROOT/bin $path )
source $FVROOT/bin/g5_modules

setenv FVWORK $FVHOME/../tmpjasens.$$
setenv HOLDRESULTS $FVWORK/Results
mkdir -p $FVWORK
mkdir -p $HOLDRESULTS

@ offset_hr  = $VAROFFSET / 60
@ offset_sec = $VAROFFSET * 60

cd $FVHOME/anasa
if ($?this_nymdhh) then
   set lstcases = `/bin/ls -1 standalone.${this_nymdhh}z`
else
   set lstcases = `/bin/ls -1 standalone.*`
endif
if ( $status ) then
  echo $myname": standalone cases listed"
  exit 1
endif
echo $lstcases[1] | grep +
if ( $status ) then
 
   # case when running analysis from background
   set fcst_end = `echo $lstcases[1] | cut -d. -f2`
   set nymde = `echo $fcst_end | cut -c1-8`
   set hhe   = `echo $fcst_end | cut -c10-11`
   set nhmse = ${hhe}0000
   set fcst_beg = $fcst_end

   @ fcoff = 0

   set salog = $EXPID.jedi_anasa.log.${nymde}_${hhe}z.txt  
else

   # case when running "hofx" from forecasts (forecast verified against obs)
   set fcst_beg = `echo $lstcases[1] | cut -d. -f2 | cut -d+ -f1`
   set fcst_end = `echo $lstcases[1] | cut -d. -f2 | cut -d+ -f2`

   set nymd0 = `echo $fcst_beg | cut -c1-8`
   set yyyy0 = `echo $fcst_beg | cut -c1-4`
   set mm0   = `echo $fcst_beg | cut -c5-6`
   set dd0   = `echo $fcst_beg | cut -c7-8`
   set nhms0 = `echo $fcst_beg | cut -c1-8`
   set hh0   = `echo $fcst_beg | cut -c10-11`

   set nymde = `echo $fcst_end | cut -c1-8`
   set yyyye = `echo $fcst_end | cut -c1-4`
   set mme   = `echo $fcst_end | cut -c5-6`
   set dde   = `echo $fcst_end | cut -c7-8`
   set hhe   = `echo $fcst_end | cut -c10-11`
   set nhmse = ${hhe}0000

   # dates:
   set d1 = ${nymd0}${hh0}
   set d2 = ${nymde}${hhe}

   # Reformat into YYYY-MM-DD HH:00:00
   set dt1 = `echo $d1 | sed 's/\(....\)\(..\)\(..\)\(..\)/\1-\2-\3 \4:00:00/'`
   set dt2 = `echo $d2 | sed 's/\(....\)\(..\)\(..\)\(..\)/\1-\2-\3 \4:00:00/'`

   # Convert to epoch seconds (GNU date)
   set t1 = `date -d "$dt1" +%s`
   set t2 = `date -d "$dt2" +%s`

   @ fcoff = ($t2 - $t1) / 3600 - $offset_hr
   if ( $fcoff == 0 ) then
      /bin/rm $lstcases[1]
      echo "jedi_anasa.j: t=0 found, stopping"
      echo "jedi_anasa.j: check standalone files if this not meant"
      exit(0)
   endif

   set salog = $EXPID.jedi_anasa.log.${nymd0}_${hh0}z+${nymde}_${hhe}z.txt  

endif

# Start of analysis cycle (typically 3-hour offset from analysis time)
# --------------------------------------------------------------------
set bana = ( `tick $nymde $nhmse -$offset_sec` )

set nymdb = `echo $bana[1] | cut -c1-8`
set yyyyb = `echo $bana[1] | cut -c1-4`
set mmb   = `echo $bana[1] | cut -c5-6`
set ddb   = `echo $bana[1] | cut -c7-8`
set hhb   = `echo $bana[2] | cut -c1-2`

set nymda = $nymde
set hha   = $hhe
set nhmsa = ${hhe}0000

if ( -d $FVWORK/janasa ) /bin/rm -r $FVWORK/janasa

jedi_driver.csh $bana[1] $bana[2] $nymda $nhmsa $fcoff |& tee -a $FVWORK/$salog
if ($status) then
  echo "jedi_anasa.j: Failed on jedi_driver, aborting"
  exit(1)
endif

# For now, archive by "hand"
# -------------------------
if ( $fcoff ) then # for now, ignore saving output from sa-analysis (only fcst verification)
  /bin/mv $FVWORK/$EXPID.jedi_hofx.${nymdb}_${hhb}z.tar $ARCHIVE/jedi/verify/obs/Y$yyyyb/M$mmb/
endif


# Clean up and go on
# ------------------
/bin/rm $lstcases[1]

if ($?this_nymdhh) then
   if( (`uname -s` == "Linux") && ((`uname -m` == "ia64")||(`uname -m` == "x86_64")) ) then
      cd
      /bin/rm -r $FVWORK
   endif
else
   set lstcases = `/bin/ls -1 standalone.*`
   if ( $status ) then
        echo $myname": no more cases to run, anasa job completed"
        exit 0
   endif
   set jname = janasa
   set lname = $jname.log.o%j
   if ( $#lstcases > 0 ) then
      if ( $BATCH_SUBCMD == "sbatch" ) then
         sbatch -d afterany:${PBS_JOBID} -J $jname -o $lname jedi_anasa.j
      else
         qsub -W depend=afterany:${PBS_JOBID} -N $jname -o $lname jedi_anasa.j
      endif
   endif
endif

#/bin/mv *.tar *.txt $HOLDRESULTS/

