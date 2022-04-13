#!/bin/csh -f

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME atmos_eldas.csh

if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - entry point to obtain LDAS4en increments"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms freqa "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of current anal as in YYYYMMDD"
   echo "   nhms   -  time of current anal as in HHMMSS"
   echo "   freqa   -  frequency of ADASen analysis, as in HHMMSS"
   echo " "
   echo " "
   echo "    This procedures handles the LDAS coupling in the ensembe DAS. In its simplest form "
   echo "  this procedure makes the LDAS4ens ensavg analysis available to each of the members of the   "
   echo "  ensemble."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000  060000"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "  FVHOME        - location of experiment            "
   echo "  FVROOT        - location of DAS build             "
   echo "  FVWORK        - location of work directory        "
   echo "  LDHOME4ENS    - location of LDAS4ENS experiment   "
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

##source $FVROOT/bin/g5_modules  
set path = ( . $FVHOME/run $FVROOT/bin $path )

setenv FAILED 0
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1
if ( !($?LDHOME4ENS)     ) setenv FAILED 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3
set freqa  = $4

set hh     = `echo $nhms | cut -c1-2`
set yyyymmddhh  = ${nymd}${hh} 

setenv ENSWORK $FVWORK
if (-e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

echo " ${MYNAME}: LDAS4ENS coupling: run ldas for atmens coupling"
# ens forc access:  $FVHOME/ensdiag/mem***
# go to LDHOME4ENS to run ldas
cd $LDHOME4ENS/run
echo "ldas_home_dir: ", $LDHOME4ENS
# submit job and capture job ID 
set jobldas = "$LDHOME4ENS/run/lenkf.j"
set jobIDlong = `sbatch $jobldas`
set jobID = `echo $jobIDlong  |awk -F'[ ]' '{print $4}'`
setenv ldasJobIDs  $jobID
echo $ldasJobIDs ": LDAS4ens coupling lenkf jobID "

## back to fvwork 
cd $FVWORK

echo " ${MYNAME}: LDAS4ENS coupling: stage/link LdasIncr for eAGCM corrector "
setenv RSTSTAGE4AENS $FVHOME/atmens/RST

if ($?ldasJobIDs) then
    $FVROOT/bin/jobIDfilter -w $ldasJobIDs
    unsetenv ldasJobIDs
endif

set lenkf_status_file = ${FVWORK}/lenkf_job_completed.txt
/bin/rm -f $lenkf_status_file

/bin/cp $LDHOME4ENS/run/lenkf_job_completed.txt  $lenkf_status_file

set lenkf_status = `cat $lenkf_status_file`
echo $lenkf_status
echo  $lenkf_status ": lenkf_status"
if ($lenkf_status =~  SUCCEEDED )  then
   echo "LDAS4ens coupling Lenkf job SUCCEEDED, stageLdasIncr4ens"

# current all member incr outputs in cat/ens_avg 
   set LINC_DIR = ${LDHOME4ENS}/output/*/cat/ens_avg/

#make atmens/lana/mem*
   cd ${FVHOME}/atmens 
   mkdir enslana 
   @ nmem = 0
   set dirs = (`/bin/ls -d mem0*`)
   foreach dir ($dirs)
      set nnn = `echo $dir | cut -c4-6`
      mkdir  ${FVHOME}/atmens/enslana/mem${nnn}
      @ nmem ++ 
   end  #foreach dir 
   cd - 

   @ adas_int = $freqa / 10000
   @ adas_int = $adas_int * 3600  

   set ldas_int = 10800
   set ldasDT  = `grep LANDASSIM_DT: ${LDHOME4ENS}/run/LDAS.rc | cut -d':' -f2`
   if ( ${ldasDT} > 0 ) then
      set ldas_int  = ${ldasDT}
   endif

   set ldas_t0 = 013000
   set ldasT0 = `grep LANDASSIM_T0:  ${LDHOME4ENS}/run/LDAS.rc | cut -d':' -f2`
   if ( ${ldasT0} > 0 ) then
       set ldas_t0 = ${ldasT0}
   endif
   set t0hh = `echo ${ldas_t0} | cut -c1-2`
   set t0mm = `echo ${ldas_t0} | cut -c3-4`
   @ cent_int = $t0hh * 3600 + $t0mm * 60

   set  lincr_native_name = catch_progn_incr
   set  lincr_default_name = ldas_inc

   /bin/cp  $RSTSTAGE4AENS/*.rst.lcv.*.bin my_d_rst
   set adas_strt = ( `rst_date ./my_d_rst` )  

   set secs = 0
   while ( $secs  < $adas_int )
     # the begining time of the window secs=0 
     set ldas_strt = ( `tick $adas_strt $secs` )
     # ldas anal time
     set ldas_anlt = ( `tick $ldas_strt $cent_int` )

     set yyyy_a=`echo $ldas_anlt[1] | cut -c1-4`
     set mm_a=`echo $ldas_anlt[1]   | cut -c5-6`
     set dd_a=`echo $ldas_anlt[1]   | cut -c7-8`
     set tttt_a=`echo $ldas_anlt[2] | cut -c1-4`
     # default name for AGCM: ldas_inc.yyyymmdd_hhnn00
     @ n = 0
     while ($n < $nmem)
        @ n++
        set lentag = `echo $n | awk '{printf "%04d", $1}'`
        echo $lentag

        set memtag = `echo $n | awk '{printf "%03d", $1}'`
        echo $memtag
        /bin/ln -s  ${LINC_DIR}/Y${yyyy_a}/M${mm_a}/*.${lincr_native_name}${lentag}.$ldas_anlt[1]_${tttt_a}z.nc4\
        ${FVHOME}/atmens/enslana/mem$memtag/ldas_inc.$ldas_anlt[1]_${tttt_a}00 
     end 

## copy to FVWORK 
     cd ${FVWORK}  
     set dirs = (`/bin/ls -d mem0*`)
     foreach dir ($dirs)
        set nnn = `echo $dir | cut -c4-6`
        /bin/cp  ${FVHOME}/atmens/enslana/mem${nnn}/ldas_inc.$ldas_anlt[1]_${tttt_a}00\
           ${FVWORK}/mem${nnn}/ldas_inc.$ldas_anlt[1]_${tttt_a}00 
     end  #foreach dir 
#---
   @ secs = $secs + $ldas_int
   end  #sec while loop 

else 
   echo " ${MYNAME}: WARNING: ldas4ens failed, no ldasIncr for this cycle to enAGCM "
   exit 1
endif   #end ldas enkf succeeded 

# normal return  
touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit 0 


