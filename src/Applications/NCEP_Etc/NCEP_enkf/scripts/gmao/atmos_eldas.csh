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
   echo "  $MYNAME  expid nymd nhms freq "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of current anal as in YYYYMMDD"
   echo "   nhms   -  time of current anal as in HHMMSS"
   echo "   freq   -  frequency of LDAS4en analysis, as in HHMMSS"
   echo " "
   echo " "
   echo "    This procedures handles the LDAS coupling in the ensembe DAS. In its simplest form "
   echo "  this procedure makes the LDAS4ens ensavg analysis available to each of the members of the   "
   echo "  ensemble."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 030000 "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "  FVHOME        - location of experiment            "
   echo "  FVROOT        - location of DAS build             "
   echo "  FVWORK        - location of work directory        "
   echo "  LDHOME4ens    - location of LDAS4ens experiment   "
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
if ( !($?LDHOME4ens)     ) setenv FAILED 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid = $1
set nymd  = $2
set nhms  = $3
set freq  = $4
set hh     = `echo $nhms | cut -c1-2`
set yyyymmddhh  = ${nymd}${hh} 

setenv ENSWORK $FVWORK
if (-e $ENSWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif

         echo " ${MYNAME}: LDAS4ENS coupling: run ldas for atmens coupling"
          # ens forc access:  $FVHOME/ensdiag/mem***
         # go to LDHOME to run ldas
      cd $LDHOME4ens/run
      echo "ldas_home_dir: ", $LDHOME4ens
      # submit job and capture job ID 
       set jobldas = "$LDHOME4ens/run/lenkf.j"
       set jobIDlong = `$PBS_BIN/sbatch $jobldas`
       set jobID = `echo $jobIDlong  |awk -F'[ ]' '{print $4}'`
       setenv ldasJobIDs  $jobID
       echo $ldasJobIDs ": LDAS4ens coupling lenkf jobID in LandAnalysisRun"

## back to fvwork 
      cd $FVWORK

        echo " ${MYNAME}: LDAS4ENS coupling: stage/link LdasIncr for eAGCM corrector "
        setenv RSTSTAGE4AENS $FVHOME/atmens/RST

               if ($?ldasJobIDs) then
                    $FVROOT/bin/jobIDfilter -w $ldasJobIDs
                    unsetenv ldasJobIDs
                endif

           set lenkf_status_file = ${FVWORK}/lenkf_job_completed.txt
               rm -f $lenkf_status_file

               cp $LDHOME4ens/run/lenkf_job_completed.txt  $lenkf_status_file

               set lenkf_status = `cat $lenkf_status_file`
               echo $lenkf_status
               echo  $lenkf_status ": lenkf_status"
              if ($lenkf_status =~  SUCCEEDED )  then
               echo "LDAS4ens coupling Lenkf job SUCCEEDED, stageLdasIncr4ens"

# current all member incr outputs in cat/ens_avg 
    set LINC_DIR = ${LDHOME4ens}/output/*/cat/ens_avg/

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

    set ldas_int = 10800
    set adas_int = 21600

      set  lincr_native_name = catch_progn_incr
      set  lincr_default_name = ldas_inc

      
      @ cent_int = ($ldas_int / 2)  

     /bin/cp  $RSTSTAGE4AENS/$EXPID.rst.lcv.*.bin my_d_rst
     set adas_strt = ( `rst_date ./my_d_rst` )  

      set secs = 0

     while ( $secs  < $adas_int )
         # the begining time of the window secs=0 
           set ldas_strt = ( `tick $adas_strt $secs` )
         # for ldas_incr, use centered time 
           set ldas_cntr = ( `tick $ldas_strt $cent_int` )
          # ldas anal time
           set ldas_anlt = ( `tick $ldas_strt $ldas_int` )

         set yyyy_a=`echo $ldas_anlt[1] | cut -c1-4`
         set mm_a=`echo $ldas_anlt[1]   | cut -c5-6`
         set dd_a=`echo $ldas_anlt[1]   | cut -c7-8`
         set tttt_a=`echo $ldas_anlt[2] | cut -c1-4`
         set tttt_c=`echo $ldas_cntr[2] | cut -c1-4`
# default name for AGCM: ldas_inc.yyyymmdd_hhnn00
       @ n = 0
while ($n < $nmem)
set lentag = `echo $n | awk '{printf "%04d", $1}'`
echo $lentag
@ n++
set memtag = `echo $n | awk '{printf "%03d", $1}'`
echo $memtag
      /bin/ln -s  ${LINC_DIR}/Y${yyyy_a}/M${mm_a}/*.${lincr_native_name}${lentag}.$ldas_anlt[1]_${tttt_a}z.nc4\
        ${FVHOME}/atmens/enslana/mem$memtag/ldas_inc.$ldas_cntr[1]_${tttt_c}00 
   end 

## copy to FVWORK 
     cd ${FVWORK}  
     set dirs = (`/bin/ls -d mem0*`)
     foreach dir ($dirs)
        set nnn = `echo $dir | cut -c4-6`
        /bin/cp  ${FVHOME}/atmens/enslana/mem${nnn}/ldas_inc.$ldas_cntr[1]_${tttt_c}00\
           ${FVWORK}/mem${nnn}/ldas_inc.$ldas_cntr[1]_${tttt_c}00 
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


