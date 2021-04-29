#!/bin/csh -f

# ldas_run.csh - invokes the land  analysis.
#
# !REVISION HISTORY:
#
#  Apr2021  sqzhang   Initial script
#-------------------------------------

set echo

setenv MYNAME ldas_run.csh

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - entry point to obtain LDAS increments"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  stage freql freqa"
   echo " "
   echo " where"
   echo "   stage  -   0 (run ldas) or  1 (stage ldas increments)"
   echo "   freql   -  frequency of ldas analysis increments, as in HHMMSS"
   echo "   freqa   -  frequency of adas analysis increments,  as in HHMMSS"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedures handles the LDAS coupling in the central DAS. " 
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME 1 030000 060000"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "  FVHOME        - location of experiment            "
   echo "  FVROOT        - location of DAS build             "
   echo "  FVWORK        - location of work directory        "
   echo "  LDHOME        - location of LDAS experiment   "
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

set path = ( . $FVHOME/run $FVROOT/bin $path )

setenv FAILED 0
if ( !($?FVHOME)         ) setenv FAILED 1
if ( !($?FVROOT)         ) setenv FAILED 1
if ( !($?FVWORK)         ) setenv FAILED 1
if ( !($?LDHOME)         ) setenv FAILED 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set stage = $1
set freql = $2 
set freqa = $3 

     cd $FVWORK
     set adas_strt = ( `rst_date ./d_rst` )
set nymd    = `echo $adas_strt[1] | cut -c1-8`
set hh     = `echo $adas_strt[2] | cut -c1-2`
set yyyymmddhh = ${nymd}${hh} 

if (-e $FVWORK/.DONE_${MYNAME}.${yyyymmddhh} ) then
   echo " ${MYNAME}: already done"
   exit(0)
endif 

if ( $stage == 0 ) then 
    echo " ${MYNAME}: stage 0"

      @ adas_int = $freqa / 10000
      @ adas_int = $adas_int * 3600
      @ cent_int = 3600
      @ tavg1_tick0 = -1800
      @ inst1_tick0 = 0

       set inst1_strt = ( `tick $adas_strt $inst1_tick0` )
       set tavg1_strt = ( `tick $adas_strt $tavg1_tick0` )

      set secs = 0

     while ( $secs  < = $adas_int )
           set inst1_now = ( `tick $inst1_strt $secs` )
           set tavg1_now = ( `tick $tavg1_strt $secs` )

           set tttt_i=`echo $inst1_now[2] | cut -c1-4`
           set tttt_a=`echo $tavg1_now[2] | cut -c1-4` 
 
        /bin/cp  ${FVHOME}/recycle/holdforc/*.inst1_2d_lfo_Nx+-.$inst1_now[1]_${tttt_i}z.nc4\
        ${FVWORK}
        /bin/cp  ${FVHOME}/recycle/holdforc/*.tavg1_2d_lfo_Nx+-.$tavg1_now[1]_${tttt_a}z.nc4\
        ${FVWORK}

      @ secs = $secs + $cent_int
   end 

#link $FVWORK for ldas met_forcing  access 
       /bin/rm -f $FVHOME/lana/forc
       /bin/ln -s $FVWORK $FVHOME/lana/forc

        ls -l $FVHOME/lana/forc/*lfo_Nx+* 

         echo " ${MYNAME}: LDAS coupling: run ldas for central DAS coupling"
         # go to LDHOME to submit ldas run
      cd $LDHOME/run
      echo "ldas_home_dir: ", $LDHOME
      set lcapdat = `cat cap_restart | cut -c1-8`
      set lcaptim = `cat cap_restart | cut -c10-15`
      echo "ldas_6h_window starting at: ", $lcapdat, $lcaptim
      echo "adas_anal_window starting at: ", $adas_strt[1], $adas_strt[2]

      # submit job and capture job ID 
       set jobldas = "$LDHOME/run/lenkf.j"
       set jobIDlong = `$PBS_BIN/sbatch $jobldas`
       set jobID = `echo $jobIDlong  |awk -F'[ ]' '{print $4}'`
       setenv ldasJobIDs  $jobID
       echo $ldasJobIDs ": LDAS coupling lenkf jobID in LandAnalysisRun"

## back to fvwork 
       cd $FVWORK

##stage incr 
    else 
        cd $FVWORK
        echo " ${MYNAME}: LDAS coupling: stage/link LdasIncr for AGCM corrector "
               if ($?ldasJobIDs) then
                    $FVROOT/bin/jobIDfilter -w $ldasJobIDs
                    unsetenv ldasJobIDs
                endif

           set lenkf_status_file = ${FVWORK}/lenkf_job_completed.txt
               rm -f $lenkf_status_file

               cp $LDHOME/run/lenkf_job_completed.txt  $lenkf_status_file

               set lenkf_status = `cat $lenkf_status_file`
               echo $lenkf_status
               echo  $lenkf_status ": lenkf_status"
              if ($lenkf_status =~  SUCCEEDED )  then
               echo "LDAS coupling Lenkf job SUCCEEDED, stageLdasIncr"
              endif 

            /bin/rm -f ${FVHOME}/recycle/holdforc/* 

# current all member incr outputs in cat/ens_avg 
    set LINC_DIR = ${LDHOME}/output/*/cat/ens_avg/

      cd ${FVHOME}/lana 

      @ ldas_int = $freql / 10000
      @ ldas_int = $ldas_int * 3600

      @ adas_int = $freqa / 10000
      @ adas_int = $adas_int * 3600

      set  lincr_native_name = catch_progn_incr
      set  lincr_default_name = ldas_inc

      
      @ cent_int = ($ldas_int / 2)  

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
          if ( -e  ${LINC_DIR}/Y${yyyy_a}/M${mm_a}/*${lincr_native_name}.$ldas_anlt[1]_${tttt_a}z.nc4) then

              /bin/cp  ${LINC_DIR}/Y${yyyy_a}/M${mm_a}/*.${lincr_native_name}.$ldas_anlt[1]_${tttt_a}z.nc4\
        ${FVWORK}/ldas_inc.$ldas_cntr[1]_${tttt_c}00

      /bin/ln -s  ${LINC_DIR}/Y${yyyy_a}/M${mm_a}/*${lincr_native_name}.$ldas_anlt[1]_${tttt_a}z.nc4\
        ${FVHOME}/lana/ldas_inc.$ldas_cntr[1]_${tttt_c}00 
              else 
       echo " ${MYNAME}: WARNING: ldas incr file not found, no ldasIncr for this cycle"
       exit 1
             endif
      @ secs = $secs + $ldas_int
   end 

# normal return  
touch $FVWORK/.DONE_${MYNAME}.${yyyymmddhh}
echo " ${MYNAME}: Complete "
      exit 0  

   endif #end stage=1

    cd ${FVWORK} 


