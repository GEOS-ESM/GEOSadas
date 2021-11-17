#!/bin/csh

setenv DRYRUN "check"
setenv DRYRUN echo
setenv DRYRUN 

# post_prepobsen.csh - prepare environment for ensemble forecast
#                      sensitivy to observations (EFSO)
#
# !REVISION HISTORY:
#
#  01Apr2017  Todling   Initial script
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmens_prepobsen.csh

if ( $#argv < 7 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - prepare environment for ensemble-FSO calculations"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms taub aver action"
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   taub   -  forecast interval EFSO calculated for (in hours)"
   echo "   aver   -  verification type (ana,asm,or niana) "
   echo "   prog   -  ensemble forecasts (NULL, prg,or niprg) "
   echo "   action -  the following are valid: "
   echo "             setrc  - to generate acquire file with all needed for EFSO"
   echo "             null   - to actually create all links and resolution conversions"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedure is responsible for converting the output of the "
   echo "  EnKF backward integration into ODS and producing whetever other "
   echo "  diagnostic and statistic desired from the EFSO procedure. "
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME b541iau 20091019 000000 24 asm setrc"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    AENSTAT_MPIRUN   - mp_stats MPI command line         "
   echo "    ATMENSETC        - location of experiment            "
   echo "    FVWORK           - location of work directory        "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENS_FSO_JGRAD  - 1: use GMAO normalized error instead of EnKF internal norm"
   echo "                        (default: 0 - do nothing)"
   echo "    ATMENS_FSO_FSENS  - 1: use ensemble mean adjoint sensitivity"
   echo "                        2: use central adjoint sensitivity"
   echo "                        (default: 0 - do nothing)"
   echo "    ATMENS_FSO_AVRFY  - 0: use central analysis/asm for verification"
   echo "                        1: use non-inflated ensemble analysis for verification"
   echo "                        (default: 0)"
   echo "    ATMENS_FSO_MFCST  - 0: use central forecast for error definition"
   echo "                        1: use ensemble mean forecast for error definition"
   echo "                        (default: 0)"
   echo "    ATMENS_NCPUSTAR - number of CPUS used for untar (default: 32) "
   echo "    NCSUFFIX          - suffix of hdf/netcdf files (default: nc4)"
   echo "    DATADIR           - location where original data resides"
   echo "                        (default: /archive/u/user)"
   echo "    SRCEXPID          - original experiment (use when other than expid)"
   echo "                        (default: expid)"
   echo " "
   echo " SEE ALSO"
   echo "   ut_prepobsens.j - unit tester for this procedure"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Created modified: 01Apr2017   by: R. Todling"
   echo "     Last modified: 06Oct2021      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?AENSTAT_MPIRUN)) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1

if ( !($?ATMENS_FCST_OFFLINE)  ) setenv ATMENS_FCST_OFFLINE 1  # 0= extended forecasts created during EnADAS cycle
                                                               # 1= extended forecasts created offline (default)
if ( !($?ATMENS_FSO_JGRAD)  ) setenv ATMENS_FSO_JGRAD 0  # 1= use forecast error 

if ( !($?ATMENS_FSO_FSENS)  ) setenv ATMENS_FSO_FSENS 0  # 1= use ensemble mean adjoint sensitivity
                                                         # 2= use central adjoint sensitivity

if ( !($?ATMENS_FSO_AVRFY)  ) setenv ATMENS_FSO_AVRFY 0  # 0= use central analysis  
                                                         # 1= use ensemble mean analysis
if ( !($?ATMENS_FSO_MFCST)  ) setenv ATMENS_FSO_MFCST 0  # 0= use central fcsts
                                                         # 1= use mean of ens forecast
if ( !($?ATMENS_NCPUSTAR)  ) setenv ATMENS_NCPUSTAR 32
if ( !($?SRCEXPID)      ) setenv SRCEXPID NULL
if ( !($?DATADIR)       ) setenv DATADIR $ARCHIVE
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4 

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid  = $1
set anymd  = $2
set anhms  = $3
set ftau   = $4
set aver   = $5
set prog   = $6
set action = $7

if ( "$SRCEXPID" == "NULL" ) setenv SRCEXPID $expid

# actions:  setrc 

@ ana_offset = 3 * 3600
@ sixhours   = 6 * 3600
@ ftau_sec   = $ftau * 3600

# Current analysis time
# ---------------------
set ayyyy = `echo $anymd | cut -c1-4`
set amm   = `echo $anymd | cut -c5-6`
set ahh   = `echo $anhms | cut -c1-2`
set ayyyymmddhh = ${anymd}${ahh}

setenv ENSWORK $FVWORK

if ( -e $ENSWORK/.DONE_${MYNAME}.$ayyyymmddhh ) then
   echo "${MYNAME}: Already complete."
   exit (0)
endif

if ( ! -d $ENSWORK ) mkdir -p $ENSWORK
cd $ENSWORK

if ( ! -d prog/fcsterr ) mkdir -p prog/fcsterr

# Initial time
# ------------
set date0 = (`tick $anymd $anhms -$ana_offset`)
set nymd = $date0[1]
set nhms = $date0[2]
set yyyy = `echo $nymd | cut -c1-4`
set mm   = `echo $nymd | cut -c5-6`
set hh   = `echo $nhms | cut -c1-2`
set yyyymmddhh = ${nymd}${hh}

# Previous analysis time
# ----------------------
set pdate = (`tick $anymd $anhms -$sixhours`)
set pnymd = $pdate[1]
set pnhms = $pdate[2]
set pyyyy = `echo $pnymd | cut -c1-4`
set pmm   = `echo $pnymd | cut -c5-6`
set phh   = `echo $pnhms | cut -c1-2`
set pyyyymmddhh = ${nymd}${hh}

# Verification time
# -----------------
set av0date = ( `tick $nymd $nhms $ftau_sec` )
set av0date = ( `tick $av0date $ana_offset` )
set av0nymd = $av0date[1]
set av0nhms = $av0date[2]
set av0yyyy = `echo $av0nymd | cut -c1-4`
set av0mm   = `echo $av0nymd | cut -c5-6`
set av0hh   = `echo $av0nhms | cut -c1-2`
set av0hhmn = ${av0hh}00
set av0yyyymmddhh = ${av0nymd}${av0hh}

set xv0date = ( `tick $av0date[1] $av0date[2] $sixhours` )
set xv0nymd = $xv0date[1]
set xv0nhms = $xv0date[2]
set xv0yyyy = `echo $xv0nymd | cut -c1-4`
set xv0mm   = `echo $xv0nymd | cut -c5-6`
set xv0hh   = `echo $xv0nhms | cut -c1-2`
set xv0yyyymmddhh = ${xv0nymd}${xv0hh}

# When forecasts are generated online (as enADAS runs), there is a time-shift in the tarball 
# namings holding the prognostic fields; this is not the case when fcsts are done offline
# -------------------------------------------------------------------------------------------
if ( $ATMENS_FCST_OFFLINE ) then
  set xfadate = ( $nymd $nhms )
  set xfanymd = $xfadate[1]
  set xfanhms = $xfadate[2]
  set xfayyyy = `echo $xfanymd | cut -c1-4`
  set xfamm   = `echo $xfanymd | cut -c5-6`
  set xfahh   = `echo $xfanhms | cut -c1-2`
  set xfayyyymmddhh = ${xfanymd}${xfahh}

  set xfbdate = (`tick $nymd $nhms -$sixhours`)
  set xfbnymd = $xfbdate[1]
  set xfbnhms = $xfbdate[2]
  set xfbyyyy = `echo $xfbnymd | cut -c1-4`
  set xfbmm   = `echo $xfbnymd | cut -c5-6`
  set xfbhh   = `echo $xfbnhms | cut -c1-2`
  set xfbyyyymmddhh = ${xfbnymd}${xfbhh}
else
  set xfbdate = ( $nymd $nhms )
  set xfbnymd = $xfbdate[1]
  set xfbnhms = $xfbdate[2]
  set xfbyyyy = `echo $xfbnymd | cut -c1-4`
  set xfbmm   = `echo $xfbnymd | cut -c5-6`
  set xfbhh   = `echo $xfbnhms | cut -c1-2`
  set xfbyyyymmddhh = ${xfbnymd}${xfbhh}

  set xfadate = (`tick $nymd $nhms $sixhours`)
  set xfanymd = $xfadate[1]
  set xfanhms = $xfadate[2]
  set xfayyyy = `echo $xfanymd | cut -c1-4`
  set xfamm   = `echo $xfanymd | cut -c5-6`
  set xfahh   = `echo $xfanhms | cut -c1-2`
  set xfayyyymmddhh = ${xfanymd}${xfahh}
endif

# Current times of forecasts from background and analysis
# -------------------------------------------------------
set fadate = ( $nymd $nhms )
set fanymd = $fadate[1]
set fanhms = $fadate[2]
set fayyyy = `echo $fanymd | cut -c1-4`
set famm   = `echo $fanymd | cut -c5-6`
set fadd   = `echo $fanymd | cut -c7-8`
set fahh   = `echo $fanhms | cut -c1-2`
set fayyyymmddhh = ${fanymd}${fahh}

set fbdate = (`tick $nymd $nhms -$sixhours`)
set fbnymd = $fbdate[1]
set fbnhms = $fbdate[2]
set fbyyyy = `echo $fbnymd | cut -c1-4`
set fbmm   = `echo $fbnymd | cut -c5-6`
set fbdd   = `echo $fbnymd | cut -c7-8`
set fbhh   = `echo $fbnhms | cut -c1-2`
set fbyyyymmddhh = ${fbnymd}${fbhh}

# Following cycle time
# ---------------------
set nanadate = (`tick $nymd $nhms $sixhours`)
set nnymd = $nanadate[1]
set nnhms = $nanadate[2]
set nyyyy = `echo $nnymd | cut -c1-4`
set nmm   = `echo $nnymd | cut -c5-6`
set nhh   = `echo $nnhms | cut -c1-2`
set nyyyymmddhh = ${nnymd}${nhh}

# Verification from cycle above 
# -----------------------------
set vnanadate = (`tick $nnymd $nnhms $ftau_sec`)
set vnnymd = $vnanadate[1]
set vnnhms = $vnanadate[2]
set vnyyyy = `echo $vnnymd | cut -c1-4`
set vnmm   = `echo $vnnymd | cut -c5-6`
set vnhh   = `echo $vnnhms | cut -c1-2`
set vnyyyymmddhh = ${vnnymd}${vnhh}


#=====================================================================
# 1st phase: acquire and position required input for ob-sensitivity 
#=====================================================================
@ notavail = 0
if ( $action == "setrc" ) then
   setenv ACQRC aensosens.rc
   if ( -e $ACQRC ) /bin/rm $ACQRC
   touch $ACQRC
endif

# Unfold tar balls, copy analysis, and establish links as desired ... 
# -------------------------------------------------------------------
if ( (! -e osense_$ayyyymmddhh.dat) || (! -e $expid.atmens_osens.${anymd}_${ahh}z.bin) ) then
   if ( $DRYRUN == "check" ) then
      if( ! -e $DATADIR/$SRCEXPID/atmens/Y$ayyyy/M$amm/$SRCEXPID.atmens_osens.${anymd}_${ahh}z.bin ) then
         echo "${MYNAME}: missing  $SRCEXPID.atmens_osens.${anymd}_${ahh}z.bin "
         @ notavail = $notavail + 1
      endif
   else
      if ( $action == "setrc" ) then
         if ( $SRCEXPID == $expid ) then
            echo $DATADIR/$expid/atmens/Y$ayyyy/M$amm/$expid.atmens_osens.${anymd}_${ahh}z.bin >>  $ACQRC
         else
            echo "$DATADIR/$SRCEXPID/atmens/Y$ayyyy/M$amm/$SRCEXPID.atmens_osens.${anymd}_${ahh}z.bin => $expid.atmens_osens.${anymd}_${ahh}z.bin" >>  $ACQRC
         endif
      else
         $DRYRUN /bin/cp $expid.atmens_osens.${anymd}_${ahh}z.bin  osense_$ayyyymmddhh.dat
      endif
   endif
else
    echo "${MYNAME}: copied forward ob-space residual file ... "
endif

# Satbias information
# -------------------
foreach ftype ( satbang satbias )
  if ( (! -e $expid.ana.$ftype.${pnymd}_${phh}z.txt) || (! -e $ftype ) ) then
     if ( $DRYRUN == "check" ) then
        if( ! -e $DATADIR/$SRCEXPID/ana/Y$pyyyy/M$pmm/$SRCEXPID.ana.$ftype.${pnymd}_${phh}z.txt ) then
           echo "${MYNAME}: missing  $SRCEXPID.ana.$ftype.${pnymd}_${phh}z.txt "
           @ notavail = $notavail + 1
        endif
     else
        if ( $action == "setrc" ) then
           if ( $SRCEXPID == $expid ) then
               echo $DATADIR/$expid/ana/Y$pyyyy/M$pmm/$expid.ana.$ftype.${pnymd}_${phh}z.txt >>  $ACQRC
           else
               echo "$DATADIR/$SRCEXPID/ana/Y$pyyyy/M$pmm/$SRCEXPID.ana.$ftype.${pnymd}_${phh}z.txt => $expid.ana.$ftype.${pnymd}_${phh}z.txt" >>  $ACQRC
           endif 
        else
           $DRYRUN ln -sf $expid.ana.$ftype.${pnymd}_${phh}z.txt $ftype
        endif
     endif
  else
      echo "${MYNAME}: copied sat-bias files ... "
  endif
end

# ensemble of backgrounds at analysis time
#foreach ftype ( bkg cbkg )
foreach ftype ( )
   if ( ! -d $expid.atmens_${ftype}.${nymd}_${hh}z ) then
      if ( $DRYRUN == "check" ) then
         if ( ! -e $DATADIR/$expid/atmens/Y$yyyy/M$mm/$expid.atmens_${ftype}.${nymd}_${hh}z.tar ) then
            echo "${MYNAME}: missing  $expid.atmens_${ftype}.${nymd}_${hh}z.tar "
            @ notavail = $notavail + 1
         endif
      else
         if ( $action == "setrc" ) then
            echo $DATADIR/$expid/atmens/Y$yyyy/M$mm/$expid.atmens_${ftype}.${nymd}_${hh}z.tar >> $ACQRC
         else
            if ( -e $expid.atmens_${ftype}.${nymd}_${hh}z.tar ) then
               echo "${MYNAME}: unfolding ensemble of backgrounds ($ftype)  ... "
               if ( $ATMENS_NCPUSTAR > 1 ) then
                  $DRYRUN parallel-untar.py $expid.atmens_${ftype}.${nymd}_${hh}z.tar $ATMENS_NCPUSTAR
               else
                  $DRYRUN tar xvf $expid.atmens_${ftype}.${nymd}_${hh}z.tar 
               endif
            endif
         endif
      endif
   endif
end
# ensemble of (non-inflated) analyses at initial time
if ( ! -d $expid.atmens_e$aver.${nnymd}_${nhh}z ) then
   if ( $DRYRUN == "check" ) then
      if ( ! -e $DATADIR/$SRCEXPID/atmens/Y$nyyyy/M$nmm/$SRCEXPID.atmens_e$aver.${nnymd}_${nhh}z.tar ) then
         echo "${MYNAME}: missing  $SRCEXPID.atmens_e$aver.${nnymd}_${nhh}z.tar "
         @ notavail = $notavail + 1
      endif
   else
      if ( $action == "setrc" ) then
         if ( $SRCEXPID == $expid ) then
            echo $DATADIR/$expid/atmens/Y$nyyyy/M$nmm/$expid.atmens_e$aver.${nnymd}_${nhh}z.tar >> $ACQRC
         else
            echo "$DATADIR/$SRCEXPID/atmens/Y$nyyyy/M$nmm/$SRCEXPID.atmens_e$aver.${nnymd}_${nhh}z.tar => $expid.atmens_e$aver.${nnymd}_${nhh}z.tar" >> $ACQRC
         endif
      else
         echo "${MYNAME}: unfolding non-inflated ensemble of analyses  ... "
         $DRYRUN tar xvf $expid.atmens_e$aver.${nnymd}_${nhh}z.tar --wildcards --no-anchored "*${SRCEXPID}.$aver.eta.${anymd}_${ahh}00z.$NCSUFFIX"
         if ( $SRCEXPID != $expid && -d $SRCEXPID.atmens_e$aver.${nnymd}_${nhh}z ) then
            $DRYRUN /bin/mv $SRCEXPID.atmens_e$aver.${nnymd}_${nhh}z $expid.atmens_e$aver.${nnymd}_${nhh}z
         endif
         ln -sf $expid.atmens_e$aver.${nnymd}_${nhh}z/mem* .
         if ( $SRCEXPID != $expid ) then
            foreach dir (`/bin/ls -d mem*`)
              cd $dir
                 foreach fn (`/bin/ls *.$aver.*`)
                     set sfx = `echo $fn | cut -d. -f2-`
                     ln -sf $fn $expid.$sfx
                 end
              cd -
            end # <dir>
         endif # <expid>
      endif
   endif
endif

# ensemble forecasts issued from analyses
if ( $ATMENS_FSO_MFCST <= 1 ) then
 if ( ! -d $expid.atmens_e${prog}.${xfanymd}_${xfahh}z ) then
   if ( $DRYRUN == "check" ) then
      if ( ! -e $DATADIR/$SRCEXPID/atmens/Y$xfayyyy/M$xfamm/$SRCEXPID.atmens_e${prog}.${xfanymd}_${xfahh}z.tar ) then 
         echo "${MYNAME}: missing  $SRCEXPID.atmens_e${prog}.${xfanymd}_${xfahh}z.tar "
         @ notavail = $notavail + 1
      endif
   else
      if ( $action == "setrc" ) then
         if ( $SRCEXPID == $expid ) then
            echo $DATADIR/$expid/atmens/Y$xfayyyy/M$xfamm/$expid.atmens_e${prog}.${xfanymd}_${xfahh}z.tar >> $ACQRC
         else
            echo "$DATADIR/$SRCEXPID/atmens/Y$xfayyyy/M$xfamm/$SRCEXPID.atmens_e${prog}.${xfanymd}_${xfahh}z.tar => $expid.atmens_e${prog}.${xfanymd}_${xfahh}z.tar"  >> $ACQRC
         endif
      else
         echo "${MYNAME}: unfolding ensemble of forecasts from analyses $expid.atmens_e${prog}.${xfanymd}_${xfahh}z.tar ... "
         $DRYRUN tar xvf $expid.atmens_e${prog}.${xfanymd}_${xfahh}z.tar --wildcards --no-anchored "*${SRCEXPID}.prog.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX"
         if ( $ATMENS_FSO_FSENS == 1 ) then
            $DRYRUN tar xvf $expid.atmens_e${prog}.${xfanymd}_${xfahh}z.tar --wildcards --no-anchored "*${SRCEXPID}.fsens*.eta.*-${anymd}_${ahh}z.$NCSUFFIX"
         endif
         if ( $SRCEXPID != $expid && -d $SRCEXPID.atmens_e${prog}.${xfanymd}_${xfahh}z ) then
            $DRYRUN /bin/mv $SRCEXPID.atmens_e${prog}.${xfanymd}_${xfahh}z $expid.atmens_e${prog}.${xfanymd}_${xfahh}z
         endif
         cd prog
         $DRYRUN ln -sf ../$expid.atmens_e${prog}.${xfanymd}_${xfahh}z   ${fanymd}_${fahh}z
         cd ${fanymd}_${fahh}z
         if ( $SRCEXPID != $expid ) then
            foreach dir (`/bin/ls -d mem*`)
              cd $dir
                 foreach fn (`/bin/ls $SRCEXPID.prog.* $SRCEXPID.fsens*`)
                     set sfx = `echo $fn | cut -d. -f2-`
                     ln -sf $fn $expid.$sfx
                 end
              cd -
            end # <dir>
         endif # <expid>
         cd $ENSWORK
      endif
   endif
 endif
endif # $ATMENS_FSO_MFCST

# ensemble forecasts issued from backgrounds
if ( $ATMENS_FSO_MFCST <= 1 ) then
 if ( ! -d $expid.atmens_e${prog}.${xfbnymd}_${xfbhh}z ) then
   if ( $DRYRUN == "check" ) then
      if ( ! -e $DATADIR/$SRCEXPID/atmens/Y$xfbyyyy/M$xfbmm/$SRCEXPID.atmens_e${prog}.${xfbnymd}_${xfbhh}z.tar ) then 
         echo "${MYNAME}: missing  $SRCEXPID.atmens_e${prog}.${xfbnymd}_${xfbhh}z.tar "
         @ notavail = $notavail + 1
      endif
   else
      if ( $action == "setrc" ) then
         if ( $SRCEXPID == $expid ) then
            echo $DATADIR/$expid/atmens/Y$xfbyyyy/M$xfbmm/$expid.atmens_e${prog}.${xfbnymd}_${xfbhh}z.tar >> $ACQRC
         else
            echo "$DATADIR/$SRCEXPID/atmens/Y$xfbyyyy/M$xfbmm/$SRCEXPID.atmens_e${prog}.${xfbnymd}_${xfbhh}z.tar => $expid.atmens_e${prog}.${xfbnymd}_${xfbhh}z.tar" >> $ACQRC
         endif
      else
         echo "${MYNAME}: unfolding ensemble of forecasts from backgroud state $expid.atmens_e${prog}.${xfbnymd}_${xfbhh}z.tar  ... "
         $DRYRUN tar xvf $expid.atmens_e${prog}.${xfbnymd}_${xfbhh}z.tar --wildcards --no-anchored "*${SRCEXPID}.prog.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX"
         if ( $ATMENS_FSO_FSENS == 1 ) then
            $DRYRUN tar xvf $expid.atmens_e${prog}.${xfbnymd}_${xfbhh}z.tar --wildcards --no-anchored "*${SRCEXPID}.fsens*.eta.*-${anymd}_${ahh}z.$NCSUFFIX"
         endif
         if ( $SRCEXPID != $expid && -d $SRCEXPID.atmens_e${prog}.${xfbnymd}_${xfbhh}z ) then
            $DRYRUN /bin/mv $SRCEXPID.atmens_e${prog}.${xfbnymd}_${xfbhh}z $expid.atmens_e${prog}.${xfbnymd}_${xfbhh}z
         endif
         cd prog
         $DRYRUN ln -sf ../$expid.atmens_e${prog}.${xfbnymd}_${xfbhh}z   ${fbnymd}_${fbhh}z
         cd ${fbnymd}_${fbhh}z
         if ( $SRCEXPID != $expid ) then
            foreach dir (`/bin/ls -d mem*`)
              cd $dir
                 foreach fn (`/bin/ls $SRCEXPID.prog.* $SRCEXPID.fsens*`)
                     set sfx = `echo $fn | cut -d. -f2-`
                     ln -sf $fn $expid.$sfx
                 end
              cd -
            end # <dir>
         endif # <expid>
         cd $ENSWORK
      endif
   endif
 endif
endif # ATMENS_FSO_MFCST

if ( $ATMENS_FSO_MFCST == 0 || $ATMENS_FSO_FSENS == 2 ) then
   if ( $ATMENS_FSO_MFCST == 0 ) then
      set ftype = "prog"
      set xtag  = ""
      set mn    = "00"
   endif
   if ( $ATMENS_FSO_FSENS == 2 ) then
      set ftype = "fsens_twe"
      set xtag  = "-${anymd}_${ahh}z"
      set mn    = ""
   endif
# forecasts issued from backgrounds
   if ( ! -e prog/fcsterr/$expid.$ftype.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX ) then
     if ( $DRYRUN == "check" ) then
       if ( ! -e $DATADIR/$SRCEXPID/prog/Y$fbyyyy/M$fbmm/D$fbdd/H$fbhh/$SRCEXPID.$ftype.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX  ) then
          echo "${MYNAME}: missing  $SRCEXPID.$ftype.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX "
          @ notavail = $notavail + 1
       endif
     else
        if ( $action == "setrc" ) then
           echo $DATADIR/$SRCEXPID/prog/Y$fbyyyy/M$fbmm/D$fbdd/H$fbhh/$SRCEXPID.$ftype.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX >> $ACQRC
        else
           $DRYRUN /bin/mv $SRCEXPID.$ftype.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX prog/fcsterr/$expid.$ftype.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX
        endif
      endif
   endif 
# forecasts issued from analysis
   if ( ! -e prog/fcsterr/$expid.$ftype.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX ) then
     if ( $DRYRUN == "check" ) then
       if ( ! -e $DATADIR/$SRCEXPID/prog/Y$fayyyy/M$famm/D$fadd/H$fahh/$SRCEXPID.$ftype.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX ) then
          echo "${MYNAME}: missing  $SRCEXPID.$ftype.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX "
          @ notavail = $notavail + 1
       endif
     else
        if ( $action == "setrc" ) then
           echo $DATADIR/$SRCEXPID/prog/Y$fayyyy/M$famm/D$fadd/H$fahh/$SRCEXPID.$ftype.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX >> $ACQRC
        else
           $DRYRUN /bin/mv $SRCEXPID.$ftype.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX prog/fcsterr/$expid.$ftype.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}${mn}z${xtag}.$NCSUFFIX
        endif
     endif
   endif 
endif # central forecasts

# verification from central
if ( $ATMENS_FSO_AVRFY == 0 ) then
   if ( ! -e prog/fcsterr/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX ) then
     if ( $DRYRUN == "check" ) then
        if ( ! -e $DATADIR/$SRCEXPID/ana/Y$av0yyyy/M$av0mm/$SRCEXPID.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX ) then
           echo "${MYNAME}: missing  $SRCEXPID.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX " 
           @ notavail = $notavail + 1
        endif
     else
        if ( $action == "setrc" ) then
           if ( $SRCEXPID == $expid ) then
              echo  $DATADIR/$expid/ana/Y$av0yyyy/M$av0mm/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX >> $ACQRC
           else
              echo "$DATADIR/$SRCEXPID/ana/Y$av0yyyy/M$av0mm/$SRCEXPID.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX => $expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX" >> $ACQRC
           endif
        else
           $DRYRUN /bin/mv  $expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX prog/fcsterr
        endif
     endif
   endif 
endif # central analysis

# If so, retrieve non-inflated analysis at verification time
if ( $ATMENS_FSO_AVRFY == 1 && $ATMENS_FSO_MFCST <= 1 ) then
   if ( $DRYRUN == "check" ) then
      if ( ! -e $DATADIR/$SRCEXPID/atmens/Y$vnyyyy/M$vnmm/$SRCEXPID.atmens_e$aver.${vnnymd}_${vnhh}z.tar ) then
         echo "${MYNAME}: missing  $SRCEXPID.atmens_e$aver.${vnnymd}_${vnhh}z.tar "
         @ notavail = $notavail + 1
      endif
   else
      if ( $action == "setrc" ) then
         if ( $SRCEXPID == $expid ) then
            echo $DATADIR/$expid/atmens/Y$vnyyyy/M$vnmm/$expid.atmens_e$aver.${vnnymd}_${vnhh}z.tar >> $ACQRC
         else
            echo "$DATADIR/$SRCEXPID/atmens/Y$vnyyyy/M$vnmm/$SRCEXPID.atmens_e$aver.${vnnymd}_${vnhh}z.tar => $expid.atmens_e$aver.${vnnymd}_${vnhh}z.tar" >> $ACQRC
         endif
      else
         echo "${MYNAME}: unfolding non-inflated ensemble of analyses  ... "
         $DRYRUN tar xvf $expid.atmens_e$aver.${vnnymd}_${vnhh}z.tar --wildcards --no-anchored "*${SRCEXPID}.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX"
         if ( $SRCEXPID != $expid && -d $SRCEXPID.atmens_e$aver.${vnnymd}_${vnhh}z ) then
            $DRYRUN /bin/mv $SRCEXPID.atmens_e$aver.${vnnymd}_${vnhh}z $expid.atmens_e$aver.${vnnymd}_${vnhh}z
         endif
         if ( $SRCEXPID != $expid ) then
            cd $expid.atmens_e$aver.${vnnymd}_${vnhh}z
            foreach dir (`/bin/ls -d mem*`)
              cd $dir
                 foreach fn (`/bin/ls *.$aver.*`)
                     set sfx = `echo $fn | cut -d. -f2-`
                     ln -sf $fn $expid.$sfx
                 end
              cd -
            end # <dir>
         endif # <expid>
      endif
   endif
endif # non-inflated ensemble analysis

if ( $action == "setrc" ) then
   if ( -e $ACQRC ) then
       touch $ENSWORK/.DONE_${MYNAME}_$action.$ayyyymmddhh
       echo "${MYNAME}: file $ACQRC created successfully. "
       exit 0
   else   
       echo "${MYNAME}: file $ACQRC not created, aborting ..."
       exit 1
   endif
endif

if ( ! -e $ENSWORK/.DONE_${MYNAME}_setrc.$ayyyymmddhh ) then
   echo "${MYNAME}: cannot find indicator of successful acquire, aborting ..."
   exit 2
endif

# check availability of files
# ---------------------------
if ( $DRYRUN == "check" ) then
   if ( $notavail == 0 ) then
      echo "All files found" 
      exit 0
   else
      echo "Some files missing"
      exit 1
   endif
endif

#=====================================================================
# 2nd phase: when applicable, calc mean, interpolate fields, etc ...
#=====================================================================

cd $ENSWORK

# Calculate mean non-inflated analysis
# ------------------------------------
if ( ! -e ensmean/$expid.$aver.eta.${anymd}_${ahh}00z.$NCSUFFIX ) then
  if ( ! -d ensmean ) mkdir ensmean
    if ( $?AENSTAT_FAST_MPIRUN ) then
       $DRYRUN $AENSTAT_FAST_MPIRUN -o ensmean/$expid.$aver.eta.${anymd}_${ahh}00z.$NCSUFFIX \
                                          mem*/$expid.$aver.eta.${anymd}_${ahh}00z.$NCSUFFIX
    else
       $DRYRUN $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o ensmean/$expid.$aver.eta.${anymd}_${ahh}00z.$NCSUFFIX \
                                                           mem*/$expid.$aver.eta.${anymd}_${ahh}00z.$NCSUFFIX
    endif
endif

set ana_ens_mean = "ensmean/$expid.$aver.eta.${anymd}_${ahh}00z.$NCSUFFIX"
set ens_mres  = `getgfiodim.x $ana_ens_mean`
if ($status) then
    echo "${MYNAME}: error trying to determine ens resolution, aborting ..."
    exit (1)
endif
set eres = "z"
if ( $ens_mres[1] ==   72 ) set eres = "a"
if ( $ens_mres[1] ==  144 ) set eres = "b"
if ( $ens_mres[1] ==  288 ) set eres = "c"
if ( $ens_mres[1] ==  576 ) set eres = "d"
if ( $ens_mres[1] == 1152 ) set eres = "e"
if ( $ens_mres[1] == 2304 ) set eres = "f"
if ( $eres == "z" ) then
    echo "${MYNAME}: error identifying known resolution, aborting ..."
    exit (1)
endif

# convert prognostic fields and verifying analysis/assimilation to ensemble resolution
# ------------------------------------------------------------------------------------
if ( $ATMENS_FSO_MFCST == 0 ) then
   cd $ENSWORK/prog/fcsterr
   foreach fn ( `/bin/ls *prog.eta*.$NCSUFFIX `)
      if ( ! -e $fn.orig ) then
         $DRYRUN /bin/mv $fn  $fn.orig
         $DRYRUN dyn2dyn.x -g5 -res $eres -o $fn $fn.orig
      endif
   end
endif

if ( $ATMENS_FSO_MFCST == 1 ) then
   cd $ENSWORK/prog
#  calculate ensemble mean of forecasts from 
#  analysis ...
   if ( ! -e fcsterr/$expid.prog.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX ) then
      if ( $?AENSTAT_FAST_MPIRUN) then
         echo    $AENSTAT_FAST_MPIRUN -o  fcsterr/$expid.prog.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX ...
         $DRYRUN $AENSTAT_FAST_MPIRUN -o  fcsterr/$expid.prog.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX \
                                             ${fanymd}_${fahh}z/mem*/$expid.prog.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX
      else
         echo    $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o  fcsterr/$expid.prog.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX ...
         $DRYRUN $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o  fcsterr/$expid.prog.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX \
                                                                   ${fanymd}_${fahh}z/mem*/$expid.prog.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX
      endif
   endif
#  and background  ...
   if ( ! -e fcsterr/$expid.prog.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX ) then
      if ( $?AENSTAT_FAST_MPIRUN) then
         echo    $AENSTAT_FAST_MPIRUN -o  fcsterr/$expid.prog.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX ...
         $DRYRUN $AENSTAT_FAST_MPIRUN -o  fcsterr/$expid.prog.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX \
                                             ${fbnymd}_${fbhh}z/mem*/$expid.prog.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX
      else
         echo    $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o  fcsterr/$expid.prog.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX ...
         $DRYRUN $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o  fcsterr/$expid.prog.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hhmn}z.$NCSUFFIX \
                                                                   ${fbnymd}_${fbhh}z/mem*/$expid.prog.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX
      endif
   endif
endif 
if ( $ATMENS_FSO_AVRFY == 0 ) then
   cd $ENSWORK/prog/fcsterr
   foreach fn ( `/bin/ls *${aver}.eta*.$NCSUFFIX `)
      if ( ! -e $fn.orig ) then
         $DRYRUN /bin/mv $fn  $fn.orig
         $DRYRUN dyn2dyn.x -g5 -res $eres -o $fn $fn.orig
      endif
   end
endif
if ( $ATMENS_FSO_AVRFY == 1 && $ATMENS_FSO_MFCST <= 1 ) then
   cd $ENSWORK
   if ( ! -e prog/fcsterr/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX ) then
      if ( $?AENSTAT_FAST_MPIRUN) then
         echo    $AENSTAT_FAST_MPIRUN -o  prog/fcsterr/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX ...
         $DRYRUN $AENSTAT_FAST_MPIRUN -o  prog/fcsterr/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX \
          $expid.atmens_e$aver.${vnnymd}_${vnhh}z/mem*/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX
      else
         echo    $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o  prog/fcsterr/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX ...
         $DRYRUN $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o  prog/fcsterr/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX \
                                $expid.atmens_e$aver.${vnnymd}_${vnhh}z/mem*/$expid.$aver.eta.${av0nymd}_${av0hhmn}z.$NCSUFFIX
      endif
   endif
endif

# build final-time errors
# -----------------------
if ( $ATMENS_FSO_JGRAD ) then
 set rcName = $ATMENSETC/initadj.rc
 set initadjx = `which initadj.x`
 if ( -e $rcName ) then
   cd $ENSWORK/prog/fcsterr
   set norms  = `echorc.x -rc $rcName pert_norm`
   set nnorms = $#norms
   set fnl_tagn = ${av0nymd}_${av0hh}z
   foreach fn ( `/bin/ls *prog.eta*.$NCSUFFIX` )
      set ini_tagn = `echo $fn | cut -d. -f4 | cut -d+ -f1`
      set cur_nymd = `echo $fn | cut -d. -f4 | cut -d+ -f2 |  cut -c1-8`
      set cur_hh   = `echo $fn | cut -d. -f4 | cut -d+ -f2 |  cut -c10-11`; set cur_nhms = ${cur_hh}0000
      if ( ! -e $expid.Jgradf_${norms[1]}.eta.$ini_tagn.$NCSUFFIX ) then
         if ( -e $expid.$aver.eta.${cur_nymd}_${cur_hh}z.$NCSUFFIX ) then
            esma_mpirun -np 1 $initadjx -g5 -pick $cur_nymd $cur_nhms -rc $rcName $fn $expid.$aver.eta.${cur_nymd}_${cur_hh}z.$NCSUFFIX
         else
            echo "${MYNAME}: error, cannot find verifying analysis, aborting ..."
            exit (1)
         endif
         @ ng = 1
         while ( $ng <= $nnorms )
            /bin/mv Jgradf_${norms[$ng]}.eta.nc4 $expid.Jgradf_${norms[$ng]}.eta.${ini_tagn}+${fnl_tagn}.$NCSUFFIX
            @ ng++
         end
         /bin/mv Jnormf.txt  $expid.Jnormf.${ini_tagn}+${fnl_tagn}.txt
      else
         echo "${MYNAME}: already have $expid.Jgradf_${norms[1]}.eta.${ini_tagn}+${fnl_tagn}.$NCSUFFIX ..."
      endif
   end
   # calculate sum of errors/gradients ...
   @ ng = 1
   while ( $ng <= $nnorms )
      set gradfs = `/bin/ls $expid.Jgradf_${norms[$ng]}.eta.*+${fnl_tagn}.$NCSUFFIX`
      set ngrads = $#gradfs
      if ( $ngrads == 2 ) then
         #dynp.x -gsi -scale 0.5 -g5 -pureadd -realp -twoperts -ainc -adm -res $eres \
         dynp.x -gsi -scale 1 -g5 -pureadd -realp -twoperts -ainc -res $eres \
                  -s $gradfs[1] -p  $gradfs[2] -os Jgradf_${norms[$ng]}.eta.nc4
      else
         echo "${MYNAME}: error, cannot handle $ngrads gradient vectors, aborting ..."
         exit (1)
      endif
      @ ng++
   end
 else
    echo "${MYNAME}: error, cannot find initadj.rc file, aborting ..."
    exit (1)
 endif
endif

# Calculate ensemble mean of ensemble forecast sensitivities, ... wired norm for now
# ----------------------------------------------------------------------------------
if ( $ATMENS_FSO_FSENS == 1 ) then
  cd $ENSWORK/prog
  # from Xa
  if ( $?AENSTAT_FAST_MPIRUN) then
     echo    $AENSTAT_FAST_MPIRUN ... ${fahh}z $expid.fsens_twe.eta.${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX
     $DRYRUN $AENSTAT_FAST_MPIRUN -o fcsterr/$expid.fsens_twe.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX \
                     ${fanymd}_${fahh}z/mem*/$expid.fsens_twe.eta.${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX
  else
     echo    $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc ... ${fahh}z $expid.fsens_twe.eta.${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX
     $DRYRUN $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc \
                             -o  fcsterr/$expid.fsens_twe.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX \
                 ${fanymd}_${fahh}z/mem*/$expid.fsens_twe.eta.${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX
  endif

  # from Xb
  if ( $?AENSTAT_FAST_MPIRUN) then
     echo    $AENSTAT_FAST_MPIRUN ... ${fbhh}z $expid.fsens_twe.eta.${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX
     $DRYRUN $AENSTAT_FAST_MPIRUN -o fcsterr/$expid.fsens_twe.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX \
                     ${fbnymd}_${fbhh}z/mem*/$expid.fsens_twe.eta.${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX
  else
     echo    $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc ... ${fbhh}z $expid.fsens_twe.eta.${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX
     $DRYRUN $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc \
                             -o  fcsterr/$expid.fsens_twe.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX \
                 ${fbnymd}_${fbhh}z/mem*/$expid.fsens_twe.eta.${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX
  endif
endif

# Add the two input sensitivity vectors, ... wired norm for now 
# -------------------------------------------------------------
if ( $ATMENS_FSO_FSENS > 0 ) then
   cd $ENSWORK/prog/fcsterr
   if ( -e $expid.fsens_twe.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX && \
        -e $expid.fsens_twe.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX  ) then
      $DRYRUN dynp.x -gsi -scale 0.5 -g5 -pureadd -realp -twoperts -ainc -adm -res $eres \
                     -s  $expid.fsens_twe.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX \
                     -p  $expid.fsens_twe.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX \
                     -os $expid.fsens_twe.eta.${anymd}_${ahh}z.$NCSUFFIX
      if ($status) then
         echo "${MYNAME}: error, failed executing dynp, aborting ..."
         exit (1)
      endif
      ln -sf $expid.fsens_twe.eta.${anymd}_${ahh}z.$NCSUFFIX fsens.eta.${anymd}_${ahh}z.$NCSUFFIX
   else
      echo "${MYNAME}: error, cannot find fcst sensitivities vectors ..."
      echo "${MYNAME}: missing either:  $expid.fsens_twe.eta.${fanymd}_${fahh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX "
      echo "${MYNAME}:             or:  $expid.fsens_twe.eta.${fbnymd}_${fbhh}z+${av0nymd}_${av0hh}z-${anymd}_${ahh}z.$NCSUFFIX "
      echo "${MYNAME}: or both, aborting ..."
      exit(1)
   endif
   cd $ENSWORK
endif

# Move prog/fcst to updated_ens and link back to workdir
# ------------------------------------------------------
cd $ENSWORK
if (! -d updated_ens ) mkdir updated_ens
if (! -d updated_ens/fcsterr ) then
   /bin/mv prog/fcsterr updated_ens
   cd prog
   ln -sf $ENSWORK/updated_ens/fcsterr . 
   cd -
endif

# If made it to here ...
touch $ENSWORK/.DONE_${MYNAME}.$ayyyymmddhh
exit(0)
