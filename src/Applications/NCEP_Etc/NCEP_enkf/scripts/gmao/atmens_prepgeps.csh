#!/bin/csh

setenv DRYRUN "check"
setenv DRYRUN echo
setenv DRYRUN 

# post_prepegps.csh - prepare environment for running GEOS EPS
#
# !REVISION HISTORY:
#
#  17Apr2017  Todling   Initial script
#  07Aug2017  Todling   Mechanism to allow retriving tarball with
#                       additional set of ensemble analysis
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

# local env vars (not to become global)
setenv MYNAME atmens_prepgeps.csh

if ( $#argv < 7 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - prepare environment for GEOS ensemble-prediction system (GEPS)"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  expid nymd nhms ftau atype aver action"
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  date of analysis, as in YYYYMMDD"
   echo "   nhms   -  time of analysis, as in HHMMSS"
   echo "   ftau   -  forecast interval EFSO calculated for (in hours)"
   echo "   atype  -  analysis type (ana or niana) - for initializing model"
   echo "   aver   -  verification type (casm, cana, emana, niana)"
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
   echo "  $MYNAME b541iau 20091019 000000 ana setrc"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    AENSTAT_MPIRUN   - mp_stats MPI command line         "
   echo "    ATMENSETC        - location of experiment            "
   echo "    FVWORK           - location of work directory        "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENS_GEPS_RECENTER 1: use to recenter ensemble analysis"
   echo "                         (default: 0)"
   echo "    ATMENS_NCPUSTAR   - number of CPUS used for untar (default: 32) "
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
if ( !($?AENS_GAAS_OPT) ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVWORK)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1

if ( !($?ATMENS_GEPS_RECENTER)     ) setenv ATMENS_GEPS_RECENTER     0  # 1= will recenter analysis
if ( !($?ATMENS_GEPS_FROM_CENTRAL) ) setenv ATMENS_GEPS_FROM_CENTRAL 0  # 1= forecast from central rst/bkg
if ( !($?ATMENS_NCPUSTAR) ) setenv ATMENS_NCPUSTAR 32

if ( !($?SRCEXPID)      ) setenv SRCEXPID NULL
if ( !($?DATADIR)       ) setenv DATADIR $ARCHIVE
if ( !($?NCSUFFIX)      ) setenv NCSUFFIX nc4 
if ( !($?REGRID_QOS)    ) then
   if ( !($?JOBGEN_QOS) ) then
       setenv REGRID_QOS compute
   else
       setenv REGRID_QOS $JOBGEN_QOS
   endif
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set expid  = $1
set anymd  = $2
set anhms  = $3
set ftau   = $4
set atype  = $5
set aver   = $6
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

# three hours from analysis time
@ three_hrs_sec = 3 * 3600
set date3 = (`tick $anymd $anhms  $three_hrs_sec`)
set a3nymd = $date3[1]
set a3nhms = $date3[2]
set a3yyyy = `echo $a3nymd | cut -c1-4`
set a3mm   = `echo $a3nymd | cut -c5-6`
set a3hh   = `echo $a3nhms | cut -c1-2`
set a3yyyymmddhh = ${a3nymd}${a3hh}

# Previous analysis time
# ----------------------
#set pdate = (`tick $anymd $anhms -$sixhours`)
#set pnymd = $pdate[1]
#set pnhms = $pdate[2]
#set pyyyy = `echo $pnymd | cut -c1-4`
#set pmm   = `echo $pnymd | cut -c5-6`
#set phh   = `echo $pnhms | cut -c1-2`
#set pyyyymmddhh = ${nymd}${hh}

# Verification time
# -----------------
set av0date = ( `tick $anymd $anhms $ftau_sec` )
set av0nymd = $av0date[1]
set av0nhms = $av0date[2]
set av0yyyy = `echo $av0nymd | cut -c1-4`
set av0mm   = `echo $av0nymd | cut -c5-6`
set av0hh   = `echo $av0nhms | cut -c1-2`
set av0yyyymmddhh = ${av0nymd}${av0hh}

set xav0date = ( `tick $av0date $ana_offset` )
set xav0nymd = $xav0date[1]
set xav0nhms = $xav0date[2]
set xav0yyyy = `echo $xav0nymd | cut -c1-4`
set xav0mm   = `echo $xav0nymd | cut -c5-6`
set xav0hh   = `echo $xav0nhms | cut -c1-2`

# For the time being, there is a time-lapse in the tarball namings hold the prognostic fields
# -------------------------------------------------------------------------------------------
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


#=====================================================================
# 1st phase: acquire and position required input for ob-sensitivity 
#=====================================================================
@ notavail = 0
if ( $action == "setrc" ) then
   setenv ACQRC ageps.rc
   if ( -e $ACQRC ) /bin/rm $ACQRC
   touch $ACQRC
endif

# Unfold tar balls, copy analysis, and establish links as desired ... 
# -------------------------------------------------------------------

if ( $ATMENS_GEPS_FROM_CENTRAL ) then

   # wired for now:
   set nmem = 32
   set erst_hres = C90
   set erst_vres = 72
   set ebkg_hres = c
   if ( ! -d centralRST ) then
      if ( $action == "setrc" ) then
           if ( $SRCEXPID == $expid ) then
              echo $DATADIR/$expid/rs/Y$yyyy/M$mm/$expid.rst.${nymd}_${hh}z.tar >> $ACQRC
           else
              echo "$DATADIR/$SRCEXPID/rs/Y$yyyy/M$mm/$SRCEXPID.rst.${nymd}_${hh}z.tar => $expid.rst.${nymd}_${hh}z.tar" >> $ACQRC
           endif
      else
           if ( ! -e $SRCEXPID.rst.${nymd}_${hh}z.tar ) then
              echo "${MYNAME}: cannot find file type central rst tar ball, aborting  ... "
              exit (1)
           else
              if (! -d centralRST/Ori ) then
                 mkdir -p centralRST/Ori
                 mkdir -p centralRST/New
                 cd centralRST/Ori
                 if ( $ATMENS_NCPUSTAR > 1 ) then
                    parallel-untar.py  ../../$SRCEXPID.rst.${nymd}_${hh}z.tar $ATMENS_NCPUSTAR
                 else 
                    tar xvf ../../$SRCEXPID.rst.${nymd}_${hh}z.tar
                 endif
                 cd ../
                 set inpdir = `echo $cwd`
                 set outdir = $inpdir/New
                 set inpdir = $inpdir/Ori
                 regrid.pl -ymd $nymd -hr $hh -grout $erst_hres -levsout $erst_vres -outdir $outdir -d $inpdir \
                           -expid $SRCEXPID -tagin Icarus -oceanin CS -i -nobkg -nolbl -nolcv \
                           -tagout Icarus -rs 3 -oceanout CS -grpID $GID -qos $REGRID_QOS -np
                 /bin/mv Ori/*.bkg*sfc*.$NCSUFFIX  New/
                 /bin/mv Ori/*.cbkg*eta*.$NCSUFFIX New/
                 cd New
                 foreach fn (`/bin/ls $SRCEXPID.cbkg*eta*$NCSUFFIX`)
                    set sfx = `echo $fn | cut -d. -f3-`
                    /bin/mv $fn $expid.cbkg.eta.$sfx
                 end
                 foreach fn (`/bin/ls $SRCEXPID.bkg*sfc*$NCSUFFIX`)
                    set sfx = `echo $fn | cut -d. -f3-`
                    /bin/mv $fn $expid.bkg.sfc.$sfx
                 end
                 foreach fn (`/bin/ls ${erst_hres}-CS_${SRCEXPID}.*`)
                    set sfx = `echo $fn | cut -d. -f2-` 
                    /bin/mv $fn $expid.$sfx
                 end
                 cd ../Ori
                 foreach fn (`/bin/ls $SRCEXPID.bkg*eta*$NCSUFFIX`)
                    set sfx = `echo $fn | cut -d. -f3-`
                    dyn2dyn.x -g5 -res $ebkg_hres -o ../New/$expid.bkg.eta.$sfx $fn
                 end
                 cd $ENSWORK
                 @ ic = 0
                 while ($ic < $nmem )
                    @ ic++
                    set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
                    if (! -d mem$memtag ) mkdir mem$memtag
                    cd mem${memtag}
                    ln -sf ../centralRST/New/${expid}.*.bin       .
                    ln -sf ../centralRST/New/${expid}.*.$NCSUFFIX .
                    cd -
                 end
              endif
           endif
      endif
   endif 

else # ATMENS_GEPS_FROM_CENTRAL=0 - forecast from ens member RSTs

# ensemble of backgrounds (needed to construct IAU tendencies) and model RSTs
  foreach ftype ( ebkg erst )
    
   if ( ! -d $expid.atmens_${ftype}.${nymd}_${hh}z ) then
      if ( $DRYRUN == "check" ) then
         if ( ! -e $DATADIR/$SRCEXPID/atmens/Y$yyyy/M$mm/$expid.atmens_${ftype}.${nymd}_${hh}z.tar ) then
            echo "${MYNAME}: missing  $SRCEXPID.atmens_${ftype}.${nymd}_${hh}z.tar "
            @ notavail = $notavail + 1
         endif
      else
         if ( $action == "setrc" ) then
            if ( $SRCEXPID == $expid ) then
               echo $DATADIR/$expid/atmens/Y$yyyy/M$mm/$expid.atmens_${ftype}.${nymd}_${hh}z.tar >> $ACQRC
            else
               echo "$DATADIR/$SRCEXPID/atmens/Y$yyyy/M$mm/$SRCEXPID.atmens_${ftype}.${nymd}_${hh}z.tar => $expid.atmens_${ftype}.${nymd}_${hh}z.tar" >> $ACQRC
            endif
         else
            if ( ! -d $expid.atmens_${ftype}.${nymd}_${hh}z ) then
               echo "${MYNAME}: unfolding ensemble of backgrounds ($ftype)  ... "
               if ( ! -e $expid.atmens_${ftype}.${nymd}_${hh}z.tar ) then
                  echo "${MYNAME}: cannot find file type $ftype , aborting  ... "
                  exit (1)
               endif
               if ( $ATMENS_NCPUSTAR > 1 ) then
                  $DRYRUN parallel-untar.py  $expid.atmens_${ftype}.${nymd}_${hh}z.tar $ATMENS_NCPUSTAR
               else 
                  $DRYRUN tar xvf $expid.atmens_${ftype}.${nymd}_${hh}z.tar 
               endif
               if ( $SRCEXPID != $expid && -d $SRCEXPID.atmens_${ftype}.${nymd}_${hh}z ) then
                  /bin/mv $SRCEXPID.atmens_${ftype}.${nymd}_${hh}z $expid.atmens_${ftype}.${nymd}_${hh}z
               endif
               cd $expid.atmens_${ftype}.${nymd}_${hh}z
               foreach dir (`/bin/ls -d mem*`)
                      if ( ! -d $ENSWORK/$dir ) mkdir $ENSWORK/$dir
                      /bin/mv $dir/$SRCEXPID.* $ENSWORK/$dir
               end
               cd $ENSWORK
               if ( $SRCEXPID != $expid ) then   # rename all files in directory when diff expid
                  foreach dir (`/bin/ls -d mem*`)
                      cd $dir
                      foreach fn ( `/bin/ls $SRCEXPID.* `)
                         set sfx = `echo $fn | cut -d. -f2-` 
                         /bin/mv $fn $expid.$sfx
                      end
                      cd -
                  end
               endif
            endif
         endif
      endif
   endif
  end
endif # ATMENS_GEPS_FROM_CENTRAL

# ensemble of analyses at initial time
set xtratime = ""
if ( -e $FVHOME/run/ageps/atmens_efsens_${ahh}.rc ) then
   echorc.x -rc $FVHOME/run/ageps/atmens_efsens_${ahh}.rc ana_time_gap_hr
   if (! $status ) then
      set xtratime = `echorc.x -rc $FVHOME/run/ageps/atmens_efsens_${ahh}.rc ana_time_gap_hr`
      @ xtratime = $xtratime * 3600
      if ( $xtratime == 0 ) set xtratime = ""
   endif
endif
foreach times ( 1 $xtratime )
 foreach ttype ( $atype )

  if ( $times == 1 ) then
     set tnymd = $nnymd
     set thh   = $nhh
     set tyyyy = $nyyyy
     set tmm   = $nmm
     set gnymd = $anymd
     set ghh   = $ahh
  else
     set tanadate = (`tick $anymd $anhms $xtratime`)
     set gnymd = $tanadate[1]
     set ghh   = `echo $tanadate[2] | cut -c1-2`
     set tanadate = (`tick $nnymd $nnhms $xtratime`)
     set tnymd = $tanadate[1]
     set tnhms = $tanadate[2]
     set tyyyy = `echo $tnymd | cut -c1-4`
     set tmm   = `echo $tnymd | cut -c5-6`
     set thh   = `echo $tnhms | cut -c1-2`
  endif

  if ( ! -d $expid.atmens_e${ttype}.${tnymd}_${thh}z ) then
   if ( $DRYRUN == "check" ) then
      if ( ! -e $DATADIR/$SRCEXPID/atmens/Y$tyyyy/M$tmm/$SRCEXPID.atmens_e${ttype}.${tnymd}_${thh}z.tar ) then
         echo "${MYNAME}: missing  $SRCEXPID.atmens_e${ttype}.${tnymd}_${thh}z.tar "
         @ notavail = $notavail + 1
      endif
   else
      if ( $action == "setrc" ) then
         if ( $SRCEXPID == $expid ) then
            echo $DATADIR/$expid/atmens/Y$tyyyy/M$tmm/$expid.atmens_e${ttype}.${tnymd}_${thh}z.tar >> $ACQRC
         else
            echo "$DATADIR/$SRCEXPID/atmens/Y$tyyyy/M$tmm/$SRCEXPID.atmens_e${ttype}.${tnymd}_${thh}z.tar => $expid.atmens_e${ttype}.${tnymd}_${thh}z.tar" >> $ACQRC
         endif
      else
         echo "${MYNAME}: unfolding non-inflated ensemble of analyses  ... "
         $DRYRUN tar xvf $expid.atmens_e${ttype}.${tnymd}_${thh}z.tar --wildcards --no-anchored "*${SRCEXPID}.${ttype}.eta.${gnymd}_${ghh}00z.$NCSUFFIX"
         if ( $SRCEXPID != $expid && -d $SRCEXPID.atmens_e${ttype}.${tnymd}_${thh}z ) then
            $DRYRUN /bin/mv $SRCEXPID.atmens_e${ttype}.${tnymd}_${thh}z $expid.atmens_e${ttype}.${tnymd}_${thh}z
         endif
         foreach dir (`/bin/ls -d mem*`)
            /bin/mv $expid.atmens_e${ttype}.${tnymd}_${thh}z/$dir/$SRCEXPID.* $ENSWORK/$dir/
         end
         # if so, rename files consistent with current expid
         if ( $SRCEXPID != $expid ) then
            foreach dir (`/bin/ls -d mem*`)
               cd $dir
               foreach fn (`/bin/ls *.${ttype}.*`)
                   set sfx = `echo $fn | cut -d. -f2-`
                   /bin/mv $fn $expid.$sfx
               end
               cd -
            end # <dir>
         endif # <expid>
         # link analysis to file following naming convension (with mem-tag name) 
         if ( $times == 1 ) then
            foreach dir (`/bin/ls -d mem*`)
               cd $dir
               foreach fn (`/bin/ls *.${ttype}.*${gnymd}_${ghh}z.$NCSUFFIX`)
                   set dat = `echo $fn | cut -d. -f4`
                   set sfx = `echo $fn | cut -d. -f5-`
                   ln -sf $fn $expid.ana.eta.$dat.$dir.$sfx
               end
               cd -
            end # <dir>
         endif
      endif
   endif
  endif

 end
end

# Get GAAS-related inputs ...
# ---------------------------
if ($AENS_GAAS_OPT == 1) then
   cd $ENSWORK
   if ( ! -d central ) mkdir -p central
   foreach date_hrz ( ${anymd}_${ahh}00z ${a3nymd}_${a3hh}00z )
    foreach aod_type ( aod_a aod_k aod_f )
     if ( ! -e central/$expid.$aod_type.sfc.${date_hrz}.$NCSUFFIX ) then 
        if ( $action == "setrc" ) then
           if ( $SRCEXPID == $expid ) then
              echo $DATADIR/$expid/chem/Y$ayyyy/M$amm/$expid.$aod_type.sfc.${date_hrz}.$NCSUFFIX >> $ACQRC
           else
              echo "$DATADIR/$SRCEXPID/chem/Y$ayyyy/M$amm/$SRCEXPID.$aod_type.sfc.${date_hrz}.$NCSUFFIX => $expid.$aod_type.sfc.${date_hrz}.$NCSUFFIX" >> $ACQRC
           endif
        else
           cd central
           # link back to make sure files are not brought into run dir each time script gets re-submitted
           ln -sf ../$expid.$aod_type.sfc.${date_hrz}.$NCSUFFIX .
           cd -
        endif
     endif
    end # aod-type
   end # date-hour
   if ( $action != "setrc" ) then
     cd $ENSWORK/central
     set lst_aod_files = (`/bin/ls *.aod_*.$NCSUFFIX`)
     cd $ENSWORK
     foreach mdir (`/bin/ls -d mem*`)
       set nnn = `echo $mdir | cut -c4-6`
       cd $mdir
       foreach fn ( $lst_aod_files )
         set pfx = `echo $fn | cut -d. -f1-4`
         ln -sf ../central/$fn $pfx.mem$nnn.$NCSUFFIX
       end
       cd -
     end
   endif
else 
   echo "${MYNAME}: cannot handle GAAS option, check AENS_GAAS_OPT, aborting ..."
   exit 1
endif

if ($ATMENS_GEPS_RECENTER) then
   cd $ENSWORK
   if ( ! -d central ) mkdir -p central
   if ( ! -e central/$expid.ana.eta.${anymd}_${ahh}00z.$NCSUFFIX ) then 
      if ( $action == "setrc" ) then
         if ( $SRCEXPID == $expid ) then
            echo $DATADIR/$expid/ana/Y$ayyyy/M$amm/$expid.ana.eta.${anymd}_${ahh}00z.$NCSUFFIX >> $ACQRC
         else
            echo "$DATADIR/$SRCEXPID/ana/Y$ayyyy/M$amm/$SRCEXPID.ana.eta.${anymd}_${ahh}00z.$NCSUFFIX => $expid.ana.eta.${anymd}_${ahh}00z.$NCSUFFIX" >> $ACQRC
         endif
      else
         if ( -e $expid.ana.eta.${anymd}_${ahh}00z.$NCSUFFIX ) then
            /bin/mv $expid.ana.eta.${anymd}_${ahh}00z.$NCSUFFIX central/
            if ( $atype != "ana" ) then # link needed to ease connection w/ recentering script
               cd central
               ln -sf $expid.ana.eta.${anymd}_${ahh}00z.$NCSUFFIX $expid.$atype.eta.${anymd}_${ahh}00z.$NCSUFFIX 
               cd -
            endif
         endif
      endif
   endif
endif

# verification options: central assimilation or central analysis
if ( $aver == "casm" || $aver == "cana"  ) then
   set Aver = `echo $aver | cut -c2-`
   if ( ! -e $expid.$Aver.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX ) then
      if ( $action == "setrc" ) then
         if ( $SRCEXPID == $expid ) then
            echo  $DATADIR/$expid/ana/Y$av0yyyy/M$av0mm/$expid.$Aver.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX >> $ACQRC
         else
            echo "$DATADIR/$SRCEXPID/ana/Y$av0yyyy/M$av0mm/$SRCEXPID.$Aver.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX => $expid.$Aver.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX" >> $ACQRC
         endif
      else
         foreach dir (`/bin/ls -d mem*`)
           cd $dir
             ln -s ../$expid.$Aver.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX .
           cd -
         end
      endif
   endif 
endif # central analysis

# verification option: non-inflated analysis 
if ( $aver == "niana" ) then
   if ( $action == "setrc" ) then
      if ( $SRCEXPID == $expid ) then
         echo $DATADIR/$expid/atmens/Y$xav0yyyy/M$xav0mm/$expid.atmens_eniana.${xav0nymd}_${xav0hh}z.tar >> $ACQRC
      else
         echo "$DATADIR/$SRCEXPID/atmens/Y$xav0yyyy/M$xav0mm/$SRCEXPID.atmens_eniana.${xav0nymd}_${xav0hh}z.tar => $expid.atmens_eniana.${xav0nymd}_${xav0hh}z.tar" >> $ACQRC
      endif
   else
      echo "${MYNAME}: unfolding non-inflated ensemble of analyses  ... "
      $DRYRUN tar xvf $expid.atmens_eniana.${xav0nymd}_${xav0hh}z.tar --wildcards --no-anchored "*${SRCEXPID}.niana.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX"
      if ( $SRCEXPID != $expid && -d $SRCEXPID.atmens_eniana.${xav0nymd}_${xav0hh}z ) then
         $DRYRUN /bin/mv $SRCEXPID.atmens_eniana.${xav0nymd}_${xav0hh}z $expid.atmens_eniana.${xav0nymd}_${xav0hh}z
      endif
      foreach dir (`/bin/ls -d mem*`)
         /bin/mv $expid.atmens_eniana.${xav0nymd}_${xav0hh}z/$dir/$SRCEXPID.* $ENSWORK/$dir/
      end
      if ( $SRCEXPID != $expid ) then
         foreach dir (`/bin/ls -d mem*`)
            cd $dir
            foreach fn (`/bin/ls *.niana.*${xav0nymd}_${xav0hh}z*`)
                set sfx = `echo $fn | cut -d. -f2-`
                ln -sf $fn $expid.$sfx
            end
            cd -
         end # <dir>
      endif # <expid>
   endif
endif # non-inflated ensemble analysis for verification

# verification option: ensemble mean analysis 
if ( $aver == "emana" ) then
   if ( $action == "setrc" ) then
      if ( $SRCEXPID == $expid ) then
         echo $DATADIR/$expid/atmens/Y$xav0yyyy/M$xav0mm/$expid.atmens_stat.${xav0nymd}_${xav0hh}z.tar >> $ACQRC
      else
         echo "$DATADIR/$SRCEXPID/atmens/Y$xav0yyyy/M$xav0mm/$SRCEXPID.atmens_stat.${xav0nymd}_${xav0hh}z.tar => $expid.atmens_stat.${xav0nymd}_${xav0hh}z.tar" >> $ACQRC
      endif
   else
      echo "${MYNAME}: unfolding non-inflated ensemble of analyses  ... "
      $DRYRUN tar xvf $expid.atmens_stat.${xav0nymd}_${xav0hh}z.tar --wildcards --no-anchored "ensmean/*${SRCEXPID}.ana.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX"
      if ( $SRCEXPID != $expid && -d $SRCEXPID.atmens_stat.${xav0nymd}_${xav0hh}z ) then
         $DRYRUN /bin/mv $SRCEXPID.atmens_stat.${xav0nymd}_${xav0hh}z $expid.atmens_stat.${xav0nymd}_${xav0hh}z
      endif
      if (! -d ensmean ) mkdir ensmean
      /bin/mv $expid.atmens_stat.${xav0nymd}_${xav0hh}z/ensmean/$SRCEXPID.ana.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX ensmean/$expid.ana.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX
      foreach dir (`/bin/ls -d mem*`)
         cd $dir
         ln -sf ../ensmean/$expid.ana.eta.${av0nymd}_${av0hh}00z.$NCSUFFIX .
         cd -
      end # <dir>
   endif
endif # ensemble mean analysis for verification


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

if ( $ATMENS_GEPS_RECENTER ) then
   if ( ! -e ensmean/$expid.${atype}.eta.${anymd}_${ahh}00z.$NCSUFFIX ) then
     if ( ! -d ensmean ) mkdir ensmean
     $DRYRUN $AENSTAT_MPIRUN -rc $ATMENSETC/mp_stats.rc -o ensmean/$expid.${atype}.eta.${anymd}_${ahh}00z.$NCSUFFIX \
                                                              mem*/$expid.${atype}.eta.${anymd}_${ahh}00z.$NCSUFFIX
   endif
   mkdir torecenter
   cd torecenter
   ln -s ../mem* .
   atmens_recenter.csh $expid $anymd $anhms $atype.eta $atype.eta $ENSWORK/torecenter $ENSWORK/central $ENSWORK/$AENSADDINFLOC
   if ( $status ) then
      echo " ${MYNAME}: error recentering analysis, aborting ..."
      exit(1)
   endif
   cd $ENSWORK
endif

# If made it to here ...
touch $ENSWORK/.DONE_${MYNAME}.$ayyyymmddhh
exit(0)
