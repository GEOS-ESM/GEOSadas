#!/bin/csh

# atmens_arch.csh - archive components of the ensemble
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  22Nov2011  Todling   Allow to arquive separate components
#                        eana - arch ana's of all members
#                        easm - arch asm.eta fields of all members
#                        eniana - arch iana's of all members
#                        ebaer- arch aerosol bkg files of all members
#                        eaaer- arch aerosol ana files of all members
#                        ebkg - arch bkg's of all members
#                        ecbkg- arch cbkg's of all members
#                        edia - arch diagnostic files for all members
#                        eoi0 - arch obs-imp on analysis
#                        eprg - arch prognostic fields of all members
#                        erst - arch rst's of all members
#                        evtk - arch vortex tracker information
#                        eoimp- arch ensemble obs-impact-related output
#                        fstat- arch forecast statistics
#                        stat - arch statistics (default)
#                        xtra - arch statistics
#  21Oct2012  Todling   Extra checks on what to store
#  10Dec2012  Todling   Slight revision of members storage
#                       No longer zip tar balls
#  11Feb2013  Todling   Handle for eprg
#  16Feb2013  Todling   Handle for obs-imp on ana: eoi0
#  19Jun2014  Todling   Handle for diagnostic output
#  03Sep2014  Todling   Add ENSARCH_ALLBKG to better handle bkg files
#  03Sep2015  Todling   Add evtk as known type to be stored
#  02Nov2016  Todling   Set silo directory to be date-specific
#  21Mar2017  Todling   Revisit aerosol-related tar balls
#  15Apr2017  Todling   RC file passed as argument
#  15May2017  Todling   Arch dir name also passed as argument
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME "atmens_arch.csh"

if ( $#argv < 6 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "   $MYNAME  - prepare to archive atmos-ensemble"
   echo " "
   echo " SYNOPSIS  "
   echo " "
   echo "  $MYNAME  expid nymd nhms rc id dir "
   echo " "
   echo " where"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nymd   -  initial date of ensemble forecast, as in YYYYMMDD "
   echo "   nhms   -  initial time of ensemble forecast, as HHMMSS"
   echo "   rc     -  full path name of storage archive file"
   echo "   id     -  work identifier"
   echo "   dir    -  directory where files for arch are located"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "  Collect files to arquive after Ensemble DAS completes a cycle"
   echo " "
   echo "  Example of valid command line:"
   echo "   $MYNAME b541iau 20091018 000000 SOMEDIR/atmens_storage.arc edas /nobackup/mydir"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES: "
   echo " "
   echo "    ARCHLOC       - location of archive, e.g., /archive/u/$user"
   echo "    ATMENSETC     - location of EnKF resource files   "
   echo "    ATMENS4ARCH   - location of current ensemble      "
   echo "    FVHOME        - location of experiment            "
   echo "    FVROOT        - location of DAS build             "
   echo "    GID           - group ID to run job under         "
   echo "    VAROFFSET     - analysis offset from initial time "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES: "
   echo " "
   echo "    ENSARCH_ALLBKG    - when set, arch all bkg files regardless of hour"
   echo "                          (Default: arch only central bkg files)"
   echo "    ENSARCH_KEEP      - when set will not remove dir with files for arch"
   echo "    ENSARCH_FIELDS    - components (list separate by comma), e.g.,"
   echo "                          eana,ebkg,edia,eprg,easm,erst,eoi0,stat,xtra "
   echo "                          (Default: stat)          "
   echo "    ENSARCH_WALLCLOCK - location of archive, e.g., /archive/u/$user"
   echo "    NCSUFFIX          - SDF suffix (detault: nc4)"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 28Jul2017      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

set expid = $1
set nymdb = $2
set nhmsb = $3
set arcfile = $4
set workid  = $5
set dir4arc = $6
set yyyyb = `echo $nymdb | cut -c1-4`
set mmb   = `echo $nymdb | cut -c5-6`
set hhb   = `echo $nhmsb | cut -c1-2`
set yyyymmddhh = ${nymdb}${hhb}

setenv FAILED 0
if ( !($?ARCHLOC)       ) setenv FAILED 1
if ( !($?ATMENS4ARCH)   ) setenv FAILED 1
if ( !($?ATMENSETC)     ) setenv FAILED 1
if ( !($?FVHOME)        ) setenv FAILED 1
if ( !($?FVROOT)        ) setenv FAILED 1
if ( !($?GID)           ) setenv FAILED 1
if ( !($?NCSUFFIX)      ) setenv FAILED 1
if ( !($?VAROFFSET)     ) setenv FAILED 1

if ( !($?ENSARCH_ALLBKG)   ) setenv ENSARCH_ALLBKG 0 
if ( !($?ENSARCH_FIELDS)   ) setenv ENSARCH_FIELDS "stat"
if ( !($?ENSARCH_WALLCLOCK)) setenv ENSARCH_WALLCLOCK 2:00:00
if ( !($?ENSARCH_KEEP)     ) setenv ENSARCH_KEEP 0 

if ( $FAILED ) then
  env
  echo " \e[0;31m ${MYNAME}: not all required env vars defined \e[0m"
  exit 1
endif

#source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

@ offset_sec =  $VAROFFSET * 60

set members = `/bin/ls -d $ATMENS4ARCH/$dir4arc/mem* | wc`
set nmem = $members[1]

cd $ATMENS4ARCH/$dir4arc

# figure out what to archive
set nc  = ` echo $ENSARCH_FIELDS | awk -F"," '{print NF-1}' `
@ nc = $nc + 1
@ ic = 0
set y = ""
while ( $ic < $nc )
   @ ic = $ic + 1
   set x = `echo $ENSARCH_FIELDS | cut -d, -f$ic`
   set y = ( $y $x )
end
set myarch_list = ( $y )

if ( "$myarch_list" == "" ) then
   echo "\e[0;32m ${MYNAME}: nothing to archive \e[0m"
   exit(0)
endif

# create subdirs where to move members and results into ...
foreach ball ( $myarch_list ) 
   set myball = $expid.atmens_${ball}.${nymdb}_${hhb}z
   if ( -e $ATMENS4ARCH/$dir4arc/$myball.tar ) /bin/rm $ATMENS4ARCH/$dir4arc/$myball.tar
   mkdir -p $myball
end

# store background members only ...
set myball = $expid.atmens_ebkg.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb $offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      mkdir -p $myball/mem$memtag
      if ( $ENSARCH_ALLBKG ) then
         foreach type ( "bkg.eta" "bkg.sfc" )
            /bin/mv mem$memtag/*.${type}.*.$NCSUFFIX $myball/mem$memtag/
         end
      else
         foreach type ( "bkg.eta.${nymds}_${hhs}z" "bkg.sfc.${nymds}_${hhs}z" )
            /bin/mv mem$memtag/*.${type}.$NCSUFFIX $myball/mem$memtag/
         end
      endif
   end
endif
# store background members only ...
set myball = $expid.atmens_ecbkg.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb $offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      mkdir -p $myball/mem$memtag
      if ( $ENSARCH_ALLBKG ) then
         foreach type ( "cbkg.eta" )
            /bin/mv mem$memtag/*.${type}.*.$NCSUFFIX $myball/mem$memtag/
         end
      else
         foreach type ( "cbkg.eta.${nymds}_${hhs}z" )
            /bin/mv mem$memtag/*.${type}.$NCSUFFIX $myball/mem$memtag/
         end
      endif
   end
endif
# store aerosol background fields only ...
set myball = $expid.atmens_ebaer.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb $offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      mkdir -p $myball/mem$memtag
      if ( $ENSARCH_ALLBKG ) then
         foreach type ( "gaas_bkg.sfc" "abkg.eta" "aod_f.sfc" )
            /bin/mv mem$memtag/*.${type}.*.$NCSUFFIX $myball/mem$memtag/
         end
      else
         foreach type ( "gaas_bkg.sfc.${nymds}_${hhs}z" "abkg.eta.${nymds}_${hhs}z" "aod_f.sfc.${nymds}_${hhs}00z" )
            /bin/mv mem$memtag/*.${type}.$NCSUFFIX $myball/mem$memtag/
         end
      endif
   end
endif
# store aerosol analysis fields only ...
set myball = $expid.atmens_eaaer.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb $offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      mkdir -p $myball/mem$memtag
      if ( $ENSARCH_ALLBKG ) then
         foreach type ( "gaas_ana.sfc" "aana.eta" "aker.eta" "aod_a.sfc" "aod_d.sfc" "aod_k.sfc" "recalc_aod_a.sfc" )
            /bin/mv mem$memtag/*.${type}.*.$NCSUFFIX $myball/mem$memtag/
         end
      else
         foreach type ( "gaas_ana.sfc.${nymds}_${hhs}z" "aana.eta.${nymds}_${hhs}z" "aker.eta.${nymds}_${hhs}z" \
                        "aod_a.sfc.${nymds}_${hhs}00z" "aod_d.sfc.${nymds}_${hhs}00z" "aod_k.sfc.${nymds}_${hhs}00z" \
                        "recalc_aod_a.sfc.${nymds}_${hhs}00z")
            /bin/mv mem$memtag/*.${type}.$NCSUFFIX $myball/mem$memtag/
         end
      endif
   end
endif
# store asm.eta fields only ...
set myball = $expid.atmens_easm.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb $offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      mkdir -p $myball/mem$memtag
      foreach type ( "asm.eta" )
         /bin/mv mem$memtag/*.${type}.*.$NCSUFFIX $myball/mem$memtag/
      end
   end
endif
# store prognostic fields only ...
set myball = $expid.atmens_eprg.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb $offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      mkdir -p $myball/mem$memtag
      foreach type ( "prog.eta" )
         /bin/mv mem$memtag/*.${type}.*.$NCSUFFIX $myball/mem$memtag/
      end
   end
endif
# store vortex tracker results only ...
set myball = $expid.atmens_evtk.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb $offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   /bin/mv $expid.tcvitals*txt $myball/
   if ( $status ) then # file does not exist
      echo " \e[0;31m ${MYNAME}: No tcvitals found, tarball $myball empty ... \e[0m"
   else
      @ ic = 0
      while ( $ic < $nmem )
         @ ic++
         set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
         mkdir -p $myball/mem$memtag
         foreach type ( "trak.FV5" )
            /bin/mv mem$memtag/*.${type}.*.txt $myball/mem$memtag/
         end
      end
    endif
endif
# store non-inflated analysis members only ...
set myball = $expid.atmens_eniana.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb -$offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      mkdir -p $myball/mem$memtag
      foreach type ( "niana.eta" )
         /bin/mv mem$memtag/*.${type}.*.$NCSUFFIX $myball/mem$memtag/
      end
   end
endif
# store analysis/increment members only ...
set myball = $expid.atmens_eana.${nymdb}_${hhb}z
set syndate = ( `tick $nymdb $nhmsb -$offset_sec` )
set nymds = `echo $syndate[1]`
set hhs   = `echo $syndate[2] | cut -c1-2`
set doall = 1
if ( $doall && (-d $myball) ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      mkdir -p $myball/mem$memtag
#     foreach type ( "ana.eta.${nymds}_${hhs}z" "inc.eta.${nymds}_${hhs}z" )
#        /bin/mv mem$memtag/*.${type}.$NCSUFFIX $myball/mem$memtag/
      foreach type ( "ana.eta" )
         /bin/mv mem$memtag/*.${type}.*.$NCSUFFIX $myball/mem$memtag/
      end
   end
endif
# store obs-imp on analysis ...
set myball = $expid.atmens_eoi0.${nymdb}_${hhb}z
if ( -d $myball ) then
   mkdir -p $myball
  /bin/mv obsimp0hr $myball
endif
# store diagnostic files ...
set myball = $expid.atmens_edia.${nymdb}_${hhb}z
if ( -d $myball ) then
   mkdir -p $myball
  /bin/mv ensdiag $myball
endif
# store mostly every thing (rsts, etc) ...
set myball = $expid.atmens_erst.${nymdb}_${hhb}z
if ( -d $myball ) then
   @ ic = 0
   while ( $ic < $nmem )
      @ ic++
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      /bin/mv mem$memtag  $myball
   end
endif 
# store stats ...
set myball = $expid.atmens_stat.${nymdb}_${hhb}z
if ( -d $myball ) then
  /bin/mv ensmean $myball
  /bin/mv ensrms  $myball
endif
# store forecast stats ...
set myball = $expid.atmens_fstat.${nymdb}_${hhb}z
if ( -d $myball ) then
  /bin/mv ensmeanf $myball
  /bin/mv ensrmsf  $myball
endif
# store obs-impact-related output ...
set myball = $expid.atmens_eoimp.${nymdb}_${hhb}z
if ( -d $myball ) then
  /bin/mv fcsterr     $myball
endif

# tar and compress each subdir
foreach ball ( $myarch_list )
   set myball = $expid.atmens_${ball}.${nymdb}_${hhb}z
   tar -cvf $myball.tar $myball &
end
wait

# check that compression went well
foreach ball ( $myarch_list )
   set myball = $expid.atmens_${ball}.${nymdb}_${hhb}z.tar
   if (! -e $myball ) then
     echo " \e[0;31m ${MYNAME}: Failed to find tarball $myball for archiving, aborting ... \e[0m"
     exit(1)
  endif
end

# reposition tarball before submitting archive job
# ------------------------------------------------
#@ pesto_status = 0
#if ( -e $ATMENSETC/atmens_storage.arc ) then
#     setenv PESTOROOT `dirname $ATMENS4ARCH/$dir4arc`
#     pesto -v -d ./ -arc $ATMENSETC/atmens_storage.arc
#     @ pesto_status = $status
#endif
#if ( $pesto_status ) then
#   echo " ${MYNAME}: Failed to run pesto, aborting ..."
#   exit(1)
#endif
# note: I want pesto to do this but too painful

# Now move all to where archiving job will look for files to store
# ----------------------------------------------------------------
set dir4arch = $ATMENS4ARCH/siloens/$yyyymmddhh/atmens/Y$yyyyb/M$mmb  # this dir is free from .no_archiving restriction
mkdir -p $dir4arch
foreach ball ( $myarch_list )
   set myball = $expid.atmens_${ball}.${nymdb}_${hhb}z.tar
   /bin/mv $myball  $dir4arch
end
/bin/mv $expid.atm_enkf.log.*.txt      $dir4arch
/bin/mv $expid.atmens_zeit.*.txt       $dir4arch
/bin/mv $expid.rndperts.dates.*z.txt   $dir4arch
/bin/mv $expid.add_infl.*z.txt         $dir4arch
/bin/mv $expid.atmens_olog.*.tar.gz    $dir4arch
/bin/mv $expid.atmens_osens.*.bin      $dir4arch
/bin/mv $expid.atmens_osens.*.ods      $dir4arch
/bin/mv $expid.eimp*ods                $dir4arch
if( -e $expid.atmens_eana_arec.${nymdb}_${hhb}z.tar ) then
  /bin/mv $expid.atmens_eana_arec*tar  $dir4arch
endif
if( -e $expid.atmens_eana_brec.${nymdb}_${hhb}z.tar ) then
  /bin/mv $expid.atmens_eana_brec*tar  $dir4arch
endif

set adate =

if ( $ENSARCH_KEEP ) then
   set xcmd = "echo"
else
   set xcmd = "/bin/rm -r"
endif

# Finally generate archiving job script and submit it
# ---------------------------------------------------
  setenv JOBGEN_NCPUS          1
  setenv JOBGEN_NCPUS_PER_NODE 1
  jobgen.pl \
       -expid $expid         \
       -q datamove           \
       -xc "$xcmd $ATMENS4ARCH/$dir4arc" \
       arch_atmens           \
       $GID                  \
       $ENSARCH_WALLCLOCK    \
       "fvarchive $adate -r $ARCHLOC -H $FVHOME -A $ATMENS4ARCH/siloens/$yyyymmddhh -a $arcfile " \
       $ATMENS4ARCH/siloens/$yyyymmddhh \
       $MYNAME               \
       $ATMENS4ARCH/siloens/$yyyymmddhh/.DONE_${MYNAME}_${workid}.$yyyymmddhh \
       "Archive AtmEns Failed"

       if ( -e arch_atmens.j ) then
          qsub arch_atmens.j
       else
          echo " \e[0;31m ${MYNAME}: Failed to generate PBS jobs to archive Atmos Ensemble, Aborting ... \e[0m"
          exit(1)
       endif

# if make it here, it's all good
# ------------------------------
echo " \e[0;32m ${MYNAME}: Complete \e[0m"
exit(0)
