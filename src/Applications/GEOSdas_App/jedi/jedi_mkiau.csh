#!/bin/csh -x 

if ( !($?JEDI_VERBOSE) ) then
    setenv JEDI_VERBOSE 0
else
    if ( $JEDI_VERBOSE )  set echo
endif

setenv MYNAME jedi_mkiau.csh

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - run mkiau for JEDI analysis "
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  nymd nhms opt"
   echo " "
   echo "  where "
   echo "   nymd  -  analysis synoptic date"
   echo "   nhms  -  analysis synoptic time"
   echo "   opt   -  opt = central, for central ADAS or"
   echo "                  memNNN, for NNN-th member of JEDI ensemble"
   echo " " 
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Initial version: 18Oct2020    by: R. Todling"
   echo "     Last   modified: 08Oct2026    by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(1)
endif

if ( !($?EXPID)             ) setenv FAILED   1
if ( !($?FVROOT)            ) setenv FAILED   1
if ( !($?FVHOME)            ) setenv FAILED   1
if ( !($?FVWORK)            ) setenv FAILED   1
if ( !($?JEDI_MKIAU_MPIRUN) ) setenv FAILED   1
if ( !($?JEDI_MKIAU_CUBED)  ) setenv FAILED   1
if ( !($?JEDI_VAROFFSET)    ) setenv FAILED   1

setenv JEDI_CUBED_ANA 0  # this is old and not supported at this point (Dec 2022)

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

set nymd0 = $1
set nhms0 = $2
set opt   = $3
set hh0 = `echo $nhms0 | cut -c1-2`
set yyyymmddhh = $nymd0${hh0}

set anadate  = `tick $nymd0 $nhms0 $JEDI_VAROFFSET`
set nymda    = $anadate[1]
set nhmsa    = $anadate[2]

if ( -e $FVWORK/.DONE_${MYNAME}.$opt.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
else
   echo "${MYNAME}: Starting ... "
endif

#source $FVROOT/bin/g5_modules
#set path = ( . $FVROOT/bin $path )

# Defaults
if ( !($?JEDI_VARWINDOW))  setenv JEDI_VARWINDOW 21600
if ( !($?JEDI_VARANAFRQ))  setenv JEDI_VARANAFRQ  3600
setenv JEDIETC $FVHOME/run/jedi/Config

if ( $opt == "central" ) then
   setenv JEDIWORK $FVWORK/jedi.$nymda.${nhmsa}
   # link bkg.eta files from FVHOME/recycle - GEOSdas workflow deletes some of these from fvwork
   #                                          when GSI is running
   cd $JEDIWORK/bkg
   touch .no_archiving
   foreach fn ( `ls $FVHOME/recycle/*.bkg*eta_rst.*nc4` )
      set sfx = `basename $fn | cut -d. -f3-`  
      ln -sf $fn $EXPID.bkg.eta.$sfx
   end
else
   if ( -e $JEDIETC/jedi_eda.yaml ) then
     setenv SUITE eda_atmos
   endif
   if ( -e $JEDIETC/jedi_getkf.yaml ) then
     setenv SUITE localensembleda_2steps
   endif
   # mimic env configuration of central ADAS within each member directory
   setenv JEDIWORK $FVWORK/jedi.$nymda.${nhmsa}/${EXPID}-${SUITE}/run/${nymda}T${nhmsa}Z/geos_atmosphere/analysis/$opt
endif

# Create analysis from increment
cd $JEDIWORK/inc
set these_incs = (`ls *.jedi_inc1.eta.*`)
if ( $#these_incs == 1 ) then
   # In case of running FGAT, JEDI puts out a single increment - the solution - a the start
   # of the var window. What we need is the increment at synoptic time, since in 3D, the increment
   # is constant within the window, we rename/retime the increment as in the middle of the window.
   # This makes it fully consistent w/ what we would expect in a 3D setting, i.e., increment at the 
   # synoptic time.
   set ttag = `echo $these_incs[1] | cut -d. -f4`
   set nymd = `echo $ttag | cut -c1-8` 
   set   hh = `echo $ttag | cut -c10-11` 
   set nhms = ${hh}0000
   if ( $nymd != $nymda || $nhms != $nhmsa ) then
      /bin/mv $these_incs[1] $EXPID.jedi_inc1.eta.${nymda}_${nhmsa}z.nc4
      reset_time.x $EXPID.jedi_inc1.eta.${nymda}_${nhmsa}z.nc4 $nymda $nhmsa -9
   endif
   set these_incs = (`ls *.jedi_inc1.eta.*`)
endif
foreach fn ( $these_incs )
  set ttag = `echo $fn   | cut -d. -f4`
  set nymd = `echo $ttag | cut -c1-8` 
  set   hh = `echo $ttag | cut -c10-11` 
  set nhms = ${hh}0000
  dyn_jediupd.x $nymd $nhms -o $EXPID.jedi_inc.eta.${nymd}_${hh}00z.nc4 \
               $JEDIWORK/bkg/$EXPID.bkg.eta.${nymd}_${hh}00z.nc4 \
               $fn $JEDIWORK/ana/$EXPID.jedi_ana.eta.${nymd}_${hh}00z.nc4
  if ( ! -e $JEDIWORK/ana/$EXPID.jedi_ana.eta.${nymd}_${hh}00z.nc4 ) then
     echo "${MYNAME}: failed to create: $EXPID.jedi_ana.eta.${nymd}_${hh}00z.nc4"
     exit (1)
  endif
end
cd -

# gather list of analysis files
cd $JEDIWORK/ana
if ( $JEDI_MKIAU_CUBED ) then 
  set anatyp = "ana.ceta"
  set bkgtyp = "bkg_clcv_rst"
  set thisrc = mkiau_cubed.rc.tenv
else
  set anatyp = "ana.eta"
  set bkgtyp = "bkg.eta"
  set thisrc = mkiau.rc.tenv
endif
set analst = `ls $EXPID.jedi_${anatyp}.*`

cd $JEDIWORK

# Only calculate IAU increment when running Central ADAS
# When running ensmeble IAU increments are only calculated
# after recentering is done on the analyses.
if ( $opt == "central" ) then
  if ( ! -e IAU_EGRESS ) then
    foreach anafn ( $analst )
      set ttag = `echo $anafn  | cut -d. -f4`     
      set nymd = `echo $ttag | cut -c1-8`
      set hhmm = `echo $ttag | cut -c10-13`
      set nhms = ${hhmm}00
      set anafn = ana/$anafn
      set bkgfn = bkg/$EXPID.${bkgtyp}.${nymd}_${hhmm}z.nc4
      set iaufn = iau/$EXPID.agcm_import_rst.${nymd}_${hhmm}z.nc4
      echo " Input  Analysis   file: $anafn"
      echo " Input  Background file: $bkgfn"
      echo " Output IAU file: $iaufn"
      # prepare RC
      setenv ANADATE $nymd
      setenv ANATIME $nhms
      setenv ANAFNAME $anafn
      setenv BKGFNAME $bkgfn
      setenv AGCMIMPRST $iaufn
      vED -env $JEDIETC/$thisrc -o mkiau.rc
   
      set nx = `echorc.x -rc mkiau.rc "NX"`
      set ny = `echorc.x -rc mkiau.rc "NY"`
      @ ncpus = $nx * $ny 

      # generate IAU increment file
      touch input.nml
      $JEDI_MKIAU_MPIRUN -np $ncpus mkiau.x
   
      # clean up
      #rm -f input.nml mkiau.rc
    end
  endif

# if so, fake 4d
  if ( ! -d iau ) mkdir iau
  cd iau/
  if ( $JEDI_HYBRID ) then
    set lst = `ls *agcm_import_rst.*nc4`
    if ( $#lst == 1 ) then
       set nowdate = ( $nymd0 $nhms0 )
       set enddate = (`tick $nowdate[1] $nowdate[2] $JEDI_VARWINDOW`)
       set notdone = 1
       while ( $notdone )
         set nymd = $nowdate[1]; set nhms = $nowdate[2]; set hhmm = `echo $nhms | cut -c1-4`
         /bin/cp $lst[1] $EXPID.agcm_import_rst.${nymd}_${hhmm}z.nc4
         if ( "$nowdate" == "$enddate" ) then
            set notdone = 0
         else
            set nowdate = (`tick $nymd $nhms $JEDI_VARANAFRQ`)
         endif
       end
    endif
  else
    # this is one of those things ... the time in the file and the name-tag are inconsistent
    # this is a consequence of the change from bin to nc4 agcm_import files
    set hh = `echo $nhms | cut -c1-2`
    if ( -e $EXPID.agcm_import_rst.${nymd}_${hh}00z.nc4 ) then
       /bin/mv $EXPID.agcm_import_rst.${nymd}_${hh}00z.nc4 $EXPID.agcm_import_rst.${nymd0}_${hh0}00z.nc4
    else
       echo " ${MYNAME}: cannot find: $EXPID.agcm_import_rst.${nymd}_${hh}00z.nc4, aborting ..."
       exit (1) 
    endif
  endif


  # arquive IAU increments: in 3D agcm_import stored as single file, in 4D as tar ball
  # ----------------------
  cd $JEDIWORK/iau
  tar cvf $FVWORK/$EXPID.jedi_agcmrst.${nymd0}_${hh0}z.tar $EXPID.agcm_import_rst*
  cd -

endif # central

# arquive analysis
# ----------------
cd $JEDIWORK/ana
if ( $opt == "central" ) then
  tar cvf $FVWORK/$EXPID.jedi_ana.${nymd0}_${hh0}z.tar $EXPID.jedi_ana*.nc*
else
  if ( ! -d $FVWORK/updated_ens/$opt ) mkdir -p $FVWORK/updated_ens/$opt/
  foreach fn ( `ls $EXPID.jedi_ana.eta.*` )
    set sfx = `echo $fn | cut -d. -f3-`
    /bin/mv $fn $FVWORK/updated_ens/$opt/$EXPID.ana.$sfx
  end
endif
cd -

# arquive increments
# ------------------
if ( -d $JEDIWORK/inc ) then
  cd $JEDIWORK/inc
  if ( $opt == "central" ) then
     tar cvf $FVWORK/$EXPID.jedi_inc.${nymd0}_${hh0}z.tar $EXPID.jedi_*.nc*
  else
    if ( ! -d $FVWORK/updated_ens/$opt ) mkdir -p $FVWORK/updated_ens/$opt/
    foreach fn ( `ls $EXPID.jedi_inc.eta.*` )
      set sfx = `echo $fn | cut -d. -f3-`
      /bin/mv $fn $FVWORK/updated_ens/$opt/$EXPID.inc.$sfx
    end
  endif
  cd -
endif

# If here, likely successful
# --------------------------
touch $FVWORK/.DONE_${MYNAME}.$opt.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
