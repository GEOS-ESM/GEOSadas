#!/bin/csh -x 

if ( !($?JEDI_VERBOSE) ) then
    setenv JEDI_VERBOSE 0
else
    if ( $JEDI_VERBOSE )  set echo
endif

setenv MYNAME jedi_mkiau.csh

if ( $#argv < 2 ) then
   echo " ${MYNAME}: invalid arg list, aborting"
   exit (1)
endif

if ( !($?EXPID)             ) setenv FAILED   1
if ( !($?FVROOT)            ) setenv FAILED   1
if ( !($?FVHOME)            ) setenv FAILED   1
if ( !($?FVWORK)            ) setenv FAILED   1
if ( !($?JEDI_MKIAU_MPIRUN) ) setenv FAILED   1
if ( !($?JEDI_VAROFFSET)    ) setenv FAILED   1

setenv JEDI_CUBED_ANA 0  # this is old and not supported at this point (Dec 2022)

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit (1)
endif

set nymd0 = $1
set nhms0 = $2
set hh0 = `echo $nhms0 | cut -c1-2`
set yyyymmddhh = $nymd0${hh0}

set anadate  = `tick $nymd0 $nhms0 $JEDI_VAROFFSET`
set nymda    = $anadate[1]
set nhmsa    = $anadate[2]

if ( -e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

#source $FVROOT/bin/g5_modules
#set path = ( . $FVROOT/bin $path )

# Defaults
if ( !($?JEDI_VARWINDOW))  setenv JEDI_VARWINDOW 21600
if ( !($?JEDI_VARANAFRQ))  setenv JEDI_VARANAFRQ  3600

setenv JEDIETC $FVHOME/run/jedi/Config
setenv JEDIWORK $FVWORK/jedi.$nymda.${nhmsa}

# Create analysis from increment
cd $JEDIWORK/Data/inc
foreach fn ( `ls *.jedi_inc1.eta.*` )
  set ttag = `echo $fn   | cut -d. -f4`
  set nymd = `echo $ttag | cut -c1-8` 
  set   hh = `echo $ttag | cut -c10-11` 
  set nhms = ${hh}0000
  dyn_jediupd.x $nymd $nhms -o $EXPID.jedi_inc.eta.${nymd}_${hh}00z.nc4 \
               $FVWORK/$EXPID.bkg.eta.${nymd}_${hh}00z.nc4 $fn $JEDIWORK/Data/ana/$EXPID.jedi_ana.eta.${nymd}_${hh}00z.nc4
  if ( ! -e $JEDIWORK/Data/ana/$EXPID.jedi_ana.eta.${nymd}_${hh}00z.nc4 ) then
     echo "${MYNAME}: failed to create: $EXPID.jedi_ana.eta.${nymd}_${hh}00z.nc4"
     exit (1)
  endif
end
cd -

cd $JEDIWORK/Data/ana
set analst = `ls $EXPID.jedi_ana.eta.*`

cd $JEDIWORK

if ( ! -e IAU_EGRESS ) then
  foreach anafn ( $analst )
    set ttag = `echo $anafn  | cut -d. -f4`     
    set nymd = `echo $ttag | cut -c1-8`
    set hhmm = `echo $ttag | cut -c10-13`
    set nhms = ${hhmm}00
    set anafn = Data/ana/$anafn
    set bkgfn = $FVWORK/$EXPID.bkg.eta.${nymd}_${hhmm}z.nc4
    set iaufn = Data/iau/$EXPID.agcm_import_rst.${nymd}_${hhmm}z.nc4
    echo " Input  Analysis   file: $anafn"
    echo " Input  Background file: $bkgfn"
    echo " Output IAU file: $iaufn"
    # prepare RC
    setenv ANADATE $nymd
    setenv ANATIME $nhms
    setenv ANAFNAME $anafn
    setenv BKGFNAME $bkgfn
    setenv AGCMIMPRST $iaufn
    vED -env $JEDIETC/mkiau.rc.tenv -o mkiau.rc
 
    set nx = `echorc.x -rc mkiau.rc "NX"`
    set ny = `echorc.x -rc mkiau.rc "NY"`
    @ ncpus = $nx * $ny 

    # generate IAU increment file
    touch input.nml
    $JEDI_MKIAU_MPIRUN -np $ncpus mkiau.x

    # clean up
    rm -f input.nml mkiau.rc
  end
endif

# if so, fake 4d
if ( ! -d Data/iau ) mkdir Data/iau
cd Data/iau/
if ( $JEDI_HYBRID ) then
  set lst = `ls *agcm_import_rst.*nc4`
  if ( $#lst == 1 ) then
     set nymd = `echo $lst | cut -d. -f3 | cut -c1-8`
     set hhmm = `echo $lst | cut -d. -f3 | cut -c10-13`
     set nhms = ${hhmm}00
     set enddate = (`tick $nymd $nhms $JEDI_VARWINDOW`)
     set notdone = 1
     while ( $notdone )
       set thisdate = (`tick $nymd $nhms $JEDI_VARANAFRQ`)
       set nymd = $thisdate[1]; set nhms = $thisdate[2]; set hhmm = `echo $nhms | cut -c1-4`
       /bin/cp $lst[1] $EXPID.agcm_import_rst.${nymd}_${hhmm}z.nc4
       if ( "$thisdate" == "$enddate" ) set notdone = 0
     end
  endif
else
  # this is one of those things ... the time in the file and the name-tag are inconsistent
  # this is a consequence of the change from bin to nc4 agcm_import files
  set hh = `echo $nhms | cut -c1-2`
  if ( -e $EXPID.agcm_import_rst.${nymd}_${hh}00z.nc4 ) then
     /bin/mv $EXPID.agcm_import_rst.${nymd}_${hh}00z.nc4 $EXPID.agcm_import_rst.${nymd0}_${hh0}00z.nc4
  else
     echo "cannot find: $EXPID.agcm_import_rst.${nymd}_${hh}00z.nc4, aborting ..."
     exit (1) 
  endif
endif

# arquive IAU increments
# ----------------------
tar cvf $FVWORK/$EXPID.jedi_agcmrst.${nymd0}_${hh0}z.tar $EXPID.agcm_import_rst*
cd -

# arquive analysis
# ----------------
cd $JEDIWORK/Data/ana
tar cvf $FVWORK/$EXPID.jedi_ana.${nymd0}_${hh0}z.tar $EXPID.jedi_ana*.nc*
cd -

# arquive increments
# ------------------
if ( -d $JEDIWORK/Data/inc ) then
  cd $JEDIWORK/Data/inc
  tar cvf $FVWORK/$EXPID.jedi_inc.${nymd0}_${hh0}z.tar $EXPID.jedi_*.nc*
  cd -
endif

# If here, likely successful
# --------------------------
touch $FVWORK/.DONE_${MYNAME}.$yyyymmddhh
echo " ${MYNAME}: Complete "
exit(0)
