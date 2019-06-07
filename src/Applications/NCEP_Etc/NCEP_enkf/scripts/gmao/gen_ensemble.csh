#!/bin/csh

# gen_ensemble - generate ensemble from existing experiment
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  16Nov2011  Todling   Move stats component into own script
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME gen_ensemble.csh

# need usage here
# ---------------
if ( $#argv < 9 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - articificially generates ensemble from an existing experiment "
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  EXPLOC expid inymd inhms newid enymd enhms nmem ensdir"
   echo " " 
   echo " where" 
   echo "   EXPLOC - archive location where original experiment resides,"
   echo "             e.g., /archive/u/$user/"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   inymd  -  date of files in archive to serve as samples, as in YYYYMMDD"
   echo "              i.e., ensemble will be generated from file centered around this date"
   echo "   inhms  -  time of files in archive, as in HHMMSS"
   echo "   newid  -  new experiment id"
   echo "   enymd  -  actual date user wants ensemble at, as in YYYYMMDD"
   echo "   enhms  -  actual time user wants ensemble at, as in HHMMSS"
   echo "   nmem   -  number of members to be created"
   echo "   ensdir -  location to place generated ensemble"
   echo " " 
   echo " DESCRIPTION"
   echo "    This procedure creates an ensemble of background by taking backgrounds" 
   echo "  from different dates of an existing experiment and pretending them to be"
   echo "  all valid at a given date/time."
   echo "    The procedure also creates an ensemble of GCM restarts by copying "
   echo "  restarts from an existing experiment, at a given time, and re-time-tagging"
   echo "  them to pretend they all fall on the desired initial date/time of the "
   echo "  experiment to be performed."
   echo " " 
   echo " Example of valid command line:"
   echo " $MYNAME $NOBACKUP/expid/recycle b541iau 20091019 000000 \\"
   echo "         z000_b72 20090801 000000 10 /discover/nobackup/$user/Gen_Ens"
   echo " " 
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ASYNBKG   - frequency of background (minutes) "
   echo "    FVROOT    - location of DAS build             "
   echo "    TIMEINC   - frequency of analysis (minutes)   "
   echo " " 
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " " 
   echo "    ATMENS_DOSTATS    - allows bypass calculation of statistics"
   echo "                        (default: 1 - do it)"
   echo "    SIMULATE_ENSEMBLE - dry test: exec without actual copying"
   echo " " 
   echo " REMARKS "
   echo " " 
   echo "  1. After multiple versions of the ensemble initialization "
   echo "     procedure we have now settle on something rather simpler "
   echo "     than what performed here. The present possible modes of "
   echo "     initializing the ensemble are more effective, and less  "
   echo "     prone to spind-up/down issues."
   echo "  2. Therefore, this procedure is largely obsolete, though its"
   echo "     sub-components are stil useful."
   echo " "
   echo " SEE ALSO "
   echo "   gen_ensbkg.csh - generates ensemble of backgrounds"
   echo "   gen_ensrst.csh - generates ensemble of restarts"
   echo " " 
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ASYNBKG)   )  setenv FAILED   1
if ( !($?TIMEINC)   )  setenv FAILED   1
if ( !($?FVROOT)    )  setenv FAILED   1
if ( !($?NCSUFFIX)  )  setenv NCSUFFIX nc4

# The following env variable allows generation of a fake ensemble
# ---------------------------------------------------------------
if ( !($?SIMULATE_ENSEMBLE) ) setenv SIMULATE_ENSEMBLE 0
if ( !($?ATMENS_DOSTATS)    ) setenv ATMENS_DOSTATS    1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

if ( ($?FVHOME) ) then
  set path = ( . $FVHOME/run $FVROOT/bin $path )
else
  set path = ( . $FVROOT/bin $path )
endif

if ( $SIMULATE_ENSEMBLE ) then
  set dry_run = echo
else
  set dry_run =
endif

# command line parameters
# -----------------------
set EXPLOC = $1  # location of initial background files
set expid  = $2  # experiment name of originator
set inymd  = $3  # analysis date YYYYMMDD
set inhms  = $4  # analysis time HHMMSS
set newid  = $5  # experiment name of originator
set enymd  = $6  # ensemble date YYYYMMDD
set enhms  = $7  # ensemble time HHMMSS
set nmem   = $8  # will create 2*nmem+1 members
set ensdir = $9  # root location for members and mean

@ bkgfreq_hrs =  $ASYNBKG / 60
@ bkgfreq_sec =  $ASYNBKG * 60
@ anafreq_sec =  $TIMEINC * 60

set usrbeg  = `tick $inymd $inhms -$bkgfreq_sec`
set ensbeg  = `tick $enymd $enhms -$bkgfreq_sec`

# generate 1st member
# -------------------
mkdir -p $ensdir/atmens
set ensloc = $ensdir/atmens
cd $ensloc/
touch .no_archiving

# In case additive inflation, prepare the perturbations
# -----------------------------------------------------  
  if ( -e $ATMENSETC/nmcperts.rc ) then
      setup_perts.csh ${EXPID} $anymd $anhms ensmean |& tee -a $FVWORK/setup_perts.log
  endif

mkdir mem001
@ ic = 1
foreach type ( eta )
   set it = 0
   set curdate = ( $usrbeg[1] $usrbeg[2] )
   set newdate = ( $ensbeg[1] $ensbeg[2] )
   while ( $it < $bkgfreq_hrs )
      set cyy = `echo $curdate[1] | cut -c1-4`
      set cmm = `echo $curdate[1] | cut -c5-6`
      set chh = `echo $curdate[2] | cut -c1-2`
      set nhh = `echo $newdate[2] | cut -c1-2`

                                       if(! -e mem001/$newid.bkg.$type.${newdate[1]}_${nhh}z.$NCSUFFIX )\
      $dry_run /bin/cp $EXPLOC/$expid/ana/Y$cyy/M$cmm/$expid.bkg.$type.${curdate[1]}_${chh}z.$NCSUFFIX  \
                                               mem001/$newid.bkg.$type.${newdate[1]}_${nhh}z.$NCSUFFIX
      $dry_run reset_time.x mem001/$newid.bkg.$type.${newdate[1]}_${nhh}z.$NCSUFFIX $newdate[1] $newdate[2] -9
      if ( "$dry_run" == "echo" ) \
                      touch mem001/$newid.bkg.$type.${newdate[1]}_${nhh}z.$NCSUFFIX

      set curdate = `tick $curdate[1] $curdate[2]  $bkgfreq_sec`
      set newdate = `tick $newdate[1] $newdate[2]  $bkgfreq_sec`
      @ it++
   end # it
end  # type
wait

# generate each member
# --------------------
   foreach side ( "forward" "backward" )
      set mvnymd  = $usrbeg[1]
      set mvnhms  = $usrbeg[2]
      if( "$side" == "forward" ) then
         set dt     = $anafreq_sec
         @ nmemall  = $nmem / 2
      else
         set dt     = -$anafreq_sec
         @ nmemall  = $nmem - 1
      endif
      echo " "
      while ($ic <= $nmemall)
         set movedate = `tick $mvnymd $mvnhms  $dt`
             set mvnymd = $movedate[1]
             set mvnhms = $movedate[2]

         @ ic = $ic + 1
         set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
         mkdir -p $ensloc/mem$memtag
   
         # Retrieve files from EXPLOC and place them in ensloc
         # ---------------------------------------------------
         foreach type ( eta )
            set it = 0
            set curdate = ( $mvnymd    $mvnhms )
            set newdate = ( $ensbeg[1] $ensbeg[2]  )
            while ( $it < $bkgfreq_hrs )
               set cyy = `echo $curdate[1] | cut -c1-4`
               set cmm = `echo $curdate[1] | cut -c5-6`
               set chh = `echo $curdate[2] | cut -c1-2`
               set nhh = `echo $newdate[2] | cut -c1-2`

                                            if(! -e mem$memtag/$newid.bkg.$type.${newdate[1]}_${nhh}z.$NCSUFFIX )\
               $dry_run /bin/cp $EXPLOC/$expid/ana/Y$cyy/M$cmm/$expid.bkg.$type.${curdate[1]}_${chh}z.$NCSUFFIX  \
                                                    mem$memtag/$newid.bkg.$type.${newdate[1]}_${nhh}z.$NCSUFFIX
               $dry_run reset_time.x mem$memtag/$newid.bkg.$type.${newdate[1]}_${nhh}z.$NCSUFFIX $newdate[1] $newdate[2] -9
               if ( "$dry_run" == "echo" ) \
                               touch mem$memtag/$newid.bkg.$type.${newdate[1]}_${nhh}z.$NCSUFFIX
         
               set curdate = `tick $curdate[1] $curdate[2]  $bkgfreq_sec`
               set newdate = `tick $newdate[1] $newdate[2]  $bkgfreq_sec`
               @ it++
            end # it
         end  # type
         wait
      end # while ic
   end # forward/backward


# Now calculate statistics for ensemble (mean and rms)
# ----------------------------------------------------
if ( $SIMULATE_ENSEMBLE || (! $ATMENS_DOSTATS) ) then
  echo " ${MYNAME}: by request, no statistics calculated"
else
  cd $ensloc/mem001
  set bkgetalst = `ls $expid.*bkg*eta*.$NCSUFFIX`
  cd -
  foreach bkgeta ( $bkgetalst )
     set this_nymd = `echo $bkgeta | cut -d. -f3 | cut -c1-8`
     set this_hh   = `echo $bkgeta | cut -d. -f3 | cut -c10-11`
     set this_nhms = ${this_hh}0000
     atmens_stats.csh $nmem bkg.eta $ensloc $this_nymd $this_nhms
  end
endif
exit(0)
