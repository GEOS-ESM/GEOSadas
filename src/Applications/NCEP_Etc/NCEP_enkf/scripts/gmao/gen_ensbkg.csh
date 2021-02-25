#!/bin/csh

# gen_ensbkg - generate ensemble from central backgrounds
#
# !REVISION HISTORY:
#
#    Oct2011  Todling   Initial script
#  16Nov2011  Todling   Move stats component into own script
#  16Sep2012  Todling   Created from gen_ensemble: initial members now 
#                       generated from central BKGs by simply adding 
#                       NMC-like perturbations to them
#  20Oct2012  Todling   Update API of stats script
#  12Mar2014  Todling   Update interface to main re-center program
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME gen_ensbkg.csh

# need usage here
# ---------------
if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME"
   echo " "
   echo "  $MYNAME  - articificially generates ensemble backgrounds   "
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  BKGLOC ENSLOC expid nmem "
   echo " " 
   echo " where" 
   echo "   BKGLOC - archive location where original experiment resides,"
   echo "             e.g., /archive/u/$user/"
   echo "   ENSLOC -  location to place generated ensemble"
   echo "   expid  -  usual experiment name, e.g., b541iau"
   echo "   nmem   -  number of members to be created"
   echo " " 
   echo " DESCRIPTION " 
   echo " " 
   echo "  Articificially generates ensemble backgrounds   "
   echo "  by adding NMC-like perturbations to the initial "
   echo "  central backgrounds"
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME $NOBACKUP/myexp/recycle \\ "
   echo "          $NOBACKUP/myexp/atmens 10 "
   echo " " 
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC - location of ensemble resource files"
   echo "    ASYNBKG   - frequency of background files (minutes)"
   echo "    FVHOME    - location of experiment            "
   echo "    FVROOT    - location of DAS build             "
   echo " " 
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " " 
   echo "    ATMENS_DOSTATS    - allows bypass calculation of statistics"
   echo "                        (default: 1 - do it)"
   echo "    ADDINF_FACTOR      - inflation factor (such as 0.25)"
   echo "    CENTRAL_BLEND      - 0 or 1=to blend members with central (def: 1)"
   echo " " 
   echo " REMAKRS"
   echo "  1. This procedure is largely obsolete"
   echo " "
   echo " SEE ALSO "
   echo "   gen_ensrst.csh"
   echo " " 
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENS_BATCHSUB) ) setenv FAILED 1
if ( !($?ATMENSETC) )  setenv FAILED   1
if ( !($?ASYNBKG)   )  setenv FAILED   1
if ( !($?FVHOME)    )  setenv FAILED   1
if ( !($?FVROOT)    )  setenv FAILED   1
if ( !($?NCSUFFIX)  )  setenv NCSUFFIX nc4

# The following env variable allows generation of a fake ensemble
# ---------------------------------------------------------------
if ( !($?ATMENS_DOSTATS) ) setenv ATMENS_DOSTATS    1

setenv JOBGEN_NCPUS_PER_NODE 2
if ( !($?ENSRECENTER_NCPUS) ) then
  setenv FAILED 1
else
  setenv JOBGEN_NCPUS $ENSRECENTER_NCPUS
endif

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set path = ( . $FVHOME/run $FVROOT/bin $path )

# command line parameters
# -----------------------
set BKGLOC = $1  # location of initial background files
set ENSLOC = $2  # location to place generated ensemble
set expid  = $3  # experiment name of originator
set nmem   = $4  # will create 2*nmem+1 members

if ( $CENTRAL_BLEND ) then
   set blend = "-damp"
else
   set blend = "NONE"
endif
if ( !($?ADDINF_FACTOR) ) then
   set addinf_factor = "NONE"
else
   set addinf_factor = $ADDINF_FACTOR
endif

# generate 1st member
# -------------------
mkdir -p $ENSLOC/atmens
set ensloc = $ENSLOC/atmens
cd $ensloc/
touch .no_archiving

# Make a list of background restarts
# ----------------------------------
cd $BKGLOC
set bkgetalst = `ls $expid.*bkg*eta*.$NCSUFFIX`
set bkgsfclst = `ls $expid.*bkg*sfc*.$NCSUFFIX`
cd $ensloc

# Create members as copies of central bkg's
# -----------------------------------------
if (! -e $ensloc/.DONE_COPY_BKGS ) then
   @ ic = 0
   while ($ic < $nmem)
     @ ic = $ic + 1
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
      if(! -d mem$memtag ) mkdir -p mem$memtag
      # sfc files first
      foreach bkgsfc ( $bkgsfclst )
         cd mem$memtag
         set nymd = `echo $bkgsfc | cut -d. -f3 | cut -c1-8`
         set hh   = `echo $bkgsfc | cut -d. -f3 | cut -c10-11`
         /bin/cp $BKGLOC/$bkgsfc $expid.bkg.sfc.${nymd}_${hh}z.$NCSUFFIX &
         if ( $status ) then
            echo "${MYNAME}: Failed to copy $BKGLOC/$bkgsfc, Aborting ..."
            exit(1)
         endif
         cd -
      end # <bkgsfc>
      wait
      # then eta files next
      foreach bkgeta ( $bkgetalst )
         cd mem$memtag
         set nymd = `echo $bkgeta | cut -d. -f3 | cut -c1-8`
         set hh   = `echo $bkgeta | cut -d. -f3 | cut -c10-11`
         /bin/cp $BKGLOC/$bkgeta $expid.bkg.eta.${nymd}_${hh}z.$NCSUFFIX &
         if ( $status ) then
            echo "${MYNAME}: Failed to copy $BKGLOC/$bkgeta, Aborting ..."
            exit(1)
         endif
         cd -
      end # <bkgeta>
      wait
      touch $ensloc/.DONE_COPY_BKGS
   end
endif

cd $ensloc
if (! -d ensmean ) mkdir ensmean

# For each bkg time ...
# ---------------------
ln -sf ensmean perts
foreach bkgeta ( $bkgetalst )
   set nymd = `echo $bkgeta | cut -d. -f3 | cut -c1-8`
   set hh   = `echo $bkgeta | cut -d. -f3 | cut -c10-11`
   set nhms = ${hh}0000
   set yyyymmddhh = ${nymd}${hh}

   # Also copy background restarts as ensmean
   # ----------------------------------------
   /bin/cp $BKGLOC/$bkgeta ensmean/$expid.bkg.eta.${nymd}_${hh}z.$NCSUFFIX

   # If random perturbations not available for this time yet, go after them
   # ----------------------------------------------------------------------
   if (! -e $ensloc/.DONE_MEM001_SETPERTS.$yyyymmddhh ) then 

      # Get perturbations from archive
      # ------------------------------
      if ( -e $ATMENSETC/nmcperts.rc ) then
         if( -e ensmean/dates.dat ) /bin/rm ensmean/dates.dat
         setperts.csh $expid $nmem $nymd $nhms $ASYNBKG ensmean |& tee -a $ensloc/setperts.log
         if ($status) then
            echo " ${MYNAME}: trouble in setperts, aborting"
            exit 1
         else
           touch $ensloc/.DONE_MEM001_SETPERTS.$yyyymmddhh
         endif
      endif

   endif # retrieve random perturbations

   # Now add a perturbation to each of the ensemble members
   # ------------------------------------------------------
   @ ic = 0
   while ($ic < $nmem)
      @ ic = $ic + 1
      set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
  
      if (! -e $ensloc/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh ) then

         if( $ENSPARALLEL ) then
   
            cd mem$memtag
            jobgen.pl \
                 -egress DYNRECENTER_EGRESS \
                 -q $RECENTER_QNAME         \
                 recenter_mem${memtag}      \
                 $GID                       \
                 $RECENTER_WALLCLOCK        \
                 "recenter.csh $FVROOT $expid $memtag $nymd $hh bkg.eta NONE DUMMY $blend $addinf_factor $ensloc/ensmean " \
                 $ensloc/mem$memtag    \
                 $MYNAME               \
                 $ensloc/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh \
                  "Failed to add NMC-like perturbation to the BKG"

            if ( -e recenter_mem${memtag}.j ) then
               $ATMENS_BATCHSUB recenter_mem${memtag}.j
            else
               echo " ${MYNAME}: Failed to generate PBS jobs for inflating BKGs, Aborting ... "
               touch $ensloc/.FAILED
               exit(1)
            endif
            cd -

         else  # <not parallel>

            cd mem$memtag
            recenter.csh $FVROOT $expid $memtag $nymd $hh bkg.eta NONE DUMMY $blend $addinf_factor $ensloc/ensmean
            if ( -e DYNRECENTER_EGRESS ) then
               /bin/rm DYNRECENTER_EGRESS
               touch $ensloc/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh \
            else
               echo " ${MYNAME}: dyn_recent failed to add perturbation to background member $memtag "
               exit(1)
            endif
            cd -

         endif # <parallel>

      endif # <not already done>

   end # <ic>

   # Monitor status of ongoing jobs
   # ------------------------------
   if ( $ENSPARALLEL ) then
     jobmonitor.csh $nmem $MYNAME $ensloc $yyyymmddhh
   endif

   # Clean up some
   # -------------
   /bin/rm ensmean/$expid.nmcpert.eta.${nymd}_${hh}z.*.$NCSUFFIX \

end # <bkgeta>

# Take care of sfc (place them in ensmean dir)
# ----------------
/bin/cp mem001/*bkg.sfc*$NCSUFFIX ensmean

# Check that forecasts have taken place successfully
# TBD: make this an independent script to be used elsewhere
# --------------------------------------------------
@ ic = 0
@ idone = 0
while ( $ic < $nmem )
     # Get positioned
     # --------------
     @ ic++
     set memtag  = `echo $ic |awk '{printf "%03d", $1}'`
     if ( -e $ensloc/.DONE_MEM${memtag}_${MYNAME}.$yyyymmddhh ) then
        @ idone = $idone + 1
     endif
end

if ( $idone == $nmem ) touch .DONE_${MYNAME}.$yyyymmddhh

# Clean up
# --------
/bin/rm -r spool updated_ens perts 
/bin/rm mem*/*.j
mkdir SAVEmean
/bin/mv ensmean/$expid.bkg.eta.*.$NCSUFFIX SAVEmean
/bin/mv ensmean/$expid.bkg.sfc.*.$NCSUFFIX SAVEmean
/bin/rm -r ensmean
/bin/mv SAVEmean ensmean

# Now calculate statistics for ensemble (mean and rms)
# ----------------------------------------------------
if ( $ATMENS_DOSTATS ) then
   foreach bkgeta ( $bkgetalst )
      set nymd = `echo $bkgeta | cut -d. -f3 | cut -c1-8`
      set hh   = `echo $bkgeta | cut -d. -f3 | cut -c10-11`
      set nhms = ${hh}0000
      atmens_stats.csh $nmem bkg.eta $ensloc $nymd $nhms
   end
else
   echo " ${MYNAME}: by request, no statistics calculated"
endif
#
exit(0)
