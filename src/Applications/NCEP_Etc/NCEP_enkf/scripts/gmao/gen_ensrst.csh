#!/bin/csh 

# gen_ensemble - generate ensemble from existing experiment
#
# !REVISION HISTORY:
#
#  14Nov2011  Todling   Create ensemble of restarts
#  23Nov2011  Todling   Add recentering of ensemble of bkgs
#                       (only recenter upper-air eta files for now)
#  16Sep2012  Todling   No longer handle's bkg's by default
#  20Oct2012  Todling   Update API of stats script
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME gen_ensrst.csh

if ( $#argv < 6 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - articificially generates ensemble of GCM restarts" 
   echo " "
   echo " SYNOPSIS "
   echo "  $MYNAME  nymd nhms oexpid nexpid rstloc ensloc "
   echo " "
   echo " where"
   echo "   nymd    -  initial date of experiment, as in YYYYMMDD"
   echo "   nhms    -  initial time of experiment, as in HHMMSS"
   echo "   oexpid  -  name of experiment for original set of restarts"
   echo "   nexpid  -  name of present (new) hyrbrid experiment"
   echo "   rstloc  -  location of original restarts"
   echo "   ensloc  -  location of initial ensemble for present experiment"
   echo " "
   echo " DESCRIPTION "
   echo " "
   echo "  Articificially generates ensemble of GCM (restarts) initial" 
   echo "  conditions. This is done by simply associating the ens of restarts "
   echo "  to a pre-existing set of restarts. User must be mindful of two situations:"
   echo "     (i) full resolution hybrid: initial rsts can be associated to "
   echo "         those under recycle"
   echo "    (ii) dual resolution hybrid: an extra set of restarts at reduced "
   echo "         resolution must be made available for the same initial time "
   echo "         of the experiment (that is, as the restarts under recycle)"
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME 20090731 210000 z000_b72 u000_c72 \\"
   echo "          /discover/nobackup/$user/z000_b72/recycle \\"
   echo "          /discover/nobackup/$user/u000_c72/atmens "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    FVROOT       - location of DAS build             "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    NCSUFFIX     - suffix of bkg files (default: nc4)"
   echo "    RECENTERBKG  - force recentering of bkg.eta files (default: 0; not)"
   echo " "
   echo " SEE ALSO"
   echo "   atmens_recenter.csh - control recentering of ensemble members"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

set nymd   = $1   # date of original rsts
set nhms   = $2   # time of original rsts
set oexpid = $3   # name of experiment w/ original rsts
set nexpid = $4   # name of new experiment
set rstloc = $5   # location of rst from original experiment
set ensloc = $6   # location of atmens directory for new exp

set hh = `echo $nhms | cut -c1-2`

setenv FAILED 0
if ( !($?FVROOT)    )  setenv FAILED   1
if ( !($?NCSUFFIX)  )  setenv NCSUFFIX nc4

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit(1)
endif

if ( !($?RECENTERBKG) ) setenv RECENTERBKG 0

set path = ( . $FVROOT/bin $path )

if ( -d $ensloc/rst ) /bin/rm -r $ensloc/rst
mkdir $ensloc/rst
cd $ensloc/rst
echo $rstloc/$oexpid*${nymd}_${hh}z.bin
/bin/cp $rstloc/$oexpid*.${nymd}_${hh}z.bin .

# if so, rename restarts ...
if ( "$oexpid" != "$nexpid" ) then
   foreach fn ( `/bin/ls $oexpid.*bin`)
     set sfx = `echo $fn | cut -d. -f2-`
     /bin/mv $fn $nexpid.$sfx
   end
endif

# now copy rsts to each of the members ...
# (must copy; no links)
cd $ensloc
set members = `/bin/ls -d mem* | wc`
set nmem = $members[1]
@ n = 0
while ( $n < $nmem )
   @ n = $n + 1
   set nnn = `echo $n | awk '{printf "%03d", $1}'` 
   cd mem$nnn
   /bin/cp ../rst/*bin .
   cd ../
end

# NOTE: Once comfortable, remove the stuff below ...
# --------------------------------------------------
if ( $RECENTERBKG ) then

  # finally, recenter ensemble of backgrounds around reasonable mean
  # for that (reasonable mean), use the bkg_rst files as central mean
  # upper-handling first ...
  cd $ensloc/rst
  foreach type ( eta )
    if( "$rstloc" != "$ensloc/rst" ) /bin/cp $rstloc/$oexpid.bkg*${type}*.$NCSUFFIX .
    foreach fn ( `/bin/ls $oexpid.bkg*${type}*.*.$NCSUFFIX `)
       set timtag    = `echo $fn | cut -d. -f3`
       set this_nymd = `echo $timtag | cut -c1-8`
       set this_hh   = `echo $timtag | cut -c10-11`
       set this_nhms = ${this_hh}0000
       set suffix = `echo $fn | cut -d. -f3-`
       ln -sf $fn $nexpid.bkg.${type}.$suffix
       cd ../
       atmens_recenter.csh $nexpid $this_nymd $this_nhms bkg.${type} bkg.${type} $ensloc $ensloc/rst NONE
       cd rst
       /bin/rm $nexpid.bkg.${type}.$suffix

       # Now re-calculate statistics for ensemble (mean and rms)
       # -------------------------------------------------------
       atmens_stats.csh $nmem bkg.eta $ensloc $this_nymd $this_nhms
    end
  end

  # Now for the surface, simply use the same bkg_rst files for each member
  # obviously: no need to recenter; no need to calc stats (trivial)
  # ----------------------------------------------------------------------
  cd $ensloc/rst
  if ( ! -e $ensloc/ensmean ) mkdir $ensloc/ensmean
  if ( ! -e $ensloc/ensrms  ) mkdir $ensloc/ensrms
  foreach type ( sfc )
    if( "$rstloc" != "$ensloc/rst" ) /bin/cp $rstloc/$oexpid.bkg*${type}*.$NCSUFFIX .
    foreach fn ( `/bin/ls $oexpid.bkg*${type}*.*.$NCSUFFIX `)
       set timtag    = `echo $fn | cut -d. -f3`
       set this_nymd = `echo $timtag | cut -c1-8`
       set this_hh   = `echo $timtag | cut -c10-11`
       set this_nhms = ${this_hh}0000
       set suffix = `echo $fn | cut -d. -f3-`
       @ n = 0
       while ( $n < $nmem )
          @ n = $n + 1
          set memtag = `echo $n | awk '{printf "%03d", $1}'` 
          /bin/cp $fn $ensloc/mem$memtag/$nexpid.bkg.${type}.$suffix
       end
       /bin/cp $fn $ensloc/ensmean/$nexpid.bkg.${type}.$suffix
    end
  end

endif # <RECENTERBKG>
