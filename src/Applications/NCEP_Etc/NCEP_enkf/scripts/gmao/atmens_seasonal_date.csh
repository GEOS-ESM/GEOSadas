#!/bin/csh

# atmens_seasonal_dates - generate random dates from datebase that 
#                         fall within season of present date
#
# !REMARKS:
#   1) this is an awful little script, but it works!
#
# !REVISION HISTORY:
#
#  04Apr2013  Todling   Initial script
#  12Jun2014  El Akkraoui  Bug fix for handling edge of database
#  30Apr2020  Todling   Adjustment to fend against possibility of databased not having leap date
#
#------------------------------------------------------------------

if ( !($?ATMENS_VERBOSE) ) then
    setenv ATMENS_VERBOSE 0
else
    if ( $ATMENS_VERBOSE )  set echo
endif

setenv MYNAME atmens_seasonal_dates.csh

if ( $#argv < 3 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " $MYNAME  - generate random dates falling within season "
   echo "            of current date."
   echo " "
   echo " SYNOPSIS"
   echo " "
   echo "  $MYNAME  nymd nhms nmem "
   echo " "
   echo "  where"
   echo "    nymd   -  initial date of ensemble forecast, as in YYYYMMDD "
   echo "    nhms   -  initial time of ensemble forecast, as HHMMSS"
   echo "    nmem   -  number of members (random dates to generate)"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "   This procedure generates random random dates from dates within"
   echo "   database of NMC-perturbations falling within the season of the"
   echo "   date specified at command line."
   echo " "
   echo " Example of valid command line:"
   echo "  $MYNAME 20110325 12000 32"
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENSETC     - location of EnKF resource files   "
   echo "    FVROOT        - location of DAS build             "
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo "    ATMENS_VERBOSE - turn on shell verbose (default: 0)"
   echo "    VERBOSE        - minimal echo of results (default: 1)"
   echo " "
   echo " SEE ALSO"
   echo "  randates.py           - generate random dates within given" 
   echo "                          range of dates (by J. Whitaker)"
   echo "  ut_seasonal_dates.csh - unity tested for the present script"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 08Apr2013      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?ATMENSETC)     ) setenv FAILED  1
if ( !($?VERBOSE)       ) setenv VERBOSE 1

if ( $FAILED ) then
  env
  echo " ${MYNAME}: not all required env vars defined"
  exit 1
endif

set path = ( . $FVROOT/bin $path ) 

# Odd variables that offend MPT-built execs
  unsetenv PMI_RANK
  unsetenv PMI_FD
  unsetenv PMI_JOBID
  unsetenv PMI_SIZE

# +/- window days (usually 45-days)
set season = `echorc.x -rc $ATMENSETC/nmcperts.rc nmc_perts_date_window`
@ oneday_sec = 24 * 3600 # one day
@ season_sec = $season * 24 * 3600 # forty-five days

# check this is really active ...
if ( $season_sec > 0 ) then
   setenv NOT_SEASONAL 0
else
   setenv NOT_SEASONAL 1
endif

set nymd = $1
set nhms = $2
set nmem = $3
if ( $VERBOSE ) then
   echo "current date: $nymd $nhms"
endif

# error check
# -----------
@ twoseason = 2 * $season
@ twoseason_sec = $twoseason * 24 * 3600
if ( $nmem > $twoseason ) then
   setenv NOT_SEASONAL 1
   echo "${MYNAME}: Warning, number of members too large for seasonal perts selection"
   echo "${MYNAME}: perturbations selected at random with entire database, instead"
   echo "${MYNAME}: nmem = $nmem  2*season = $twoseason "
endif
set db_datemin = `echorc.x -rc $ATMENSETC/nmcperts.rc nmc_perts_date_beg`
set db_datemax = `echorc.x -rc $ATMENSETC/nmcperts.rc nmc_perts_date_end`
set datemin = $db_datemin
set datemax = $db_datemax
if ( $VERBOSE ) then
   echo "actual datebase beg date: $datemin"
   echo "actual datebase end date: $datemax"
endif

# Generate random dates within desired range
# ------------------------------------------
if ( $NOT_SEASONAL ) then
    randates.py $datemin $datemax $nmem >&! dates.dat
    exit(0)
endif

# pretend databased is smaller to the extent defined by season
set lnymd = `echo $db_datemin | cut -c1-8`
set unymd = `echo $db_datemax | cut -c1-8`
set ldate = `tick $lnymd 0  $season_sec`
set udate = `tick $unymd 0 -$season_sec`
set lnymdhh = `echo $ldate[1]`00
set unymdhh = `echo $udate[1]`00
if ( $VERBOSE ) then
   echo "pretend datebase begins: $lnymdhh"
   echo "present datebase   ends: $unymdhh"
endif
set lmmdd = `echo $lnymd | cut -c1-6`
set ummdd = `echo $unymd | cut -c1-6`

# Determine present month/date within database
set byyyy_db = `echo $datemin | cut -c1-4`
set eyyyy_db = `echo $datemax | cut -c1-4`

set mmdd = `echo $nymd | cut -c5-8`
set pb_nymdhh = ${byyyy_db}${mmdd}00

if ( $pb_nymdhh >= $lnymdhh ) then
   set new_nymdhh = $pb_nymdhh
else
   set new_nymdhh = ${eyyyy_db}${mmdd}00
endif
if (! $?new_nymdhh ) then
   echo "${MYNAME}: failed to determine date, aborting ..."
   exit(1)
endif
set new_nymd = `echo $new_nymdhh | cut -c1-8`
set new_nhms = `echo $new_nymdhh | cut -c9-10`0000
# check for possible lack of leap date in database
set leapmmdd = `echo $new_nymd | cut -c5-8`
if ( $leapmmdd == "0229" ) then
   set mydir     = `echorc.x -rc $ATMENSETC/nmcperts.rc nmc_perts_location`
   set fname0229 = `echorc.x -rc $ATMENSETC/nmcperts.rc -template dummy $new_nymd $new_nhms nmc_perts_fnametmpl`
   if ( ! -e $mydir/$fname0229 ) then
      if ($VERBOSE) then
         echo "data base missing: $mydir/$fname0229 ..."
      endif
      set lyyyy = `echo $lnymd | cut -c1-4`
      # check if database covers leap year
      if ( $lmmdd < "0227" ) then
        # year of feb 29 is near beg of database
      else
        # year of feb 29 is near end of database
        @ lyyyy = $lyyyy + 1
      endif
      set new_nymd = ${lyyyy}0228
      if ( $VERBOSE ) then
        echo "adjusted date due to missing leap date: $new_nymd"
      endif
   endif
endif
if ( $VERBOSE ) then
   echo "present date reset to similar date within datebase: $new_nymdhh " 
endif

# Now find "45-day" window around presently reset date
# ----------------------------------------------------
set dateini = `tick $new_nymd $new_nhms -$season_sec`
set datefnl = `tick $new_nymd $new_nhms  $season_sec`

# Readjust date based on boundary dates available in datebase
# -----------------------------------------------------------
set reset_datemin = $dateini[1]00
set reset_datemax = $datefnl[1]00
@ ndaysa = $reset_datemin - $db_datemin
@ ndaysb = $reset_datemax - $db_datemax
if ( $ndaysa < 0 ) then # following should never happen
    echo "${MYNAME}: near egde of database, choosing the first $twoseason days past present date"
    set dateini = ( $new_nymd $new_nhms )
    set datefnl = ` tick $new_nymd $new_nhms  $twoseason_sec`
    set reset_datemin = $dateini[1]00
    set reset_datemax = $datefnl[1]00
endif
if ( $ndaysb > 0 ) then
    echo "${MYNAME}: near egde of database, choosing the last $twoseason days prior to present date"
    set datefnl = ( $new_nymd $new_nhms )
    set dateini = ` tick $new_nymd $new_nhms -$twoseason_sec`
    set reset_datemin = $dateini[1]00
    set reset_datemax = $datefnl[1]00
endif
if ( $VERBOSE ) then
   echo "will search for perts starting: $dateini"
   echo "will search for perts   ending: $datefnl"
endif

# Generate random dates within desired range
# ------------------------------------------
randates.py $reset_datemin $reset_datemax $nmem >&! dates.dat

# Check dates generated to make sure they fall within reason ...
# --------------------------------------------------------------
set alldates = `cat dates.dat`
@ n = 0
while ( $n < $nmem )
   @ n++
   if ( $alldates[$n] < $datemin || $alldates[$n] > $datemax ) then
       echo "${MYNAME}: error, random date outside range of data:"
       echo "${MYNAME}: datebase beg: $datemin"
       echo "${MYNAME}: datebase end: $datemax"
       echo "${MYNAME}: out-of-range date: $alldates[$n]"
       echo "${MYNAME}: Aborting."
       exit (1)
   endif
end

# for now, if made it here, exit successfully ...
# -----------------------------------------------
exit(0)
