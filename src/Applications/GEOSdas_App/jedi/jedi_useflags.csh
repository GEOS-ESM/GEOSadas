#!/bin/csh
 
setenv MYNAME jedi_useflags.csh

# Set up for JEDI analysis

if ( $#argv < 4 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - Invoke GMAO obs-db to set JEDI obs at given cycle"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  nymd nhms inpobs outobs"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Initial version: 19May2026    by: R. Todling"
   echo "     Last   modified: 19May2026    by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif

setenv FAILED 0
if ( !($?EXPID)         )  setenv FAILED   1
if ( !($?FVHOME)        )  setenv FAILED   1
if ( !($?FVROOT)        )  setenv FAILED   1
if ( !($?FVWORK)        )  setenv FAILED   1

# Command line arguments
set nymda = $1   # initial date of var window
set nhmsa = $2   # initial time of var window
set inpobs = $3  # original (templated) obs yaml files 
set outobs = $4  # location to put edited obs yaml files
#
set yyyya    = `echo $nymda | cut -c1-4`
set mma      = `echo $nymda | cut -c5-6`
set dda      = `echo $nymda | cut -c7-8`
set hha      = `echo $nhmsa | cut -c1-2`
set yyyymmddhh = ${nymda}${hha}

if ( -e $FVWORK/.DONE_${MYNAME}.$yyyymmddhh ) then
   echo "${MYNAME}: all done"
   exit(0)
endif

if ( ($?FVHOME) ) then
   set path = ( . $FVHOME/run $FVROOT/bin $path )
else
   set path = ( . $FVROOT/bin $path )
endif
if ( ! -d $outobs ) mkdir -p $outobs

setenv MYWORK $FVWORK/$FVWORK/jedi.$nymda.$nhmsa/EditObs
if (   -d $MYWORK ) /bin/rm -r $MYWORK
mkdir -p $MYWORK
cd $MYWORK


/bin/cp -r $inpobs .
/bin/cp $FVHOME/run/gmao_global_convinfo.rc convinfo
/bin/cp $FVHOME/run/gmao_global_ozinfo.rc   ozinfo
/bin/cp $FVHOME/run/gmao_global_satinfo.rc  satinfo

#cat > this.rc << EOF
# &setup
#  nymd = $nymd
#  nhms = $nhms
#  notused_flag = -2
#  satinfo_tmpl = "./gmao_global_satinfo.rc"
#  satinfo_outf = "./satinfo"
#  dbname    = "$FVROOT/etc/gmao_satinfo.db"
#  nowarn = .true.
# /
#EOF
#make_satinfo.x < this.rc
gsiinfo.pl $EXPID $nymda $nhmsa
 
cd $inpobs
set lst = `ls *.yaml`
cd $MYWORK
foreach fn  ( $lst )
  set this = `echo $fn | cut -d. -f1`
  set use = (`grep $this satinfo | cut -c26-30`)
  set cld = (`grep $this satinfo | cut -c72-77`)
  if ( $status ) then
    /bin/cp $inpobs/$this.yaml $outobs/$this.yaml
  else
    /bin/rm -f sed_file
    echo "s/>>>use_channels_${this}<<</$use/1"     >> sed_file
    echo "s/>>>clddet_channels_${this}<<</$cld/1"  >> sed_file
    sed -f sed_file  $inpobs/$this.yaml  > $outobs/$this.yaml
  endif
end
