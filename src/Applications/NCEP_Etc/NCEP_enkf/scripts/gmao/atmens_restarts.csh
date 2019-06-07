#!/bin/csh

setenv MYNAME atmens_restarts.csh

if ( $#argv < 5 ) then
   echo " "
   echo " \\begin{verbatim} "
   echo " "
   echo " NAME "
   echo " "
   echo "  $MYNAME  - entry point to ensembole AOD analysis"
   echo " "
   echo " SYNOPSIS "
   echo " "
   echo "  $MYNAME  odir oexpid ndir nexpid nymdhh "
   echo " "
   echo " where"
   echo "   odir    -  original experiment location (e.g., archive location)"
   echo "   oexpid  -  original experiment name"
   echo "   ndir    -  new experiment location (i.e., nobackup area)"
   echo "   nexpid  -  new experiment name"
   echo "   nymdhh -  date of analysis, as in YYYYMMDDHH"
   echo " "
   echo " DESCRIPTION"
   echo " "
   echo "    This procedures copies all files needed to start an ensemble experiment from "
   echo "  an existing ensemble experiment."
   echo " "
   echo "  Example of valid command line:"
   echo "  $MYNAME /archive/u/rtodling e512a_rt $NOBACKUP e513T_rt 2014071709  "
   echo " "
   echo " REQUIRED ENVIRONMENT VARIABLES"
   echo " "
   echo " OPTIONAL ENVIRONMENT VARIABLES"
   echo " "
   echo " RESOURCE FILES"
   echo " "
   echo " SEE ALSO"
   echo " "
   echo " AUTHOR"
   echo "   Ricardo Todling (Ricardo.Todling@nasa.gov), NASA/GMAO "
   echo "     Last modified: 15Oct2014      by: R. Todling"
   echo " \\end{verbatim} "
   echo " \\clearpage "
   exit(0)
endif


setenv DRYRUN #echo

set oriarc = $1   # typically $ARCHIVE
set oriexp = $2   # OPS e.g., e5130_hyb_01
set dirnew = $3   # typically $NOBACKUP
set newexp = $4   # you know better
set datehh = $5   # YYYYMMDDHH

cd $dirnew/$newexp
if ( -e atmens ) then
   if ( -d atmens/ensmean || -d atmens/ensrms || -d atmens/mem001 ) then 
       echo "You should remove existing atmens directory from your new exp"
       exit(1)
   endif
endif
mkdir atmens
cd atmens
touch .no_archiving

set nymd = `echo $datehh | cut -c1-8`
set yyyy = `echo $datehh | cut -c1-4`
set mm   = `echo $datehh | cut -c5-6`
set hh   = `echo $datehh | cut -c9-10`

dmget $oriarc/$oriexp/atmens/Y$yyyy/M$mm/$oriexp.atmens_ebkg.${nymd}_${hh}z.tar \
      $oriarc/$oriexp/atmens/Y$yyyy/M$mm/$oriexp.atmens_ecbkg.${nymd}_${hh}z.tar \
      $oriarc/$oriexp/atmens/Y$yyyy/M$mm/$oriexp.atmens_erst.${nymd}_${hh}z.tar \
      $oriarc/$oriexp/atmens/Y$yyyy/M$mm/$oriexp.atmens_stat.${nymd}_${hh}z.tar

foreach typ ( ebkg ecbkg erst stat )
  $DRYRUN tar xvf $oriarc/$oriexp/atmens/Y$yyyy/M$mm/$oriexp.atmens_${typ}.${nymd}_${hh}z.tar &
end
wait

if ( "$DRYRUN" == "echo" ) then
   exit(0)
endif

# ebkg
/bin/mv $oriexp.atmens_ebkg.${nymd}_${hh}z/mem* .

# ecbkg; erst
foreach dir (`/bin/ls -d mem*`)
   echo "moving contents of $dir ..."
   /bin/mv $oriexp.atmens_ecbkg.${nymd}_${hh}z/$dir/$oriexp.* $dir/
   /bin/mv $oriexp.atmens_erst.${nymd}_${hh}z/$dir/$oriexp.* $dir/
end

# stat
/bin/mv $oriexp.atmens_stat.${nymd}_${hh}z/ens* .

# clean up
foreach typ ( ebkg ecbkg erst stat )
   /bin/rm -r $oriexp.atmens_${typ}.${nymd}_${hh}z
end

# rename files
foreach dir (`/bin/ls -d ensmean ensrms mem*`)
    cd $dir
    foreach fn ( `/bin/ls $oriexp.*bin $oriexp.*hdf $oriexp.*nc4 $oriexp.*txt`)
       set sfx = `echo $fn | cut -d. -f2-`
       /bin/mv $fn $expidn.$sfx
    end
    cd -
end

echo "All done!"
exit(0)
