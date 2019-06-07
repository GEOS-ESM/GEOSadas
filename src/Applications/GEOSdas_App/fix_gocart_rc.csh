#!/bin/csh

set newlev = $1
set gocdir = $2

cd $gocdir

if (-e sed_file) rm -f sed_file
echo "s/z72/z$newlev/1"       > sed_file
echo "s/L72/L$newlev/1"      >> sed_file
echo "s/_72_/_${newlev}_/1"  >> sed_file

foreach fn ( `ls` )
  if ( ! -d $fn ) then
     /bin/mv $fn $fn.hold
     sed -f sed_file  $fn.hold  > $fn
     /bin/rm $fn.hold
  endif
end
/bin/rm sed_file

touch .fix_gocart_L${newlev}
cd -
exit (0)
