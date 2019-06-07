#!/bin/csh -f

if ( $#argv == 2 ) then

   set nymd_beg = $1
   set nymd_end = $2

else

   echo "Usage:"
   echo "        l2a_all nymd_beg nymd_end"
   exit 1

endif

setenv ARCH `uname -s`

if ( ! $?ESMADIR ) then
    echo $0": ESMADIR variable is not set"
    exit 1
endif

set path = ( . $ESMADIR/$ARCH/bin $path )
setenv PYTHONPATH /opt/epd/lib/python2.6/site-packages/:$ESMADIR/$ARCH/lib/Python 

set nymd = $nymd_beg
while ( $nymd <= $nymd_end )
  foreach hh ( 00 03 06 09 12 15 18 21 )
       ./aod_l2a.py aod_l2a-calculon.pcf $nymd ${hh}0000
  end
  set nymd = `tick $nymd`
end
