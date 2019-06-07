#!/bin/csh -f

if ( $#argv == 2 ) then

   set nymd_beg = $1
   set nymd_end = $2

else

   set nymd_beg = 20110301
   set nymd_end = 20110304

endif

setenv ARCH `uname -s`

if ( ! $?ESMADIR ) then
    echo $0": ESMADIR variable is not set"
    exit 1
endif

set path = ( . $ESMADIR/$ARCH/bin $path )
setenv PYTHONPATH $ESMADIR/$ARCH/lib/Python 

set nymd = $nymd_beg
while ( $nymd <= $nymd_end )
  @ yy = $nymd / 10000
  @ mm = ($nymd - ( $yy * 10000 ) ) / 100
  @ dd = $nymd - ( ( $yy * 10000 ) + ( $mm * 100 ) )
  if ( $mm < 10 ) set mm = 0$mm
  if ( $dd < 10 ) set dd = 0$dd
  foreach hh ( 00 03 06 09 12 15 18 21 )
       ./ana_aod.csh -f $yy $mm $dd $hh
  end
  set nymd = `tick $nymd`
end
