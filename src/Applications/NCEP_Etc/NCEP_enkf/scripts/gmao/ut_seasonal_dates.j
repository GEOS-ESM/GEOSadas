#!/bin/csh

setenv  FVROOT /discover/swdev/rtodling/Latest/E5/GEOSadas/Linux
source $FVROOT/bin/g5_modules
set path = ( . $FVROOT/bin $path )

setenv ATMENSETC etc

set nymd = 20070429
set nhms = 00
set nmem = 32

@ syn_sc = 6 * 3600
@ ntot = 1 #365 * 4
@ n=0
while ( $n < $ntot )
   @ n++
   atmens_seasonal_date.csh  $nymd $nhms $nmem
   set newdate = `tick $nymd $nhms $syn_sc`
   set nymd = $newdate[1]
   set nhms = $newdate[2]
end
