#!/bin/csh

setenv  FVROOT /discover/nobackup/projects/gmao/advda/rtodling/4OPS/g526/GEOSadas/Linux
source $FVROOT/bin/g5_modules
set path = ( . $FVROOT/bin $path )

setenv ATMENSETC etc

set nymd = 20171202
set nymd = 20200229
set nhms = 00
set nmem = 32

setenv VERBOSE 1
@ syn_sc = 6 * 3600
@ ntot = 1 #365 * 4
@ n=0
while ( $n < $ntot )
   @ n++
   atmens_seasonal_date.csh  $nymd $nhms $nmem
   if ($status) then
       setenv VERBOSE 1
       atmens_seasonal_date.csh  $nymd $nhms $nmem
       exit(1)
   endif
   set newdate = `tick $nymd $nhms $syn_sc`
   set nymd = $newdate[1]
   set nhms = $newdate[2]
end
