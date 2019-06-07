#!/bin/csh -f
# Reblock.csh
# shell script to enable parallel processing in the FixEndian part
# of GEOSdas.csm
# Usage Reblock.csh file
#
# 20080404 TOwens initial version adapted from GEOSdas.csm
#

set ublk = $1
set ublk_tmp = "ublk.tmp.$$"
 /bin/mv $ublk $ublk_tmp
 reblock $ublk_tmp $ublk
 if ( $status ) then
     /bin/mv $ublk_tmp $ublk # move it back to original name, but file likely no good
     touch ${ublk}.FAILED_BLOCK
     exit 1
 endif
 /bin/rm $ublk_tmp
exit 0
