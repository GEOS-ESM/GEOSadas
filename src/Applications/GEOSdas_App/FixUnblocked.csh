#!/bin/csh -f
# FixUnblocked.csh
# Script to allow parallel execution of the FixUnblocked routine
# in the PreAnalysisQC_() subroutine from GEOSdas.csm
#
# Usage: FixUnblocked.csh unblocked_file
# 20080404 TOwens initial version adapted from GEOSdas.csm

 set ublk = $1
 set ublk_tmp = "ublk.tmp.$$"
 /bin/mv $ublk $ublk_tmp
 block $ublk_tmp $ublk
 if ( $status ) then
      reblock $ublk_tmp $ublk
      if ( $status ) then
           /bin/mv $ublk_tmp $ublk # move it back to orignal name, but file likely no good
           touch ${ublk}.FAILED_BLOCK
      endif
 endif
 /bin/rm $ublk_tmp
