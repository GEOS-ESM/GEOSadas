#!/bin/csh

setenv FVROOT /discover/swdev/rtodling/g580/GEOSadas/Linux

set expout  = e572_fp
#set workdir = /archive/u/rtodling/NMCperts
set workdir = /discover/nobackup/rtodling/NMCperts
set nymd    = 20121211  # date to generate pert for
set nhms    = 000000    # time to generate pert for
set ndays   = 1         # number of perts to create from given date/time

# NEED TO DO:
# ----------
# 20120410
# 20120411

# from Y2011/M12/??? to Y2012/M04/D07
#set fcstdir = /archive/dao_ops/GEOS-5.7.2/GEOSadas-5_7_2_p2_m1
#set expid   = e572p2_fp

# from Y2012/M04/D08 to Y2012/M05/D09
#set fcstdir = /archive/u/dao_ops/GEOS-5.7.2/GEOSadas-5_7_2_p5
#set expid   = e572p5_fp   # expid of exp holding fcsts
#setenv OFFSET_HR 3

# from Y2012/M05/D11 to present
set fcstdir = /archive/u/dao_ops/GEOS-5.7.2/GEOSadas-5_7_2_p5_m1
#set fcstdir = /archive/u/rtodling  # boundary dates
set expid   = e572p5_fp   # expid of exp holding fcsts
setenv OFFSET_HR 3

$FVROOT/bin/gen_nmcperts.csh  $fcstdir $workdir $expid $expout $nymd $nhms $ndays
