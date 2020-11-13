#!/bin/csh
#SBATCH --account=g0613
#SBATCH --constraint=hasw
#SBATCH --ntasks=28
#SBATCH --time=12:00:00
#SBATCH --qos=dastest
#SBATCH --partition=preops
#SBATCH --output=ut_nmcperts.log.o%j

setenv FVROOT /discover/nobackup/projects/gmao/advda/rtodling/4OPS/g526/GEOSadas/Linux
source $FVROOT/bin/g5_modules

setenv GEN_NMCPERTS_NCPUS 8
setenv GEN_NMCPERTS_LOCAL 1
setenv OPTRH 2
setenv DRYRUN #echo

set expid   = f522_fp
set expid   = f521_fp

set expout  = $expid
if ($OPTRH == 1) then
   set workdir = /discover/nobackup/projects/gmao/obsdev/rtodling/fcst4berrcov.f522_fp.20190304.000000/NMCperts_geosrh
endif
if ($OPTRH == 2) then
   set workdir = /discover/nobackup/projects/gmao/obsdev/rtodling/fcst4berrcov.f522_fp.20190304.000000/NMCperts_bec
   set workdir = /discover/nobackup/projects/gmao/obsdev/rtodling/fcst4berrcov.f522_fp.20190304.000000/NMCperts_bec_test
endif
if ($OPTRH == 3) then
   set workdir = /discover/nobackup/projects/gmao/obsdev/rtodling/fcst4berrcov.f522_fp.20190304.000000/NMCperts_gsi
endif
if ( $expid == "f521_fp" ) then
   set nymd    = 20181101  # date to generate pert for
   set nhms    = 000000    # time to generate pert for
   set ndays   = 122       # number of perts to create from given date/time
endif

if ( $expid == "f522_fp" ) then
  set expout  = $expid
  set nymd    = 20190303  # date to generate pert for
  set nhms    = 000000    # time to generate pert for
  set ndays   = 370       # number of perts to create from given date/time
endif
if ( $expid == "f525_fp" ) then
  set expout  = $expid
  set nymd    = 20200307  # date to generate pert for
  set nhms    = 000000    # time to generate pert for
  set ndays   = 33        # number of perts to create from given date/time
endif

if ( ! -d $workdir ) mkdir -p $workdir

# from Y2012/M05/D11 to present
#set fcstdir = /archive/u/rtodling  # boundary dates
set fcstdir = /discover/nobackup/projects/gmao/obsdev/rtodling/fcst4berrcov.f522_fp.20190304.000000/ACQedFiles/
setenv OFFSET_HR 3

$FVROOT/bin/gen_nmcperts.csh  $fcstdir $workdir $expid $expout $nymd $nhms $ndays
