#!/bin/csh -x

# Job parameters
# --------------
setenv EXPID C48f6
setenv FVHOME $NOBACKUP/$EXPID

set nymd = 20180706
set nhms = 000000

setenv GID g0620
setenv group_list "SBATCH -A $GID"
setenv FVROOT `cat $FVHOME/.FVROOT`

# MODIS data; acquire or use staged data?
# ---------------------------------------
setenv USE_MODIS_STAGE 1
setenv MODIS_STAGE_DIR /discover/nobackup/dao_ops/intermediate/flk/modis

setenv aerosol_acquire 1
if ($USE_MODIS_STAGE) setenv aerosol_acquire 0

# Set the environment
# -------------------
source $FVHOME/run/FVDAS_Run_Config
set path = ( . $FVHOME/run $FVROOT/bin $path )

setenv NCPUS_AOD 8
setenv MPIRUN_AOD "mpiexec_mpt "
setenv SKIP_PSAS 1

# FVWORK directory
# ----------------
setenv FVWORK $NOBACKUP/AODWORK.$$
if (! -d $FVWORK ) mkdir -p $FVWORK
echo "FVWORK: $FVWORK"
cd $FVWORK

# Check for obsys-gaas.rc in FVWORK
# ---------------------------------
if (! -e $FVWORK/obsys-gaas.rc) then
    set rcfile = $FVHOME/run/obsys-gaas.rc
    echo "Copy to FVWORK: $rcfile"
    /bin/cp $rcfile $FVWORK
endif
setenv rcflag "-rc obsys-gaas.rc"

# Check for aerosol bkgs in FVWORK
# --------------------------------
foreach aod_time_offset ( 000000 030000 )
   set gaastime = (`tick $nymd $nhms 0 $aod_time_offset`)
   set ymd = $gaastime[1]
   set hms = $gaastime[2]

   set flg1 = "-template $EXPID $ymd $hms"
   set flg2 = "-rc $FVHOME/run/gaas/gaas.rc gaas_bkg_filename"
   set aer_f = `echorc.x $flg1 $flg2`

   # If not in FVWORK, then look in archive
   # --------------------------------------
   if (! -e $FVWORK/$aer_f) then
       echo "Cannot find aerosol bkg file in FVWORK: $aer_f"
       set yyyy = `echo $ymd | cut -c1-4`
       set mm   = `echo $ymd | cut -c5-6`
       set aer_f = $FVARCH/$EXPID/ana/Y$yyyy/M$mm/$aer_f

       if (! -e $aer_f) then
           echo "Cannot find aerosol bkg file in archive: $aer_f"
           echo "Exiting"
           exit 99
       endif
       echo "Copy to FVWORK: $aer_f"
       /bin/cp $aer_f $FVWORK
   endif
end
set echo

# Check for external data directory
# ---------------------------------
if (! $?EXTDATA) setenv EXTDATA $FVWORK/ExtData
if (! -d $EXTDATA) then
    /bin/mkdir -p $EXTDATA
    /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/agcmpert $EXTDATA/
    /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/g5chem $EXTDATA/
    /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/g5gcm $EXTDATA/
    /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/PIESA $EXTDATA/
    /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/MERRA2 $EXTDATA/
    /bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput_4dvar/AeroCom $EXTDATA/
    /bin/touch $EXTDATA/.no_archiving
endif

# Get aerosol observations
# ------------------------
@ delta_hrs = 3 * 60 * 60
set date1 = ( $nymd $nhms )
set date2 = `tick $date1 delta_hrs`

set determine_aod_obsclass = 0
if (! $?AOD_OBSCLASS) set determine_aod_obsclass = 1
if ($determine_aod_obsclass) then
   setenv AOD_OBSCLASS `obsclass_filter.pl all $date1 4 $rcflag`
   setenv AOD_OBSCLASS `aod_data.py aod_filter $AOD_OBSCLASS`
endif

get_aero_obs.csh  $date1 1
get_aero_obs.csh  $date2 1

# Run the script
# --------------
run_gaas_ana.csh $EXPID $nymd $nhms 1 $FVWORK DEFAULT
