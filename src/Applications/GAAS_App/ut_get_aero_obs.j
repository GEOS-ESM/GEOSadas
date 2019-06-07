#!/bin/csh

# Job parameters
# --------------
setenv FVHOME /discover/nobackup/jstassi/C48f1
set nymd = 20170528
set nhms = 180000
setenv MODIS_L2_HDF 2    #  1 = use archived data, 2 = use staged data

# MODIS_L2_STAGE_DIR is needed if MODIS_L2_HDF equals 2
# -----------------------------------------------------
setenv MODIS_L2_STAGE_DIR /discover/nobackup/dao_ops/intermediate/flk/modis

# Set the environment
# -------------------
source $FVHOME/run/FVDAS_Run_Config
source $FVROOT/bin/g5_modules
set path = ( . $FVHOME/run $FVROOT/bin $path )

setenv NCPUS_AOD 8
setenv MPIRUN_AOD "mpiexec_mpt "

# FVWORK directory
# ----------------
setenv FVWORK /discover/nobackup/$user/FVWORK.$MODIS_L2_HDF
if (! -d $FVWORK ) mkdir -p $FVWORK
echo "FVWORK: $FVWORK"
cd $FVWORK

# Check for obsys-gaas.rc in FVWORK
# ---------------------------------
if (! -e $FVWORK/obsys-gaas.rc) then
    set rcfile = $FVHOME/run/obsys-gaas.rc
    echo "Copy to FVWORK: $rcfile"
    /bin/cp $rcfile .
endif

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

# Run the script
# --------------
run_gaas_ana.csh $EXPID $nymd $nhms 1 $FVWORK DEFAULT
