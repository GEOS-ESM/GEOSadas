#!/bin/csh

setenv FVROOT /home/dao_ops/GEOSadas-5_43_0/GEOSadas/install-SLES15
set path = ( . $FVROOT/bin $path )

setenv DRYRUN #echo
setenv OUTFIGS $TMP/SwellExperiments/Figs

set humKX = (`echorc.x -rc ostats.rc -ncol 2 specific_humidity`)
set humNM = (`echorc.x -rc ostats.rc -ncol 1 specific_humidity`)

set acTKX = (`echorc.x -rc ostats.rc -ncol 2 aircraftT`)
set acTNM = (`echorc.x -rc ostats.rc -ncol 1 aircraftT`)

set radKX = (`echorc.x -rc ostats.rc -ncol 2 radiance`)
set radNM = (`echorc.x -rc ostats.rc -ncol 1 radiance`)

set gpsKX = (`echorc.x -rc ostats.rc -ncol 2 gnssro`)
set gpsNM = (`echorc.x -rc ostats.rc -ncol 1 gnssro`)

set ozKX = (`echorc.x -rc ostats.rc -ncol 2 ozone`)
set ozNM = (`echorc.x -rc ostats.rc -ncol 1 ozone`)

set stwKX = (`echorc.x -rc ostats.rc -ncol 2 satwind`)
set stwNM = (`echorc.x -rc ostats.rc -ncol 1 satwind`)

set humKX = ()
set acTKX = ()
set radKX = ()
set gpsKX = ()
#set ozKX  = ()
set stwKX = ()

set expidGSI  = x0054
set expidGSI  = x0053RPY
set expidJEDI = j4drpy

#set nymd = 20260113
set nymd = 20260120
set nhms = 000000

set ODSarch  = $DAD/archive/544
set ODSarch  = $DAD/archive/543
set IODAarch = $DAD/archive/JEDI/543

# GEOS-GSI experiment output
if ( $expidGSI != "null" ) then

  @ ic = 1
  foreach kx ( $stwKX )
    set yyyy = `echo $nymd | cut -c1-4`
    set mm   = `echo $nymd | cut -c5-6`
    set dd   = `echo $nymd | cut -c7-8`
    set hh   = `echo $nhms | cut -c1-2`
    set instr = $stwNM[$ic]
    
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --obtype sondes_u --satid $kx \
        --fig $OUTFIGS/$expidGSI.${instr}.${nymd}_${hh}z.png \
        $ODSarch/$expidGSI/obs/Y$yyyy/M$mm/D$dd/H$hh/$expidGSI.diag_conv.${nymd}_${hh}z.ods &
    
    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $humKX )
    set yyyy = `echo $nymd | cut -c1-4`
    set mm   = `echo $nymd | cut -c5-6`
    set dd   = `echo $nymd | cut -c7-8`
    set hh   = `echo $nhms | cut -c1-2`
    set instr = $humNM[$ic]
    
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --obtype sondes_q --satid $kx \
        --fig $OUTFIGS/$expidGSI.${instr}.${nymd}_${hh}z.png \
        $ODSarch/$expidGSI/obs/Y$yyyy/M$mm/D$dd/H$hh/$expidGSI.diag_conv.${nymd}_${hh}z.ods &
    
    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $acTKX )
    set yyyy = `echo $nymd | cut -c1-4`
    set mm   = `echo $nymd | cut -c5-6`
    set dd   = `echo $nymd | cut -c7-8`
    set hh   = `echo $nhms | cut -c1-2`
    set instr = $acTNM[$ic]
    
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --obtype temperature --satid $kx \
        --fig $OUTFIGS/$expidGSI.aircraftT_${instr}.${nymd}_${hh}z.png \
        $ODSarch/$expidGSI/obs/Y$yyyy/M$mm/D$dd/H$hh/$expidGSI.diag_conv.${nymd}_${hh}z.ods &
    
    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $radKX )
    set yyyy = `echo $nymd | cut -c1-4`
    set mm   = `echo $nymd | cut -c5-6`
    set dd   = `echo $nymd | cut -c7-8`
    set hh   = `echo $nhms | cut -c1-2`
    set instr = $radNM[$ic]
    
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --obtype radiance --satid $kx \
        --fig $OUTFIGS/$expidGSI.${instr}.${nymd}_${hh}z.png \
        $ODSarch/$expidGSI/obs/Y$yyyy/M$mm/D$dd/H$hh/$expidGSI.diag_${instr}.${nymd}_${hh}z.ods &
    
    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $ozKX )
    set yyyy = `echo $nymd | cut -c1-4`
    set mm   = `echo $nymd | cut -c5-6`
    set dd   = `echo $nymd | cut -c7-8`
    set hh   = `echo $nhms | cut -c1-2`
    set instr = $ozNM[$ic]
  
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --var ozoneProfile --satid $kx \
        --fig $OUTFIGS/$expidGSI.${instr}.${nymd}_${hh}z.png \
        $ODSarch/$expidGSI/obs/Y$yyyy/M$mm/D$dd/H$hh/$expidGSI.diag_${instr}.${nymd}_${hh}z.ods &

    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $gpsKX )
    set yyyy = `echo $nymd | cut -c1-4`
    set mm   = `echo $nymd | cut -c5-6`
    set dd   = `echo $nymd | cut -c7-8`
    set hh   = `echo $nhms | cut -c1-2`
    set instr = $gpsNM[$ic]
  
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --var bendingAngle --satid $kx \
        --scale obs --fig $OUTFIGS/$expidGSI.${instr}.${nymd}_${hh}z.png \
        $ODSarch/$expidGSI/obs/Y$yyyy/M$mm/D$dd/H$hh/$expidGSI.diag_conv.${nymd}_${hh}z.ods &

    @ ic++
  end
  wait

endif # GEOS-GSI

# GEOS-JEDI experiment output

if ( $expidJEDI != "null" ) then

  set jedi_date = (`tick $nymd $nhms -10800`)
  set jnymd = $jedi_date[1]
  set jnhms = $jedi_date[2]

  @ ic = 1
  foreach kx ( $stwKX )
    set jyyyy = `echo $jnymd | cut -c1-4`
    set jmm   = `echo $jnymd | cut -c5-6`
    set jdd   = `echo $jnymd | cut -c7-8`
    set jhh   = `echo $jnhms | cut -c1-2`
    set  hh   = `echo $nhms  | cut -c1-2`
    set instr = $stwNM[$ic]
  
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --obtype sondes_u --satid $kx \
           --fig $OUTFIGS/$expidJEDI.${instr}.${nymd}_${hh}z.png \
           --tarname $IODAarch/$expidJEDI/jedi/obs/Y$jyyyy/M$jmm/$expidJEDI.jedi_hofx.${jnymd}_${jhh}z.tar \
           satwind.${jnymd}T${jhh}0000Z.nc4 &

    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $humKX )
    set jyyyy = `echo $jnymd | cut -c1-4`
    set jmm   = `echo $jnymd | cut -c5-6`
    set jdd   = `echo $jnymd | cut -c7-8`
    set jhh   = `echo $jnhms | cut -c1-2`
    set  hh   = `echo $nhms  | cut -c1-2`
    set instr = $humNM[$ic]
  
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --obtype sondes_q --satid $kx \
           --fig $OUTFIGS/$expidJEDI.${instr}.${nymd}_${hh}z.png \
           --tarname $IODAarch/$expidJEDI/jedi/obs/Y$jyyyy/M$jmm/$expidJEDI.jedi_hofx.${jnymd}_${jhh}z.tar \
           sondes.${jnymd}T${jhh}0000Z.nc4 &

    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $acTKX )
    set jyyyy = `echo $jnymd | cut -c1-4`
    set jmm   = `echo $jnymd | cut -c5-6`
    set jdd   = `echo $jnymd | cut -c7-8`
    set jhh   = `echo $jnhms | cut -c1-2`
    set  hh   = `echo $nhms  | cut -c1-2`
    set instr = $acTNM[$ic]
  
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --obtype temperature --satid $kx \
           --fig $OUTFIGS/$expidJEDI.aircraftT_${instr}.${nymd}_${hh}z.png \
           --tarname $IODAarch/$expidJEDI/jedi/obs/Y$jyyyy/M$jmm/$expidJEDI.jedi_hofx.${jnymd}_${jhh}z.tar \
           aircraft_temperature.${jnymd}T${jhh}0000Z.nc4 &

    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $radKX )
    set jyyyy = `echo $jnymd | cut -c1-4`
    set jmm   = `echo $jnymd | cut -c5-6`
    set jdd   = `echo $jnymd | cut -c7-8`
    set jhh   = `echo $jnhms | cut -c1-2`
    set  hh   = `echo $nhms  | cut -c1-2`
    set instr = $radNM[$ic]
  
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --obtype radiance --satid $kx \
           --fig $OUTFIGS/$expidJEDI.${instr}.${nymd}_${hh}z.png \
           --tarname $IODAarch/$expidJEDI/jedi/obs/Y$jyyyy/M$jmm/$expidJEDI.jedi_hofx.${jnymd}_${jhh}z.tar \
           ${instr}.${jnymd}T${jhh}0000Z.nc4 &

    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $ozKX )
    set jyyyy = `echo $jnymd | cut -c1-4`
    set jmm   = `echo $jnymd | cut -c5-6`
    set jdd   = `echo $jnymd | cut -c7-8`
    set jhh   = `echo $jnhms | cut -c1-2`
    set  hh   = `echo $nhms  | cut -c1-2`
    set instr = $ozNM[$ic]
  
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --var ozoneProfile \
           --fig $OUTFIGS/$expidJEDI.${instr}.${nymd}_${hh}z.png \
           --tarname $IODAarch/$expidJEDI/jedi/obs/Y$jyyyy/M$jmm/$expidJEDI.jedi_hofx.${jnymd}_${jhh}z.tar \
           ${instr}.${jnymd}T${jhh}0000Z.nc4 &
  
    @ ic++
  end
  wait

  @ ic = 1
  foreach kx ( $gpsKX )
    set jyyyy = `echo $jnymd | cut -c1-4`
    set jmm   = `echo $jnymd | cut -c5-6`
    set jdd   = `echo $jnymd | cut -c7-8`
    set jhh   = `echo $jnhms | cut -c1-2`
    set  hh   = `echo $nhms  | cut -c1-2`
    set instr = $gpsNM[$ic]
  
    $DRYRUN ~/src/python/JEDI/OBS/ioda_prs.binned.py --var bendingAngle --satid $kx \
           --scale obs \
           --fig $OUTFIGS/$expidJEDI.${instr}.${nymd}_${hh}z.png \
           --tarname $IODAarch/$expidJEDI/jedi/obs/Y$jyyyy/M$jmm/$expidJEDI.jedi_hofx.${jnymd}_${jhh}z.tar \
           gps.${jnymd}T${jhh}0000Z.nc4 &

    @ ic++
  end
  wait

endif # GEOS-JEDI
