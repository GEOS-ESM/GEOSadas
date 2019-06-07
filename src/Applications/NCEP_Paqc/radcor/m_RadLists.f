!=======================================================================

!-----------------------------------------------------------------------
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !MODULE: m_RadLists -- Defines commonly accessed list.
!
! !DESCRIPTION:
!
! !INTERFACE:
!
      module m_RadLists

      implicit    NONE
      private   ! except

      public
     .   kt_names,           ! List of GMAO data type indices (kt)
     .   kt_units,           ! ... and the associated units
     .   kx_names,           ! ... GMAO data source indices (kx)
     .   rkx_names,          ! ... WMO instrument type (rkx)
     .   rcode_names,        ! ... WMO radiation bias correction codes
     .   kx_RaobList,        ! ... kx's for radiosondes
     .   MetaStr_Raobs       ! String for radiosonde kx's in kx_meta
! 
! !REVISION HISTORY:
!     20Nov2006  C. Redder  Original code (from module, m_RadData)
!EOP
!-----------------------------------------------------------------

      character (len=*), parameter :: MyModule = 'm_RadLists'

!     List of GMAO data type indices (kt)
!     -----------------------------------
      integer, parameter ::
     .   nkt    = 41,
     .   kt_len = 38
      character (len = kt_len), dimension (nkt), parameter ::
     .   kt_blanks = ' '
      character (len = kt_len), dimension (nkt), parameter ::
     .   kt_names  = (/
     .  'Surface (10m) zonal wind              ',     !  1
     .  'Surface (10m) meridional wind         ',     !  2
     .  'Sea level pressure                    ',     !  3
     .  'Upper-air zonal wind                  ',     !  4
     .  'Upper-air meridional wind             ',     !  5
     .  'Upper-air geopotential height         ',     !  6
     .  'Upper-air water vapor mixing ratio    ',     !  7
     .  'Upper-air temperature                 ',     !  8
     .  'Upper-air dew-point temperature       ',     !  9
     .  'Upper-air relative humidity           ',     ! 10
     .  'Upper-air specific humidity           ',     ! 11
     .  'Surface (10m) wind speed              ',     ! 12
     .  'Surface (10m) temperature             ',     ! 13
     .  'Surface (10m) dew-point temperature   ',     ! 14
     .  'Surface (10m) relative humidity       ',     ! 15
     .  'Surface (10m) specific humidity       ',     ! 16
     .  'Precipitation rate                    ',     ! 17
     .  'Total precipitable water              ',     ! 18
     .  'Total cloud liquid water              ',     ! 19
     .  'Fractional cloud cover                ',     ! 20
     .  'Total column ozone                    ',     ! 21
     .  'Ozone                                 ',     ! 22
     .  'Upper-air thickness                   ',     ! 23
     .   kt_blanks ( 24 : 39 ),                       ! 24 - 39
     .  'Brightness Temperature                ',     ! 40
     .  'Mean layer virtual temperature        ' /)   ! 41

!     ... and the associated units
!     ----------------------------
      character (len = kt_len), dimension (nkt), parameter ::
     .   kt_units = (/
     .  'm/sec                                 ',     !  1
     .  'm/sec                                 ',     !  2
     .  'hPa                                   ',     !  3
     .  'm/sec                                 ',     !  4
     .  'm/sec                                 ',     !  5
     .  'm                                     ',     !  6
     .  'g/kg                                  ',     !  7
     .  'Kelvin                                ',     !  8
     .  'Kelvin                                ',     !  9
     .  '%                                     ',     ! 10
     .  'g/kg                                  ',     ! 11
     .  'm/sec                                 ',     ! 12
     .  'Kelvin                                ',     ! 13
     .  'Kelvin                                ',     ! 14
     .  '%                                     ',     ! 15
     .  'g/kg                                  ',     ! 16
     .  'mm/day                                ',     ! 17
     .  'mm                                    ',     ! 18
     .  'mm                                    ',     ! 19
     .  '%                                     ',     ! 20
     .  'Dobson                                ',     ! 21
     .  'Dobson                                ',     ! 22
     .  'm                                     ',     ! 23
     .   kt_blanks ( 24 : 39 ),                       ! 24 -39
     .  'Kelvin                                ',     ! 40
     .  'Kelvin                                ' /)   ! 41

!     List of GMAO data source indices (kx)
!     -------------------------------------
      integer, parameter ::
     .   nkx           = 296,
     .   kx_len        =  38,
     .   kx_RaobListSz =   7
      character (len   = *), parameter ::
     .   MetaStr_Raobs = 'class: radiosondes'
      integer, dimension ( kx_RaobListSz ),      parameter ::
     .   kx_RaobList   = (/ 7, 8, 9, 10, 11, 12, 13 /)
      character (len = kx_len), dimension (nkx), parameter ::
     .   kx_blanks = ' '
      character (len = kx_len), dimension (nkx), parameter ::
     .   kx_names  = (/
     .  'Surface Land Obs - 1                  ',     !  1
     .  'Surface Land Obs - 2                  ',     !  2
     .  'Surface Ship Obs - 1                  ',     !  3
     .  'Surface Ship Obs - 2                  ',     !  4
     .  'Environment Buoy                      ',     !  5
     .  'Drifting Buoy                         ',     !  6
     .  'Rawinsonde                            ',     !  7
     .  'Pilot Wind                            ',     !  8
     .  'Ship Released Rawinsonde              ',     !  9
     .  'Dropwinsonde                          ',     ! 10
     .  'Radar-tracked Rawinsonde              ',     ! 11
     .  'Rocketsonde                           ',     ! 12
     .  'Balloon                               ',     ! 13
     .  'Aircraft - Air/Sat Relay              ',     ! 14
     .  'Aircraft - Int. Data Sys              ',     ! 15
     .  'Aircraft Report                       ',     ! 16
     .  'Aircraft Coded Report                 ',     ! 17
     .  'Aircraft - ALPX                       ',     ! 18
     .   kx_blanks ( 19 : nkx ) /)

!     List of WMO instrument type (rkx)
!     ---------------------------------
      integer :: iType
      integer, parameter ::
     .   nrkx    = 256,
     .   rkx_len =  57
      character (len=*), parameter, dimension (nrkx) ::
     .   rkx_names = (/

     . ! Description                                                 ! Code
     . ! -----------                                                 ! ----
     .  'Reserved                                                 ', ! 1
     .  'No radiosonde - passive target (e.g. reflector)          ', ! 2
     .  'No radiosonde - active target (e.g. transponder)         ', ! 3
     .  'No radiosonde - passive temperature-humidity profiler    ', ! 4
     .  'No radiosonde - active temperature-humidity profiler     ', ! 5
     .  'No radiosonde - active radio-acoustic sounder            ', ! 6
     .  'No radiosonde - ...(reserved)                            ', ! 7
     .  'No radiosonde - ...(reserved)                            ', ! 8
     .  'No radiosonde - system unknown or not specified          ', ! 9
     .  'VIZ type A pressure-commutated (USA)                     ', ! 10
     .  'VIZ type B time-commutated (USA)                         ', ! 11
     .  'RS SDC (Space Data Corporation - USA)                    ', ! 12
     .  'Astor (no longer made - Australia)                       ', ! 13
     .  'VIZ Mark I MICROSONDE (USA)                              ', ! 14
     .  'EEC Company type 23 (USA)                                ', ! 15
     .  'Elin (Austria)                                           ', ! 16
     .  'Graw G. (Germany)                                        ', ! 17
     .  'Reserved for allocation of radiosonde                    ', ! 18
     .  'Graw M60 (Germany)                                       ', ! 19
     .  'Indian Meteorological Service MK3 (India)                ', ! 20
     .  'VIZ/Jin Yang Mark I MICROSONDE (South Korea)             ', ! 21
     .  'Meisei RS2-80 (Japan)                                    ', ! 22
     .  'Mesural FMO 1950A (France)                               ', ! 23
     .  'Mesural FMO 1945A (France)                               ', ! 24
     .  'Mesural MH73A (France)                                   ', ! 25
     .  'Meteolabor Basora (Switzerland)                          ', ! 26
     .  'AVK-MRZ (Russian Federation)                             ', ! 27
     .  'Meteorit Marz2-1 (Russian Federation)                    ', ! 28
     .  'Meteorit Marz2-2 (Russian Federation)                    ', ! 29
     .  'Oki RS2-80 (Japan)                                       ', ! 30
     .  'VIZ/Valcom type A pressure-commutated (Canada)           ', ! 31
     .  'Shanghai Radio (China)                                   ', ! 32
     .  'UK Met Office MK3 (UK)                                   ', ! 33
     .  'Vinohrady (Czechoslovakia)                               ', ! 34
     .  'Vaisala RS18 (Finland)                                   ', ! 35
     .  'Vaisala RS21 (Finland)                                   ', ! 36
     .  'Vaisala RS80 (Finland)                                   ', ! 37
     .  'VIZ LOCATE Loran-C (USA)                                 ', ! 38
     .  'Sprenger E076 (Germany)                                  ', ! 39
     .  'Sprenger E084 (Germany)                                  ', ! 40
     .  'Sprenger E085 (Germany)                                  ', ! 41
     .  'Sprenger E086 (Germany)                                  ', ! 42
     .  'AIR IS - 4A - 1680 (USA)                                 ', ! 43
     .  'AIR IS - 4A - 1680 X (USA)                               ', ! 44
     .  'RS MSS (USA)                                             ', ! 45
     .  'Air IS - 4A - 403 (USA)                                  ', ! 46
     .  'Meisei RS2-91 (Japan)                                    ', ! 47
     .  'VALCOM (Canada)                                          ', ! 48
     .  'VIZ MARK II (USA)                                        ', ! 49
     .  'GRAW DFM-90 (Germany)                                    ', ! 50
     .  'VIZ-B2 (USA)                                             ', ! 51
     .  'Vaisala RS80-57H                                         ', ! 52
     .  'AVK-RF93 (Russian Federation)                            ', ! 53
     . ('Reserved for allocation of radiosondes                   ', ! 54
     .   iType = 1, 6),                                              !-59
     .  'Vaisala RS80/MicroCora (Finland)                         ', ! 60
     .  'Vaisala RS80/DifiCora or Marwin (Finland)                ', ! 61
     .  'Vaisala RS80/PCCora (Finland)                            ', ! 62
     .  'Vaisala RS80/Star (Finland)                              ', ! 63
     .  'Orbital Sciences Corporation, Space Data Division        ', ! 64
     .  'VIZ transponder radiosonde, model number 1499-520 (USA)  ', ! 65
     . ('Reserved for additional automated sounding systems       ', ! 66-
     .   iType = 1, 5),                                              ! 70
     .  'RS90/MicroCora (Finland)                                 ', ! 71
     .  'RS90/DigiCora or Marwin (Finland)                        ', ! 72
     .  'RS90/PCCora (Finland)                                    ', ! 73
     .  'RS90/Star (Finland)                                      ', ! 74
     .  'AVK-MRZ-ARMA (Russian Federation)                        ', ! 75
     .  'AVK-RF95-ARMA (Russian Federation)                       ', ! 76
     .  'GEOLINK GPSonde GL98 (France)                            ', ! 77
     . ('Reserved for additional automated sounding systems       ', ! 78-
     .   iType = 1, 12),                                             ! 89
     .  'Radiosonde not specified or unknownd                     ', ! 90
     .  'Pressure-only radiosonde                                 ', ! 91
     .  'Pressure-only radiosonde plus transponder                ', ! 92
     .  'Pressure-only radiosonde plus radar reflector            ', ! 93
     .  'No-pressure-only radiosonde plus transponder             ', ! 94
     .  'No-pressure-only radiosonde plus radar reflector         ', ! 95
     .  'Descending radiosonde                                    ', ! 96
     . ('Reserved for sounding systems with incomplete sondes     ', ! 97-
     .   iType = 1, 3),                                              ! 99
     . ('Reserved                                                 ', ! 100-
     .   iType = 1, 155),                                            ! 254
     .  'Missing value                                            ', ! 255
     .  'Reserved (Same as WMO code 0)                            '/)! 256

!     List of WMO radiation bias correction codes
!     -------------------------------------------
      integer, parameter ::
     .   nrcodes    =  16,
     .    rcode_len =  57
      character (len = kx_len), dimension (nrcodes), parameter ::
     .   rcode_names = (/
     . ! Description                                                 ! Code
     . ! -----------                                                 ! ----
     .  'CIMO solar corrected and CIMO infrared corrected         ', ! 1
     .  'CIMO solar corrected and infrared corrected              ', ! 2
     .  'CIMO solar corrected only                                ', ! 3
     .  'Solar and infrared corrected automatically by rsonde sys ', ! 4
     .  'Solar corrected automatically by radiosonde system       ', ! 5
     .  'Solar and Infrared corrected as specified by country     ', ! 6
     .  'Solar corrected as specified by country                  ', ! 7
     . ('Reserved                                                 ', ! 8
     .   iType = 1, 7),                                              !-14
     .  'Missing value                                            ', ! 15
     .  'No correction (Same as WMO code 0)                       '/)! 16

      end module m_RadLists
!====================================================================
